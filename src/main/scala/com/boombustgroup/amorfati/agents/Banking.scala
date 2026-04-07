package com.boombustgroup.amorfati.agents

import com.boombustgroup.ledger.Distribute
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.mechanisms.{Macroprudential, YieldCurve}
import com.boombustgroup.amorfati.types.*

import scala.util.Random

object Banking:

  // ---------------------------------------------------------------------------
  // Named constants (Basel III / KNF calibration)
  // ---------------------------------------------------------------------------

  /** Corp bond risk weight (Basel III, BBB bucket). */
  private val CorpBondRiskWeight: Share = Share(0.50)

  /** Well-capitalised floor for CAR/LCR/NSFR when denominator ≤ threshold. */
  private val SafeRatioFloor: Multiplier = Multiplier(10.0)

  /** Minimum balance threshold to avoid division by zero. */
  private val MinBalanceThreshold: PLN = PLN(1.0)

  // NSFR weights (Basel III §6)
  private val AsfTermWeight: Share   = Share(0.95)
  private val AsfDemandWeight: Share = Share(0.90)
  private val RsfShort: Share        = Share(0.50)
  private val RsfMedium: Share       = Share(0.65)
  private val RsfLong: Share         = Share(0.85)
  private val RsfGovBond: Share      = Share(0.05)
  private val RsfCorpBond: Share     = Share(0.50)

  // Lending rate components
  private val FailedBankSpread: Rate           = Rate(0.50)      // 500 bps penalty spread for failed banks
  private val NplSpreadCap: Rate               = Rate(0.15)      // max NPL-driven spread (1500 bps)
  private val CarPenaltyThreshMult: Multiplier = Multiplier(1.5) // CAR penalty kicks in below minCar × 1.5
  private val CarPenaltyScale: Multiplier      = Multiplier(2.0) // bps per unit of CAR shortfall

  // Credit approval
  private val MinApprovalProb: Share         = Share(0.1)      // floor: 10% approval even under max stress
  private val NplApprovalPenalty: Multiplier = Multiplier(3.0) // approval drop per unit NPL ratio (e.g. NPL 10% → 30pp)
  private val ReserveDeficitPenalty: Share   = Share(0.5)      // 50pp approval drop when free reserves < 0

  // Crowding-out (gov bonds vs firm loans)
  private val CrowdingOutSensitivity: Multiplier = Multiplier(0.30) // 30% of bond yield gap passed through to lending spread

  // Interbank corridor (NBP: ref ± 100 bps)
  private val DepositSpreadFromRef: Rate = Rate(0.01) // deposit facility rate = refRate − 100 bps
  private val LombardSpreadFromRef: Rate = Rate(0.01) // lombard facility rate = refRate + 100 bps

  // ---------------------------------------------------------------------------
  // ADT: BankStatus
  // ---------------------------------------------------------------------------

  /** Operational status of a bank.
    *
    *   - [[Active]] tracks consecutive months with CAR below regulatory
    *     minimum.
    *   - [[Failed]] records the month of failure (BFG resolution triggered).
    */
  enum BankStatus:
    case Active(consecutiveLowCar: Int)
    case Failed(month: Int)

  // ---------------------------------------------------------------------------
  // Aggregate balance sheet (sum over all per-bank BankStates)
  // ---------------------------------------------------------------------------

  /** Aggregate banking-sector balance sheet — sum over all 7 per-bank
    * BankStates.
    *
    * Pure DTO recomputed every step via `Banking.State.aggregate`. Read-only
    * snapshot consumed by output columns, SFC identities, macro feedback loops
    * (corporate bond absorption, insurance/NBFI asset allocation), and
    * government fiscal arithmetic. All mutation happens at the per-bank level
    * in `Banking.BankState`; this aggregate is derived, never written back.
    */
  case class Aggregate(
      totalLoans: PLN,      // Outstanding corporate loans (sum of per-bank `loans`)
      nplAmount: PLN,       // Non-performing corporate loan stock (KNF Stage 3)
      capital: PLN,         // Regulatory capital (Tier 1 + retained earnings)
      deposits: PLN,        // Total customer deposits (households + firms)
      afsBonds: PLN,        // AFS gov bond portfolio (marked to market)
      htmBonds: PLN,        // HTM gov bond portfolio (accrual only)
      consumerLoans: PLN,   // Outstanding unsecured household credit
      consumerNpl: PLN,     // Non-performing consumer loan stock
      corpBondHoldings: PLN, // Corporate bond portfolio — bank share only (default 30%, CORPBOND_BANK_SHARE)
  ):
    /** Total government bond holdings (AFS + HTM). */
    def govBondHoldings: PLN = afsBonds + htmBonds

    /** Non-performing loan ratio: nplAmount / totalLoans. Returns Share.Zero
      * when loan book is empty.
      */
    def nplRatio: Share = if totalLoans > MinBalanceThreshold then nplAmount.ratioTo(totalLoans).toShare else Share.Zero

    /** Capital adequacy ratio: capital / risk-weighted assets. Corporate bonds
      * carry 50% risk weight (Basel III, BBB bucket). Returns Multiplier(10.0)
      * (well-capitalised floor) when risk-weighted assets ≤ 1 to avoid division
      * by zero.
      */
    def car: Multiplier =
      val totalRwa = totalLoans + consumerLoans + corpBondHoldings * CorpBondRiskWeight
      if totalRwa > MinBalanceThreshold then capital.ratioTo(totalRwa).toMultiplier else SafeRatioFloor

  def aggregateFromBanks(banks: Vector[BankState]): Aggregate =
    Aggregate(
      totalLoans = banks.foldLeft(PLN.Zero)(_ + _.loans),
      nplAmount = banks.foldLeft(PLN.Zero)(_ + _.nplAmount),
      capital = banks.foldLeft(PLN.Zero)(_ + _.capital),
      deposits = banks.foldLeft(PLN.Zero)(_ + _.deposits),
      afsBonds = banks.foldLeft(PLN.Zero)(_ + _.afsBonds),
      htmBonds = banks.foldLeft(PLN.Zero)(_ + _.htmBonds),
      consumerLoans = banks.foldLeft(PLN.Zero)(_ + _.consumerLoans),
      consumerNpl = banks.foldLeft(PLN.Zero)(_ + _.consumerNpl),
      corpBondHoldings = banks.foldLeft(PLN.Zero)(_ + _.corpBondHoldings),
    )

  // ---------------------------------------------------------------------------
  // Monetary aggregates (diagnostic, not SFC-relevant)
  // ---------------------------------------------------------------------------

  /** Monetary aggregates — diagnostic, NBP-compatible definitions. M0 =
    * reserves at NBP (monetary base, excl. currency in circulation — model is
    * cashless) M1 = demand deposits only (overnight, NBP definition) M2 = M1 +
    * term deposits (deposits with agreed maturity) M3 = M2 + TFI AUM +
    * short-term corporate bonds (money market instruments)
    */
  case class MonetaryAggregates(
      m0: PLN,                     // monetary base: reserves at NBP
      m1: PLN,                     // narrow money: demand deposits
      m2: PLN,                     // intermediate: M1 + term deposits
      m3: PLN,                     // broad money: M2 + TFI AUM + corp bonds
      creditMultiplier: Multiplier, // m2 / m0 (broad deposit multiplier)
  )
  object MonetaryAggregates:
    val zero: MonetaryAggregates = MonetaryAggregates(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, Multiplier.Zero)

    def compute(banks: Vector[BankState], tfiAum: PLN, corpBondOutstanding: PLN): MonetaryAggregates =
      val alive  = banks.filterNot(_.failed)
      val m0     = PLN.fromRaw(alive.map(_.reservesAtNbp.toLong).sum)
      val demand = PLN.fromRaw(alive.map(_.demandDeposits.toLong).sum)
      val term   = PLN.fromRaw(alive.map(_.termDeposits.toLong).sum)
      val m1     = demand
      val m2     = demand + term
      val m3     = m2 + tfiAum + corpBondOutstanding
      MonetaryAggregates(m0, m1, m2, m3, m2.ratioTo(m0.max(MinBalanceThreshold)).toMultiplier)

  // ---------------------------------------------------------------------------
  // Named result types
  // ---------------------------------------------------------------------------

  /** Per-bank monetary flow with sector-wide total. */
  case class PerBankAmounts(perBank: Vector[PLN], total: PLN)

  /** Result of monthly failure check. */
  case class FailureCheckResult(banks: Vector[BankState], anyFailed: Boolean)

  /** Result of BRRD bail-in on newly failed banks. */
  case class BailInResult(banks: Vector[BankState], totalLoss: PLN)

  /** Result of BFG purchase-and-assumption resolution. */
  case class ResolutionResult(banks: Vector[BankState], absorberId: BankId)

  // ---------------------------------------------------------------------------
  // Config
  // ---------------------------------------------------------------------------

  /** Configuration for a single bank in the multi-bank system. */
  case class Config(
      id: BankId,                   // unique bank identifier (0–6)
      name: String,                 // human-readable label (KNF registry)
      initMarketShare: Share,       // deposit-weighted share at t = 0
      initCet1: Share,              // initial CET1 ratio (KNF 2024)
      lendingSpread: Rate,          // bank-specific spread over base lending rate
      sectorAffinity: Vector[Share], // relative lending preference per sector
  )

  // ---------------------------------------------------------------------------
  // BankState
  // ---------------------------------------------------------------------------

  /** State of an individual bank (updated each month).
    *
    * All fields are explicit — no defaults. Constructor calls must supply every
    * field, same convention as `Firm.State`.
    */
  case class BankState(
      id: BankId,                                          // unique bank identifier (index into banks vector)
      deposits: PLN,                                       // total customer deposits (HH + firms)
      loans: PLN,                                          // outstanding corporate loan book
      capital: PLN,                                        // regulatory capital (Tier 1 + retained earnings)
      nplAmount: PLN,                                      // non-performing loan stock (KNF Stage 3)
      afsBonds: PLN,                                       // Available-for-Sale gov bonds (marked to market each month)
      htmBonds: PLN,                                       // Held-to-Maturity gov bonds (accrual only, hidden losses)
      htmBookYield: Rate,                                  // weighted-average acquisition yield on HTM portfolio
      reservesAtNbp: PLN,                                  // excess reserves held at NBP
      interbankNet: PLN,                                   // net interbank position (positive = lender)
      status: BankStatus,                                  // operational status (Active with CAR counter, or Failed)
      demandDeposits: PLN,                                 // demand deposits (% split from total deposits)
      termDeposits: PLN,                                   // term deposits
      loansShort: PLN,                                     // short-term loans (< 1 year)
      loansMedium: PLN,                                    // medium-term loans (1–5 years)
      loansLong: PLN,                                      // long-term loans (> 5 years)
      consumerLoans: PLN,                                  // outstanding unsecured household credit
      consumerNpl: PLN,                                    // consumer credit NPL stock
      corpBondHoldings: PLN,                               // corporate bond holdings (bank share, Basel III BBB)
      eclStaging: EclStaging.State = EclStaging.State.zero, // IFRS 9 ECL staging (S1/S2/S3)
  ):

    /** Total government bond holdings (AFS + HTM). All existing read-only
      * references (hqla, rsf, canLend, etc.) use this derived method.
      */
    def govBondHoldings: PLN = afsBonds + htmBonds

    /** Whether this bank has been resolved by BFG. */
    def failed: Boolean = status match
      case BankStatus.Failed(_) => true
      case _                    => false

    /** Month of failure, or 0 if active. */
    def failedMonth: Int = status match
      case BankStatus.Failed(m) => m
      case _                    => 0

    /** Consecutive months with CAR below regulatory minimum, or 0 if failed. */
    def consecutiveLowCar: Int = status match
      case BankStatus.Active(c) => c
      case _                    => 0

    /** Non-performing loan ratio: NPL / loans. Returns Share.Zero when loan
      * book is empty.
      */
    def nplRatio: Share =
      if loans > MinBalanceThreshold then nplAmount.ratioTo(loans).toShare else Share.Zero

    /** Capital adequacy ratio: capital / risk-weighted assets (Basel III). Corp
      * bonds carry [[CorpBondRiskWeight]] risk weight (BBB bucket). Returns
      * [[SafeRatioFloor]] when RWA ≤ [[MinBalanceThreshold]].
      */
    def car: Multiplier =
      val totalRwa = loans + consumerLoans + corpBondHoldings * CorpBondRiskWeight
      if totalRwa > MinBalanceThreshold then capital.ratioTo(totalRwa).toMultiplier else SafeRatioFloor

    /** High-quality liquid assets: reserves + gov bonds (Basel III Level 1). */
    def hqla: PLN = reservesAtNbp + govBondHoldings

    /** Net cash outflows (30-day): demand deposits × runoff rate. */
    def netCashOutflows(using p: SimParams): PLN = demandDeposits * p.banking.demandDepositRunoff

    /** Liquidity Coverage Ratio = HQLA / net cash outflows (Basel III). */
    def lcr(using p: SimParams): Multiplier =
      if netCashOutflows > MinBalanceThreshold then hqla.ratioTo(netCashOutflows).toMultiplier
      else SafeRatioFloor

    /** Available Stable Funding (Basel III NSFR numerator). */
    def asf: PLN = capital + termDeposits * AsfTermWeight + demandDeposits * AsfDemandWeight

    /** Required Stable Funding (Basel III NSFR denominator). */
    def rsf: PLN =
      loansShort * RsfShort + loansMedium * RsfMedium + loansLong * RsfLong +
        govBondHoldings * RsfGovBond + corpBondHoldings * RsfCorpBond

    /** Net Stable Funding Ratio = ASF / RSF. */
    def nsfr: Multiplier =
      if rsf > MinBalanceThreshold then asf.ratioTo(rsf).toMultiplier else SafeRatioFloor

  /** State of the entire banking sector. */
  case class State(
      banks: Vector[BankState],
      interbankRate: Rate,
      configs: Vector[Config],
      interbankCurve: Option[YieldCurve.State],
  ):
    def aggregate: Aggregate = aggregateFromBanks(banks)

    def marketState: MarketState =
      MarketState(
        interbankRate = interbankRate,
        configs = configs,
        interbankCurve = interbankCurve,
      )

  /** Banking-sector state that belongs to macro/market runtime state, without
    * the explicit bank population.
    */
  case class MarketState(
      interbankRate: Rate,
      configs: Vector[Config],
      interbankCurve: Option[YieldCurve.State],
  ):
    def withBanks(banks: Vector[BankState]): State =
      State(
        banks = banks,
        interbankRate = interbankRate,
        configs = configs,
        interbankCurve = interbankCurve,
      )

  // ---------------------------------------------------------------------------
  // Default configs (7 Polish banks, KNF 2024)
  // ---------------------------------------------------------------------------

  private def affinity(xs: Double*): Vector[Share] = xs.map(Share(_)).toVector

  val DefaultConfigs: Vector[Config] = Vector(
    Config(BankId(0), "PKO BP", Share(0.175), Share(0.185), Rate(-0.002), affinity(0.15, 0.15, 0.15, 0.10, 0.30, 0.15)),
    Config(BankId(1), "Pekao", Share(0.120), Share(0.178), Rate(-0.001), affinity(0.15, 0.20, 0.20, 0.15, 0.15, 0.15)),
    Config(BankId(2), "mBank", Share(0.085), Share(0.169), Rate(0.000), affinity(0.30, 0.10, 0.25, 0.10, 0.10, 0.15)),
    Config(BankId(3), "ING BSK", Share(0.075), Share(0.172), Rate(-0.001), affinity(0.15, 0.35, 0.15, 0.10, 0.10, 0.15)),
    Config(BankId(4), "Santander", Share(0.070), Share(0.170), Rate(0.000), affinity(0.15, 0.10, 0.35, 0.15, 0.10, 0.15)),
    Config(BankId(5), "BPS/Coop", Share(0.050), Share(0.150), Rate(0.003), affinity(0.05, 0.10, 0.10, 0.05, 0.05, 0.65)),
    Config(BankId(6), "Others", Share(0.425), Share(0.165), Rate(0.001), affinity(0.15, 0.17, 0.17, 0.17, 0.17, 0.17)),
  )

  // ---------------------------------------------------------------------------
  // Bank assignment
  // ---------------------------------------------------------------------------

  /** Assign a firm to a bank based on sector affinity and market share. */
  def assignBank(firmSector: SectorIdx, configs: Vector[Config], rng: Random): BankId =
    val weights = configs.map(c => c.sectorAffinity(firmSector.toInt) * c.initMarketShare) // Share * Share → Share
    val total   = weights.map(_.toLong).sum
    if total <= 0L then BankId(0)
    else
      val r      = (rng.nextDouble() * total).toLong
      val cumul  = weights.map(_.toLong).scanLeft(0L)(_ + _).tail
      val picked = cumul.indexWhere(_ > r)
      BankId(if picked >= 0 then picked else weights.length - 1)

  // ---------------------------------------------------------------------------
  // Rates
  // ---------------------------------------------------------------------------

  /** HH deposit rate (annual). Polish banks: NBP rate − spread, floored at
    * zero.
    */
  def hhDepositRate(refRate: Rate)(using p: SimParams): Rate =
    (refRate - p.household.depositSpread).max(Rate.Zero)

  /** Lending rate charged to firms. Reflects credit risk (NPL spread), capital
    * adequacy pressure (CAR penalty), and crowding-out from government bonds —
    * when risk-free yields are attractive, banks demand higher spreads on risky
    * firm loans. Failed banks get a flat penalty rate.
    */
  def lendingRate(bank: BankState, cfg: Config, refRate: Rate, bondYield: Rate)(using p: SimParams): Rate =
    if bank.failed then refRate + FailedBankSpread
    else
      val nplSpread   = (bank.nplRatio * p.banking.nplSpreadFactor).toRate.min(NplSpreadCap)
      val carThresh   = p.banking.minCar * CarPenaltyThreshMult
      val carPenalty  =
        if bank.car < carThresh then ((carThresh - bank.car) * CarPenaltyScale).toRate
        else Rate.Zero
      val crowdingOut = (bondYield - refRate - p.banking.baseSpread).max(Rate.Zero) * CrowdingOutSensitivity
      refRate + p.banking.baseSpread + cfg.lendingSpread + nplSpread + carPenalty + crowdingOut

  /** Interbank rate (WIBOR O/N proxy): blends credit stress (NPL) and liquidity
    * position (excess reserves). Under excess liquidity (post-QE, post-FX
    * intervention) rate falls toward deposit facility floor. Under scarce
    * liquidity + NPL stress, rate rises toward lombard ceiling.
    *
    * rate = depositRate + (1 − liquidityRatio) × creditStress × corridor
    */
  def interbankRate(banks: Vector[BankState], refRate: Rate)(using p: SimParams): Rate =
    val alive       = banks.filterNot(_.failed)
    val aggNpl      = PLN.fromRaw(alive.map(_.nplAmount.toLong).sum)
    val aggLoans    = PLN.fromRaw(alive.map(_.loans.toLong).sum)
    val aggDeposits = PLN.fromRaw(alive.map(_.deposits.toLong).sum)
    val aggReserves = PLN.fromRaw(alive.map(_.reservesAtNbp.toLong).sum)

    // Credit stress: NPL ratio relative to stress threshold (0 = healthy, 1 = max stress)
    val aggNplShare  = if aggLoans > MinBalanceThreshold then aggNpl.ratioTo(aggLoans).toShare else Share.Zero
    val creditStress = aggNplShare.ratioTo(p.banking.stressThreshold).toShare.clamp(Share.Zero, Share.One)

    // Liquidity position: excess reserves relative to required (0 = scarce, 1 = abundant)
    val requiredReserves = aggDeposits * p.banking.reserveReq
    val excessReserves   = aggReserves - requiredReserves
    val liquidityRatio   =
      if requiredReserves > PLN.Zero then excessReserves.ratioTo(requiredReserves).toShare.clamp(Share.Zero, Share.One)
      else Share.One

    val depositRate  = Rate.Zero.max(refRate - DepositSpreadFromRef)
    val lombardRate  = refRate + LombardSpreadFromRef
    val corridor     = lombardRate - depositRate
    // rate = depositRate + (1 - liquidityRatio) × creditStress × corridor
    val stressFactor = (Share.One - liquidityRatio) * creditStress // Share * Share → Share
    depositRate + corridor * stressFactor

  // ---------------------------------------------------------------------------
  // Credit approval
  // ---------------------------------------------------------------------------

  /** Can this bank lend `amount`? Checks projected CAR, LCR/NSFR, and
    * stochastic approval probability penalised by NPL ratio and reserve
    * utilisation. Reserve constraint is soft: approval probability decreases as
    * the bank approaches full reserve utilisation, rather than a hard block
    * (banks can temporarily fund via interbank market).
    */
  def canLend(bank: BankState, amount: PLN, rng: Random, ccyb: Multiplier)(using p: SimParams): Boolean =
    if bank.failed then false
    else
      val projectedRwa = bank.loans + bank.consumerLoans + bank.corpBondHoldings * CorpBondRiskWeight + amount
      val projectedCar = if projectedRwa > MinBalanceThreshold then bank.capital.ratioTo(projectedRwa).toMultiplier else SafeRatioFloor
      val minCar       = Macroprudential.effectiveMinCar(bank.id.toInt, ccyb)
      val carOk        = projectedCar >= minCar
      val lcrOk        = bank.lcr >= p.banking.lcrMin
      val nsfrOk       = bank.nsfr >= p.banking.nsfrMin
      val nplPenalty   = bank.nplRatio * NplApprovalPenalty // Share * Multiplier → Multiplier
      val freeReserves = bank.deposits * (Share.One - p.banking.reserveReq) - bank.loans - bank.govBondHoldings
      val resPenalty   = if freeReserves > PLN.Zero then Share.Zero else ReserveDeficitPenalty
      val approvalP    = (Share.One - nplPenalty.toShare - resPenalty).max(MinApprovalProb)
      carOk && lcrOk && nsfrOk && approvalP.sampleBelow(rng)

  // ---------------------------------------------------------------------------
  // Interbank market
  // ---------------------------------------------------------------------------

  /** Clear the interbank market: excess reserves → lender/borrower netting.
    * Hoarding factor [0,1] scales lending: 0 = full freeze, 1 = normal.
    */
  def clearInterbank(banks: Vector[BankState], configs: Vector[Config], hoarding: Share = Share.One)(using
      p: SimParams,
  ): Vector[BankState] =
    val excess: Vector[PLN] = banks
      .zip(configs)
      .map: (b, _) =>
        if b.failed then PLN.Zero
        else b.deposits * (Share.One - p.banking.reserveReq) - b.loans - b.govBondHoldings

    val lenderIdxs     = excess.indices.filter(i => excess(i) > PLN.Zero && !banks(i).failed).toVector
    val borrowerIdxs   = excess.indices.filter(i => excess(i) < PLN.Zero && !banks(i).failed).toVector
    val lenderCaps     = lenderIdxs.map(i => (excess(i) * hoarding).toLong.max(0L))
    val borrowerNeeds  = borrowerIdxs.map(i => (-excess(i)).toLong)
    val totalLending   = lenderCaps.sum
    val totalBorrowing = borrowerNeeds.sum

    if totalLending <= 0L || totalBorrowing <= 0L then banks.map(_.copy(interbankNet = PLN.Zero, reservesAtNbp = PLN.Zero))
    else
      val matched        = math.min(totalLending, totalBorrowing)
      val lenderLoans    = Distribute.distribute(matched, lenderCaps.toArray)
      val borrowerLoans  = Distribute.distribute(matched, borrowerNeeds.toArray)
      val lenderLoanById = lenderIdxs.zip(lenderLoans.iterator).toMap
      val borrowerById   = borrowerIdxs.zip(borrowerLoans.iterator).toMap

      banks.indices
        .map: i =>
          val b = banks(i)
          if b.failed then b.copy(interbankNet = PLN.Zero, reservesAtNbp = PLN.Zero)
          else
            lenderLoanById.get(i) match
              case Some(lent) =>
                b.copy(interbankNet = PLN.fromRaw(lent), reservesAtNbp = excess(i) - PLN.fromRaw(lent))
              case None       =>
                borrowerById.get(i) match
                  case Some(borrowed) =>
                    b.copy(interbankNet = PLN.fromRaw(-borrowed), reservesAtNbp = PLN.Zero)
                  case None           =>
                    b.copy(interbankNet = PLN.Zero, reservesAtNbp = PLN.Zero)
        .toVector

  // ---------------------------------------------------------------------------
  // Failure detection and resolution
  // ---------------------------------------------------------------------------

  /** Check for bank failures: CAR < effectiveMinCar for 3 consecutive months,
    * or LCR breach at 50% of minimum. Already-failed banks pass through.
    */
  def checkFailures(
      banks: Vector[BankState],
      month: Int,       // simulation month (recorded in BankStatus.Failed)
      enabled: Boolean, // whether failure mechanism is active
      ccyb: Multiplier, // countercyclical capital buffer
  )(using p: SimParams): FailureCheckResult =
    if !enabled then
      FailureCheckResult(
        banks = banks.map: b =>
          b.status match
            case BankStatus.Active(_) => b.copy(status = BankStatus.Active(0))
            case _                    => b
        ,
        anyFailed = false,
      )
    else
      val updated    = banks.map: b =>
        if b.failed then b
        else
          val consec    = b.consecutiveLowCar
          val minCar    = Macroprudential.effectiveMinCar(b.id.toInt, ccyb)
          val lowCar    = b.car < minCar
          val lcrBreach = b.lcr < p.banking.lcrMin * Share(0.5)
          val newConsec = if lowCar then consec + 1 else 0
          if newConsec >= 3 || lcrBreach then b.copy(status = BankStatus.Failed(month), capital = PLN.Zero)
          else b.copy(status = BankStatus.Active(newConsec))
      val prevFailed = banks.filter(_.failed).map(_.id).toSet
      FailureCheckResult(updated, updated.exists(b => b.failed && !prevFailed.contains(b.id)))

  /** Compute monthly BFG levy for all banks.
    *
    * Failed banks pay no levy. Active banks pay deposits × bfgLevyRate / 12.
    */
  def computeBfgLevy(banks: Vector[BankState])(using p: SimParams): PerBankAmounts =
    val perBank = banks.map: b =>
      if b.failed then PLN.Zero
      else b.deposits * p.banking.bfgLevyRate.monthly
    PerBankAmounts(perBank, PLN.fromRaw(perBank.map(_.toLong).sum))

  /** Bail-in: haircut uninsured deposits on failed banks. Deposits below
    * bfgDepositGuarantee are protected.
    */
  def applyBailIn(banks: Vector[BankState])(using p: SimParams): BailInResult =
    val withHaircut = banks.map: b =>
      if b.failed && b.deposits > PLN.Zero then
        val guaranteed = b.deposits.min(p.banking.bfgDepositGuarantee)
        val uninsured  = b.deposits - guaranteed
        val haircut    = uninsured * p.banking.bailInDepositHaircut
        (b.copy(deposits = b.deposits - haircut), haircut)
      else (b, PLN.Zero)
    BailInResult(withHaircut.map(_._1), PLN.fromRaw(withHaircut.map(_._2.toLong).sum))

  /** BFG P&A resolution: transfer deposits, bonds, performing loans, consumer
    * loans from failed banks to the healthiest surviving bank.
    */
  def resolveFailures(banks: Vector[BankState]): ResolutionResult =
    val newlyFailed = banks.filter(b => b.failed && b.deposits > PLN.Zero)
    if newlyFailed.isEmpty then ResolutionResult(banks, BankId.NoBank)
    else
      val absorberId = healthiestBankId(banks)
      val toAbsorb   = newlyFailed.filter(_.id != absorberId)
      // Single-pass PLN aggregation of all flows from failed banks (exact Long addition)
      val addDep     = PLN.fromRaw(toAbsorb.map(_.deposits.toLong).sum)
      val addLoans   = PLN.fromRaw(toAbsorb.map(f => (f.loans - f.nplAmount).toLong).sum)
      val addAfs     = PLN.fromRaw(toAbsorb.map(_.afsBonds.toLong).sum)
      val addHtm     = PLN.fromRaw(toAbsorb.map(_.htmBonds.toLong).sum)
      val addCorpB   = PLN.fromRaw(toAbsorb.map(_.corpBondHoldings.toLong).sum)
      val addCC      = PLN.fromRaw(toAbsorb.map(_.consumerLoans.toLong).sum)
      val addIB      = PLN.fromRaw(toAbsorb.map(_.interbankNet.toLong).sum)
      // Weighted yield: Σ(htmBonds × htmBookYield) — PLN × Rate → PLN
      val htmYieldWt = PLN.fromRaw(toAbsorb.map(f => (f.htmBonds * f.htmBookYield).toLong).sum)

      val resolved = banks.map: b =>
        if b.id == absorberId then
          val combinedHtm      = b.htmBonds + addHtm
          val combinedHtmYield =
            if combinedHtm > PLN.Zero then Rate((b.htmBonds * b.htmBookYield + htmYieldWt) / combinedHtm)
            else b.htmBookYield
          b.copy(
            deposits = b.deposits + addDep,
            loans = (b.loans + addLoans).max(PLN.Zero),
            afsBonds = b.afsBonds + addAfs,
            htmBonds = combinedHtm,
            htmBookYield = combinedHtmYield,
            corpBondHoldings = b.corpBondHoldings + addCorpB,
            consumerLoans = b.consumerLoans + addCC,
            interbankNet = b.interbankNet + addIB,
            status = BankStatus.Active(0),
          )
        else if b.failed && b.deposits > PLN.Zero then
          b.copy(
            deposits = PLN.Zero,
            loans = PLN.Zero,
            afsBonds = PLN.Zero,
            htmBonds = PLN.Zero,
            htmBookYield = Rate.Zero,
            nplAmount = PLN.Zero,
            interbankNet = PLN.Zero,
            reservesAtNbp = PLN.Zero,
            corpBondHoldings = PLN.Zero,
            consumerLoans = PLN.Zero,
            consumerNpl = PLN.Zero,
          )
        else b
      ResolutionResult(resolved, absorberId)

  /** Find the healthiest (highest CAR) surviving bank. Falls back to highest
    * capital if all banks have failed.
    */
  def healthiestBankId(banks: Vector[BankState]): BankId =
    val alive = banks.filterNot(_.failed)
    if alive.isEmpty then banks.maxBy(_.capital.toLong).id
    else alive.maxBy(_.car).id

  /** Reassign a firm/household from a failed bank to the healthiest surviving
    * bank.
    */
  def reassignBankId(currentBankId: BankId, banks: Vector[BankState]): BankId =
    if currentBankId.toInt < banks.length && !banks(currentBankId.toInt).failed then currentBankId
    else healthiestBankId(banks)

  // ---------------------------------------------------------------------------
  // Bond allocation
  // ---------------------------------------------------------------------------

  /** Allocate new bond issuance to banks proportional to deposits using
    * `ledger.Distribute` for exact residual closure.
    */
  def allocateBondIssuance(banks: Vector[BankState], issuance: PLN, currentYield: Rate)(using p: SimParams): Vector[BankState] =
    if issuance <= PLN.Zero then return banks
    allocateBondChange(banks, issuance, currentYield)

  /** Allocate bond redemptions to banks proportional to deposits using
    * `ledger.Distribute` for exact residual closure.
    */
  def allocateBondRedemption(banks: Vector[BankState], redemption: PLN, currentYield: Rate)(using p: SimParams): Vector[BankState] =
    if redemption <= PLN.Zero then return banks
    allocateBondChange(banks, PLN.fromRaw(-redemption.toLong), currentYield)

  /** Compat wrapper for signed net issuance.
    *
    *   - `netIssuance > 0` means new bond issuance
    *   - `netIssuance < 0` means bond redemption
    */
  def allocateBonds(banks: Vector[BankState], netIssuance: PLN, currentYield: Rate)(using p: SimParams): Vector[BankState] =
    if netIssuance > PLN.Zero then allocateBondIssuance(banks, netIssuance, currentYield)
    else if netIssuance < PLN.Zero then allocateBondRedemption(banks, PLN.fromRaw(-netIssuance.toLong), currentYield)
    else banks

  private def allocateBondChange(banks: Vector[BankState], signedChange: PLN, currentYield: Rate)(using p: SimParams): Vector[BankState] =
    if signedChange == PLN.Zero then return banks
    val aliveBanks = banks.filterNot(_.failed)
    if aliveBanks.isEmpty then return banks

    val weights =
      val nonNegativeDeposits = aliveBanks.map(b => b.deposits.toLong.max(0L))
      val totalDep            = nonNegativeDeposits.sum
      if totalDep > 0L then nonNegativeDeposits.toArray
      else Array.fill(aliveBanks.length)(1L)

    val magnitudes = Distribute.distribute(math.abs(signedChange.toLong), weights)
    val signedById = aliveBanks
      .zip(magnitudes.iterator)
      .map { case (b, amount) =>
        val signedAmount = if signedChange.toLong > 0L then PLN.fromRaw(amount) else PLN.fromRaw(-amount)
        b.id -> signedAmount
      }
      .toMap

    banks.map { b =>
      signedById.get(b.id).fold(b)(amount => applyBondAllocation(b, amount, currentYield))
    }

  private def applyBondAllocation(b: BankState, amount: PLN, currentYield: Rate)(using p: SimParams): BankState =
    if amount > PLN.Zero then
      // Issuance: split per htmShare, update book yield as weighted average
      val htmPortion   = amount * p.banking.htmShare
      val afsPortion   = amount - htmPortion
      val newHtmTotal  = b.htmBonds + htmPortion
      val newBookYield =
        if newHtmTotal > PLN.Zero then Rate((b.htmBonds * b.htmBookYield + htmPortion * currentYield) / newHtmTotal)
        else b.htmBookYield
      b.copy(afsBonds = b.afsBonds + afsPortion, htmBonds = newHtmTotal, htmBookYield = newBookYield)
    else if amount < PLN.Zero then
      // Redemption: reduce both proportionally (no floor — matches original behavior)
      val total = b.govBondHoldings
      if total <= PLN.Zero then b.copy(afsBonds = b.afsBonds + amount * Share(0.5), htmBonds = b.htmBonds + amount * Share(0.5))
      else
        val afsFrac   = b.afsBonds.ratioTo(total).toShare
        val afsReduce = amount * afsFrac
        val htmReduce = amount - afsReduce
        b.copy(afsBonds = b.afsBonds + afsReduce, htmBonds = b.htmBonds + htmReduce)
    else b

  /** Result of a bond sale from banks to a single buyer. */
  case class BondSaleResult(banks: Vector[BankState], actualSold: PLN)

  /** Remove bonds from banks proportional to holdings, transferring to a buyer.
    * Returns updated banks and the actual PLN sold (may be less than requested
    * if banks lack sufficient holdings). Sells AFS first; spills into HTM. SFC
    * invariant: actualSold leaves banks = actualSold arrives at buyer.
    */
  def sellToBuyer(banks: Vector[BankState], requested: PLN): BondSaleResult =
    if requested <= PLN.Zero then BondSaleResult(banks, PLN.Zero)
    else
      val eligible   = banks.filter(b => !b.failed && b.govBondHoldings > PLN.Zero)
      val totalBonds = PLN.fromRaw(eligible.map(_.govBondHoldings.toLong).sum)
      if totalBonds <= PLN.Zero then BondSaleResult(banks, PLN.Zero)
      else
        val requestedSale  = requested.min(totalBonds)
        val soldMagnitudes = Distribute.distribute(requestedSale.toLong, eligible.map(_.govBondHoldings.toLong).toArray)
        val soldById       = eligible
          .zip(soldMagnitudes.iterator)
          .map { case (b, sold) =>
            b.id -> PLN.fromRaw(sold)
          }
          .toMap
        val result         = banks.map { b =>
          soldById.get(b.id).fold(b) { sold =>
            val afsReduce = sold.min(b.afsBonds)
            val htmReduce = sold - afsReduce
            b.copy(
              afsBonds = (b.afsBonds - afsReduce).max(PLN.Zero),
              htmBonds = (b.htmBonds - htmReduce).max(PLN.Zero),
            )
          }
        }
        BondSaleResult(result, requestedSale)

  // ---------------------------------------------------------------------------
  // HTM forced reclassification (interest rate risk)
  // ---------------------------------------------------------------------------

  /** Result of HTM forced reclassification across all banks. */
  case class HtmForcedSaleResult(banks: Vector[BankState], totalRealizedLoss: PLN)

  /** Forcibly reclassify HTM bonds to AFS when LCR drops below
    * `htmForcedSaleThreshold × lcrMin`. On reclassification the hidden
    * mark-to-market loss is realized: realizedLoss = reclassified × duration ×
    * max(currentYield − htmBookYield, 0) Total `govBondHoldings` is unchanged —
    * only the AFS/HTM split moves.
    */
  def processHtmForcedSale(banks: Vector[BankState], currentYield: Rate)(using p: SimParams): HtmForcedSaleResult =
    val threshold = p.banking.htmForcedSaleThreshold * p.banking.lcrMin
    var totalLoss = PLN.Zero
    val updated   = banks.map: b =>
      if b.failed || b.htmBonds <= PLN.Zero || b.lcr >= threshold then b
      else
        val reclassified = b.htmBonds * p.banking.htmForcedSaleRate
        val yieldGap     = (currentYield - b.htmBookYield).max(Rate.Zero)
        val loss         = reclassified * yieldGap * Multiplier(p.banking.govBondDuration)
        totalLoss = totalLoss + loss
        b.copy(
          htmBonds = b.htmBonds - reclassified,
          afsBonds = b.afsBonds + reclassified,
          capital = b.capital - loss,
        )
    HtmForcedSaleResult(updated, totalLoss)

  // ---------------------------------------------------------------------------
  // Per-bank capital PnL
  // ---------------------------------------------------------------------------

  /** All per-bank PnL components needed to compute the capital delta. */
  case class CapitalPnlInput(
      prevCapital: PLN,            // previous period capital
      nplLoss: PLN,                // corporate NPL loss (after recovery)
      mortgageNplLoss: PLN,        // mortgage default loss (bank share)
      consumerNplLoss: PLN,        // consumer credit NPL loss (after recovery)
      corpBondDefaultLoss: PLN,    // corporate bond default loss (bank share)
      bfgLevy: PLN,                // BFG resolution fund levy
      unrealizedBondLoss: PLN,     // mark-to-market loss on gov bond portfolio (interest rate risk channel)
      intIncome: PLN,              // interest income on corporate loans
      hhDebtService: PLN,          // household mortgage debt service
      bondIncome: PLN,             // government bond coupon income
      depositInterest: PLN,        // interest paid on deposits (cost)
      reserveInterest: PLN,        // reserve remuneration from NBP
      standingFacilityIncome: PLN, // standing facility net income
      interbankInterest: PLN,      // interbank market interest
      mortgageInterestIncome: PLN, // mortgage interest income (bank share)
      consumerDebtService: PLN,    // consumer credit debt service
      corpBondCoupon: PLN,         // corporate bond coupon income (bank share)
  )

  /** Result of per-bank capital PnL computation. */
  case class CapitalPnlOutput(
      newCapital: PLN, // updated capital after all PnL flows
  )

  /** Compute new bank capital from previous capital and monthly PnL flows.
    *
    * Pure function: losses reduce capital 1:1, income items are retained at
    * `profitRetention` rate (SimParams).
    */
  def computeCapitalDelta(in: CapitalPnlInput)(using p: SimParams): CapitalPnlOutput =
    val losses         = in.nplLoss + in.mortgageNplLoss + in.consumerNplLoss +
      in.corpBondDefaultLoss + in.bfgLevy + in.unrealizedBondLoss
    val grossIncome    = in.intIncome + in.hhDebtService + in.bondIncome - in.depositInterest +
      in.reserveInterest + in.standingFacilityIncome + in.interbankInterest +
      in.mortgageInterestIncome + in.consumerDebtService + in.corpBondCoupon
    val retainedIncome = grossIncome * p.banking.profitRetention
    CapitalPnlOutput(newCapital = in.prevCapital - losses + retainedIncome)

  // ---------------------------------------------------------------------------
  // Monetary plumbing
  // ---------------------------------------------------------------------------

  /** Monthly reserve interest for a single bank: reserves × refRate × mult / 12.
    */
  def reserveInterest(bank: BankState, refRate: Rate)(using p: SimParams): PLN =
    if bank.failed || bank.reservesAtNbp <= PLN.Zero then PLN.Zero
    else bank.reservesAtNbp * (refRate * p.monetary.reserveRateMult.toMultiplier).monthly

  /** Reserve interest for all banks → per-bank amounts + sector total. */
  def computeReserveInterest(banks: Vector[BankState], refRate: Rate)(using SimParams): PerBankAmounts =
    val perBank = banks.map(b => reserveInterest(b, refRate))
    PerBankAmounts(perBank, PLN.fromRaw(perBank.map(_.toLong).sum))

  /** Standing facility flows (monthly): deposit rate for excess reserves,
    * lombard rate for borrowers. Always-on — the NBP corridor (ref ± 100 bps)
    * is structural, not optional.
    */
  def computeStandingFacilities(banks: Vector[BankState], refRate: Rate)(using p: SimParams): PerBankAmounts =
    val depositRate = (refRate - p.monetary.depositFacilitySpread).max(Rate.Zero)
    val lombardRate = refRate + p.monetary.lombardSpread
    val perBank     = banks.map: b =>
      if b.failed then PLN.Zero
      else if b.reservesAtNbp > PLN.Zero then b.reservesAtNbp * depositRate.monthly
      else if b.interbankNet < PLN.Zero then -(b.interbankNet.abs * lombardRate.monthly)
      else PLN.Zero
    PerBankAmounts(perBank, PLN.fromRaw(perBank.map(_.toLong).sum))

  /** Interbank interest flows (monthly). Net zero in aggregate (closed system).
    */
  def interbankInterestFlows(banks: Vector[BankState], rate: Rate): PerBankAmounts =
    val perBank = banks.map: b =>
      if b.failed then PLN.Zero
      else b.interbankNet * rate.monthly
    PerBankAmounts(perBank, PLN.fromRaw(perBank.map(_.toLong).sum))
