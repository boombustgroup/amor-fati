package com.boombustgroup.amorfati.agents

import com.boombustgroup.ledger.Distribute
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.mechanisms.{Macroprudential, YieldCurve}
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

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

  type BankCorpBondHoldings = BankId => PLN

  def noBankCorpBondHoldings: BankCorpBondHoldings = _ => PLN.Zero

  def bankCorpBondHoldingsFromVector(holdings: Vector[PLN]): BankCorpBondHoldings =
    bankId => holdings.lift(bankId.toInt).getOrElse(PLN.Zero)

  def nplRatio(totalLoans: PLN, nplAmount: PLN): Share =
    if totalLoans > MinBalanceThreshold then nplAmount.ratioTo(totalLoans).toShare else Share.Zero

  def capitalAdequacyRatio(capital: PLN, firmLoans: PLN, consumerLoans: PLN, corpBondHoldings: PLN): Multiplier =
    val totalRwa = firmLoans + consumerLoans + corpBondHoldings * CorpBondRiskWeight
    if totalRwa > MinBalanceThreshold then capital.ratioTo(totalRwa).toMultiplier else SafeRatioFloor

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
    case Failed(month: ExecutionMonth)

  // ---------------------------------------------------------------------------
  // Aggregate balance sheet (sum over all per-bank BankStates)
  // ---------------------------------------------------------------------------

  /** Aggregate banking-sector balance sheet — sum over all 7 per-bank
    * BankStates.
    *
    * Pure DTO recomputed from bank operational state plus explicit financial
    * stock rows. Read-only snapshot consumed by output columns, SFC identities,
    * macro feedback loops (corporate bond absorption, insurance/NBFI asset
    * allocation), and government fiscal arithmetic. Ledger-owned corporate-bond
    * holdings are supplied by the caller; this aggregate is derived, never
    * written back.
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
    def nplRatio: Share = Banking.nplRatio(totalLoans, nplAmount)

    /** Capital adequacy ratio: capital / risk-weighted assets. Corporate bonds
      * carry 50% risk weight (Basel III, BBB bucket). Returns Multiplier(10.0)
      * (well-capitalised floor) when risk-weighted assets ≤ 1 to avoid division
      * by zero.
      */
    def car: Multiplier =
      Banking.capitalAdequacyRatio(capital, totalLoans, consumerLoans, corpBondHoldings)

  def aggregateFromBankStocks(
      banks: Vector[BankState],
      financialStocks: Vector[BankFinancialStocks],
      bankCorpBondHoldings: BankCorpBondHoldings = noBankCorpBondHoldings,
  ): Aggregate =
    require(
      banks.length == financialStocks.length,
      s"Banking.aggregateFromBankStocks requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
    Aggregate(
      totalLoans = financialStocks.foldLeft(PLN.Zero)(_ + _.firmLoan),
      nplAmount = banks.foldLeft(PLN.Zero)(_ + _.nplAmount),
      capital = banks.foldLeft(PLN.Zero)(_ + _.capital),
      deposits = financialStocks.foldLeft(PLN.Zero)(_ + _.totalDeposits),
      afsBonds = financialStocks.foldLeft(PLN.Zero)(_ + _.govBondAfs),
      htmBonds = financialStocks.foldLeft(PLN.Zero)(_ + _.govBondHtm),
      consumerLoans = financialStocks.foldLeft(PLN.Zero)(_ + _.consumerLoan),
      consumerNpl = banks.foldLeft(PLN.Zero)(_ + _.consumerNpl),
      corpBondHoldings = banks.foldLeft(PLN.Zero)((acc, bank) => acc + bankCorpBondHoldings(bank.id)),
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

    def computeFromBankStocks(
        banks: Vector[BankState],
        financialStocks: Vector[BankFinancialStocks],
        tfiAum: PLN,
        corpBondOutstanding: PLN,
    ): MonetaryAggregates =
      require(
        banks.length == financialStocks.length,
        s"Banking.MonetaryAggregates.computeFromBankStocks requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
      )
      val alive  = banks.zip(financialStocks).filterNot(_._1.failed).map(_._2)
      val m0     = alive.map(_.reserve).sum
      val demand = alive.map(_.demandDeposit).sum
      val term   = alive.map(_.termDeposit).sum
      val m1     = demand
      val m2     = demand + term
      val m3     = m2 + tfiAum + corpBondOutstanding
      MonetaryAggregates(m0, m1, m2, m3, m2.ratioTo(m0.max(MinBalanceThreshold)).toMultiplier)

  // ---------------------------------------------------------------------------
  // Named result types
  // ---------------------------------------------------------------------------

  /** Per-bank monetary flow with sector-wide total. */
  case class PerBankAmounts(perBank: Vector[PLN], total: PLN)

  /** Pair of operational bank state and ledger-owned bank financial stocks. */
  case class BankStockState(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks])

  /** Result of monthly failure check. */
  case class FailureCheckResult(banks: Vector[BankState], anyFailed: Boolean)

  /** Result of BRRD bail-in on newly failed banks. */
  case class BailInResult(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks], totalLoss: PLN)

  /** Result of BFG purchase-and-assumption resolution. */
  case class ResolutionResult(
      banks: Vector[BankState],
      financialStocks: Vector[BankFinancialStocks],
      absorberId: BankId,
      bankCorpBondHoldings: Vector[PLN] = Vector.empty,
  )

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

  /** Ledger-contracted bank financial stocks owned by LedgerFinancialState. */
  case class BankFinancialStocks(
      totalDeposits: PLN, // total customer deposits (HH + firms)
      firmLoan: PLN,      // outstanding corporate loan book
      govBondAfs: PLN,    // Available-for-Sale gov bonds
      govBondHtm: PLN,    // Held-to-Maturity gov bonds
      reserve: PLN,       // reserves held at NBP
      interbankLoan: PLN, // net interbank position (positive = lender)
      demandDeposit: PLN, // demand deposits
      termDeposit: PLN,   // term deposits
      consumerLoan: PLN,  // outstanding unsecured household credit
  )

  /** Operational state of an individual bank.
    *
    * Financial ownership stocks are intentionally absent; callers must carry
    * the aligned `BankFinancialStocks` row explicitly from
    * LedgerFinancialState.
    */
  case class BankState(
      id: BankId,                                          // unique bank identifier (index into banks vector)
      capital: PLN,                                        // regulatory capital (Tier 1 + retained earnings)
      nplAmount: PLN,                                      // non-performing corporate loan stock (KNF Stage 3)
      htmBookYield: Rate,                                  // weighted-average acquisition yield on HTM portfolio
      status: BankStatus,                                  // operational status (Active with CAR counter, or Failed)
      loansShort: PLN,                                     // short-term corporate-loan maturity bucket (< 1 year)
      loansMedium: PLN,                                    // medium-term corporate-loan maturity bucket (1–5 years)
      loansLong: PLN,                                      // long-term corporate-loan maturity bucket (> 5 years)
      consumerNpl: PLN,                                    // consumer credit NPL stock
      eclStaging: EclStaging.State = EclStaging.State.zero, // IFRS 9 ECL staging (S1/S2/S3)
  ):

    /** Whether this bank has been resolved by BFG. */
    def failed: Boolean = status match
      case BankStatus.Failed(_) => true
      case _                    => false

    /** Month of failure, if this bank has already been resolved. */
    def failedMonth: Option[ExecutionMonth] = status match
      case BankStatus.Failed(m) => Some(m)
      case _                    => None

    /** Consecutive months with CAR below regulatory minimum, or 0 if failed. */
    def consecutiveLowCar: Int = status match
      case BankStatus.Active(c) => c
      case _                    => 0

  def govBondHoldings(stocks: BankFinancialStocks): PLN =
    stocks.govBondAfs + stocks.govBondHtm

  def nplRatio(bank: BankState, stocks: BankFinancialStocks): Share =
    nplRatio(stocks.firmLoan, bank.nplAmount)

  def car(bank: BankState, stocks: BankFinancialStocks, corpBondHoldings: PLN): Multiplier =
    capitalAdequacyRatio(bank.capital, stocks.firmLoan, stocks.consumerLoan, corpBondHoldings)

  def hqla(stocks: BankFinancialStocks): PLN =
    stocks.reserve + govBondHoldings(stocks)

  def netCashOutflows(stocks: BankFinancialStocks)(using p: SimParams): PLN =
    stocks.demandDeposit * p.banking.demandDepositRunoff

  def lcr(stocks: BankFinancialStocks)(using p: SimParams): Multiplier =
    val outflows = netCashOutflows(stocks)
    if outflows > MinBalanceThreshold then hqla(stocks).ratioTo(outflows).toMultiplier
    else SafeRatioFloor

  def asf(bank: BankState, stocks: BankFinancialStocks): PLN =
    bank.capital + stocks.termDeposit * AsfTermWeight + stocks.demandDeposit * AsfDemandWeight

  def rsf(bank: BankState, stocks: BankFinancialStocks, corpBondHoldings: PLN): PLN =
    bank.loansShort * RsfShort + bank.loansMedium * RsfMedium + bank.loansLong * RsfLong +
      govBondHoldings(stocks) * RsfGovBond + corpBondHoldings * RsfCorpBond

  def nsfr(bank: BankState, stocks: BankFinancialStocks, corpBondHoldings: PLN): Multiplier =
    val requiredStableFunding = rsf(bank, stocks, corpBondHoldings)
    if requiredStableFunding > MinBalanceThreshold then asf(bank, stocks).ratioTo(requiredStableFunding).toMultiplier else SafeRatioFloor

  /** State of the entire banking sector. */
  case class State(
      banks: Vector[BankState],
      interbankRate: Rate,
      configs: Vector[Config],
      interbankCurve: Option[YieldCurve.State],
      financialStocks: Vector[BankFinancialStocks] = Vector.empty,
  )

  /** Banking-sector state that belongs to macro/market runtime state, without
    * the explicit bank population.
    */
  case class MarketState(
      interbankRate: Rate,
      configs: Vector[Config],
      interbankCurve: Option[YieldCurve.State],
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
  def assignBank(firmSector: SectorIdx, configs: Vector[Config], rng: RandomStream): BankId =
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
  def lendingRate(bank: BankState, stocks: BankFinancialStocks, cfg: Config, refRate: Rate, bondYield: Rate, corpBondHoldings: PLN)(using
      p: SimParams,
  ): Rate =
    if bank.failed then refRate + FailedBankSpread
    else
      val nplSpread   = (nplRatio(bank, stocks) * p.banking.nplSpreadFactor).toRate.min(NplSpreadCap)
      val carThresh   = p.banking.minCar * CarPenaltyThreshMult
      val bankCar     = car(bank, stocks, corpBondHoldings)
      val carPenalty  =
        if bankCar < carThresh then ((carThresh - bankCar) * CarPenaltyScale).toRate
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
  def interbankRate(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks], refRate: Rate)(using p: SimParams): Rate =
    require(
      banks.length == financialStocks.length,
      s"Banking.interbankRate requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
    val alive       = banks.zip(financialStocks).filterNot(_._1.failed)
    val aggNpl      = alive.map(_._1.nplAmount).sum
    val aggLoans    = alive.map(_._2.firmLoan).sum
    val aggDeposits = alive.map(_._2.totalDeposits).sum
    val aggReserves = alive.map(_._2.reserve).sum

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
  def canLend(bank: BankState, stocks: BankFinancialStocks, amount: PLN, rng: RandomStream, ccyb: Multiplier, corpBondHoldings: PLN)(using
      p: SimParams,
  ): Boolean =
    if bank.failed then false
    else
      val projectedRwa = stocks.firmLoan + stocks.consumerLoan + corpBondHoldings * CorpBondRiskWeight + amount
      val projectedCar = if projectedRwa > MinBalanceThreshold then bank.capital.ratioTo(projectedRwa).toMultiplier else SafeRatioFloor
      val minCar       = Macroprudential.effectiveMinCar(bank.id.toInt, ccyb)
      val carOk        = projectedCar >= minCar
      val lcrOk        = lcr(stocks) >= p.banking.lcrMin
      val nsfrOk       = nsfr(bank, stocks, corpBondHoldings) >= p.banking.nsfrMin
      val nplPenalty   = nplRatio(bank, stocks) * NplApprovalPenalty // Share * Multiplier → Multiplier
      val freeReserves = stocks.totalDeposits * (Share.One - p.banking.reserveReq) - stocks.firmLoan - govBondHoldings(stocks)
      val resPenalty   = if freeReserves > PLN.Zero then Share.Zero else ReserveDeficitPenalty
      val approvalP    = (Share.One - nplPenalty.toShare - resPenalty).max(MinApprovalProb)
      carOk && lcrOk && nsfrOk && approvalP.sampleBelow(rng)

  // ---------------------------------------------------------------------------
  // Interbank market
  // ---------------------------------------------------------------------------

  /** Clear the interbank market: excess reserves → lender/borrower netting.
    * Hoarding factor [0,1] scales lending: 0 = full freeze, 1 = normal.
    */
  def clearInterbank(
      banks: Vector[BankState],
      financialStocks: Vector[BankFinancialStocks],
      configs: Vector[Config],
      hoarding: Share = Share.One,
  )(using
      p: SimParams,
  ): BankStockState =
    require(
      banks.length == financialStocks.length,
      s"Banking.clearInterbank requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
    val excess: Vector[PLN] = banks
      .zip(financialStocks)
      .zip(configs)
      .map { case ((b, stocks), _) =>
        if b.failed then PLN.Zero
        else stocks.totalDeposits * (Share.One - p.banking.reserveReq) - stocks.firmLoan - govBondHoldings(stocks)
      }

    val lenderIdxs     = excess.indices.filter(i => excess(i) > PLN.Zero && !banks(i).failed).toVector
    val borrowerIdxs   = excess.indices.filter(i => excess(i) < PLN.Zero && !banks(i).failed).toVector
    val lenderCaps     = lenderIdxs.map(i => (excess(i) * hoarding).toLong.max(0L))
    val borrowerNeeds  = borrowerIdxs.map(i => (-excess(i)).toLong)
    val totalLending   = lenderCaps.sum
    val totalBorrowing = borrowerNeeds.sum

    if totalLending <= 0L || totalBorrowing <= 0L then BankStockState(banks, financialStocks.map(_.copy(interbankLoan = PLN.Zero, reserve = PLN.Zero)))
    else
      val matched        = math.min(totalLending, totalBorrowing)
      val lenderLoans    = Distribute.distribute(matched, lenderCaps.toArray)
      val borrowerLoans  = Distribute.distribute(matched, borrowerNeeds.toArray)
      val lenderLoanById = lenderIdxs.zip(lenderLoans.iterator).toMap
      val borrowerById   = borrowerIdxs.zip(borrowerLoans.iterator).toMap

      val updatedStocks = banks.indices
        .map: i =>
          val b      = banks(i)
          val stocks = financialStocks(i)
          if b.failed then stocks.copy(interbankLoan = PLN.Zero, reserve = PLN.Zero)
          else
            lenderLoanById.get(i) match
              case Some(lent) =>
                stocks.copy(interbankLoan = PLN.fromRaw(lent), reserve = excess(i) - PLN.fromRaw(lent))
              case None       =>
                borrowerById.get(i) match
                  case Some(borrowed) =>
                    stocks.copy(interbankLoan = PLN.fromRaw(-borrowed), reserve = PLN.Zero)
                  case None           =>
                    stocks.copy(interbankLoan = PLN.Zero, reserve = PLN.Zero)
        .toVector
      BankStockState(banks, updatedStocks)

  // ---------------------------------------------------------------------------
  // Failure detection and resolution
  // ---------------------------------------------------------------------------

  /** Check for bank failures: CAR < effectiveMinCar for 3 consecutive months,
    * or LCR breach at 50% of minimum. Already-failed banks pass through.
    */
  def checkFailures(
      banks: Vector[BankState],
      financialStocks: Vector[BankFinancialStocks],
      month: ExecutionMonth, // execution month (recorded in BankStatus.Failed)
      enabled: Boolean,      // whether failure mechanism is active
      ccyb: Multiplier,      // countercyclical capital buffer
      bankCorpBondHoldings: BankCorpBondHoldings = noBankCorpBondHoldings,
  )(using p: SimParams): FailureCheckResult =
    require(
      banks.length == financialStocks.length,
      s"Banking.checkFailures requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
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
      val updated    = banks
        .zip(financialStocks)
        .map: (b, stocks) =>
          if b.failed then b
          else
            val consec    = b.consecutiveLowCar
            val minCar    = Macroprudential.effectiveMinCar(b.id.toInt, ccyb)
            val lowCar    = car(b, stocks, bankCorpBondHoldings(b.id)) < minCar
            val lcrBreach = lcr(stocks) < p.banking.lcrMin * Share(0.5)
            val newConsec = if lowCar then consec + 1 else 0
            if newConsec >= 3 || lcrBreach then b.copy(status = BankStatus.Failed(month), capital = PLN.Zero)
            else b.copy(status = BankStatus.Active(newConsec))
      val prevFailed = banks.filter(_.failed).map(_.id).toSet
      FailureCheckResult(updated, updated.exists(b => b.failed && !prevFailed.contains(b.id)))

  /** Compute monthly BFG levy for all banks.
    *
    * Failed banks pay no levy. Active banks pay deposits × bfgLevyRate / 12.
    */
  def computeBfgLevy(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks])(using p: SimParams): PerBankAmounts =
    require(
      banks.length == financialStocks.length,
      s"Banking.computeBfgLevy requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
    val perBank = banks
      .zip(financialStocks)
      .map: (b, stocks) =>
        if b.failed then PLN.Zero
        else stocks.totalDeposits * p.banking.bfgLevyRate.monthly
    PerBankAmounts(perBank, perBank.sum)

  /** Bail-in: haircut uninsured deposits on failed banks. Deposits below
    * bfgDepositGuarantee are protected.
    */
  def applyBailIn(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks])(using p: SimParams): BailInResult =
    require(
      banks.length == financialStocks.length,
      s"Banking.applyBailIn requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
    val withHaircut = banks
      .zip(financialStocks)
      .map: (b, stocks) =>
        if b.failed && stocks.totalDeposits > PLN.Zero then
          val guaranteed = stocks.totalDeposits.min(p.banking.bfgDepositGuarantee)
          val uninsured  = stocks.totalDeposits - guaranteed
          val haircut    = uninsured * p.banking.bailInDepositHaircut
          (stocks.copy(totalDeposits = stocks.totalDeposits - haircut), haircut)
        else (stocks, PLN.Zero)
    BailInResult(banks, withHaircut.map(_._1), withHaircut.map(_._2).sum)

  /** BFG P&A resolution: transfer deposits, bonds, performing loans, consumer
    * loans from failed banks to the healthiest surviving bank.
    */
  def resolveFailures(
      banks: Vector[BankState],
      financialStocks: Vector[BankFinancialStocks],
      bankCorpBondHoldings: Vector[PLN] = Vector.empty,
  ): ResolutionResult =
    require(
      banks.length == financialStocks.length,
      s"Banking.resolveFailures requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
    val holderBalances = Vector.tabulate(banks.length)(index => bankCorpBondHoldings.lift(index).getOrElse(PLN.Zero))
    val newlyFailed    = banks.zip(financialStocks).filter((b, stocks) => b.failed && stocks.totalDeposits > PLN.Zero)
    if newlyFailed.isEmpty then ResolutionResult(banks, financialStocks, BankId.NoBank, holderBalances)
    else
      val absorberId = healthiestBankId(banks, financialStocks, bankCorpBondHoldingsFromVector(holderBalances))
      val toAbsorb   = newlyFailed.filter((bank, _) => bank.id != absorberId)
      // Single-pass PLN aggregation of all flows from failed banks (exact Long addition)
      val addDep     = toAbsorb.map(_._2.totalDeposits).sum
      val addLoans   = toAbsorb.map((bank, stocks) => stocks.firmLoan - bank.nplAmount).sum
      val addAfs     = toAbsorb.map(_._2.govBondAfs).sum
      val addHtm     = toAbsorb.map(_._2.govBondHtm).sum
      val addCorpB   = toAbsorb.flatMap((bank, _) => holderBalances.lift(bank.id.toInt)).sum
      val addCC      = toAbsorb.map(_._2.consumerLoan).sum
      val addIB      = toAbsorb.map(_._2.interbankLoan).sum
      // Weighted yield: Σ(htmBonds × htmBookYield) — PLN × Rate → PLN
      val htmYieldWt = toAbsorb.map((bank, stocks) => stocks.govBondHtm * bank.htmBookYield).sum

      val resolvedRows      = banks
        .zip(financialStocks)
        .map: (b, stocks) =>
          if b.id == absorberId then
            val combinedHtm      = stocks.govBondHtm + addHtm
            val combinedHtmYield =
              if combinedHtm > PLN.Zero then Rate((stocks.govBondHtm * b.htmBookYield + htmYieldWt) / combinedHtm)
              else b.htmBookYield
            (
              b.copy(htmBookYield = combinedHtmYield, status = BankStatus.Active(0)),
              stocks.copy(
                totalDeposits = stocks.totalDeposits + addDep,
                firmLoan = (stocks.firmLoan + addLoans).max(PLN.Zero),
                govBondAfs = stocks.govBondAfs + addAfs,
                govBondHtm = combinedHtm,
                consumerLoan = stocks.consumerLoan + addCC,
                interbankLoan = stocks.interbankLoan + addIB,
              ),
            )
          else if b.failed && stocks.totalDeposits > PLN.Zero then
            (
              b.copy(htmBookYield = Rate.Zero, nplAmount = PLN.Zero, consumerNpl = PLN.Zero),
              stocks.copy(
                totalDeposits = PLN.Zero,
                firmLoan = PLN.Zero,
                govBondAfs = PLN.Zero,
                govBondHtm = PLN.Zero,
                reserve = PLN.Zero,
                interbankLoan = PLN.Zero,
                consumerLoan = PLN.Zero,
              ),
            )
          else (b, stocks)
      val resolved          = resolvedRows.map(_._1)
      val resolvedStocks    = resolvedRows.map(_._2)
      val resolvedCorpBonds = resolved.zip(resolvedStocks).zipWithIndex.map { case ((bank, stocks), index) =>
        if bank.id == absorberId then holderBalances(index) + addCorpB
        else if bank.failed && stocks.totalDeposits == PLN.Zero then PLN.Zero
        else holderBalances(index)
      }
      ResolutionResult(resolved, resolvedStocks, absorberId, resolvedCorpBonds)

  /** Find the healthiest (highest CAR) surviving bank. Falls back to highest
    * capital if all banks have failed.
    */
  def healthiestBankId(
      banks: Vector[BankState],
      financialStocks: Vector[BankFinancialStocks],
      bankCorpBondHoldings: BankCorpBondHoldings = noBankCorpBondHoldings,
  ): BankId =
    require(
      banks.length == financialStocks.length,
      s"Banking.healthiestBankId requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
    val alive = banks.zip(financialStocks).filterNot(_._1.failed)
    if alive.isEmpty then banks.maxBy(_.capital.toLong).id
    else alive.maxBy((bank, stocks) => car(bank, stocks, bankCorpBondHoldings(bank.id)))._1.id

  /** Reassign a firm/household from a failed bank to the healthiest surviving
    * bank.
    */
  def reassignBankId(
      currentBankId: BankId,
      banks: Vector[BankState],
      financialStocks: Vector[BankFinancialStocks],
      bankCorpBondHoldings: BankCorpBondHoldings = noBankCorpBondHoldings,
  ): BankId =
    if currentBankId.toInt < banks.length && !banks(currentBankId.toInt).failed then currentBankId
    else healthiestBankId(banks, financialStocks, bankCorpBondHoldings)

  // ---------------------------------------------------------------------------
  // Bond allocation
  // ---------------------------------------------------------------------------

  /** Allocate new bond issuance to banks proportional to deposits using
    * `ledger.Distribute` for exact residual closure.
    */
  def allocateBondIssuance(
      banks: Vector[BankState],
      financialStocks: Vector[BankFinancialStocks],
      issuance: PLN,
      currentYield: Rate,
  )(using p: SimParams): BankStockState =
    if issuance <= PLN.Zero then return BankStockState(banks, financialStocks)
    allocateBondChange(banks, financialStocks, issuance, currentYield)

  /** Allocate bond redemptions to banks proportional to deposits using
    * `ledger.Distribute` for exact residual closure.
    */
  def allocateBondRedemption(
      banks: Vector[BankState],
      financialStocks: Vector[BankFinancialStocks],
      redemption: PLN,
      currentYield: Rate,
  )(using p: SimParams): BankStockState =
    if redemption <= PLN.Zero then return BankStockState(banks, financialStocks)
    allocateBondChange(banks, financialStocks, PLN.fromRaw(-redemption.toLong), currentYield)

  private def allocateBondChange(
      banks: Vector[BankState],
      financialStocks: Vector[BankFinancialStocks],
      signedChange: PLN,
      currentYield: Rate,
  )(using p: SimParams): BankStockState =
    require(
      banks.length == financialStocks.length,
      s"Banking.allocateBondChange requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
    if signedChange == PLN.Zero then return BankStockState(banks, financialStocks)
    val aliveRows = banks.zip(financialStocks).filterNot(_._1.failed)
    if aliveRows.isEmpty then return BankStockState(banks, financialStocks)

    val weights =
      val nonNegativeDeposits = aliveRows.map((_, stocks) => stocks.totalDeposits.toLong.max(0L))
      val totalDep            = nonNegativeDeposits.sum
      if totalDep > 0L then nonNegativeDeposits.toArray
      else Array.fill(aliveRows.length)(1L)

    val magnitudes = Distribute.distribute(math.abs(signedChange.toLong), weights)
    val signedById = aliveRows
      .zip(magnitudes.iterator)
      .map { case ((b, _), amount) =>
        val signedAmount = if signedChange.toLong > 0L then PLN.fromRaw(amount) else PLN.fromRaw(-amount)
        b.id -> signedAmount
      }
      .toMap

    val updatedRows = banks.zip(financialStocks).map { case (b, stocks) =>
      signedById.get(b.id).fold((b, stocks))(amount => applyBondAllocation(b, stocks, amount, currentYield))
    }
    BankStockState(updatedRows.map(_._1), updatedRows.map(_._2))

  private def applyBondAllocation(b: BankState, stocks: BankFinancialStocks, amount: PLN, currentYield: Rate)(using
      p: SimParams,
  ): (BankState, BankFinancialStocks) =
    if amount > PLN.Zero then
      // Issuance: split per htmShare, update book yield as weighted average
      val htmPortion   = amount * p.banking.htmShare
      val afsPortion   = amount - htmPortion
      val newHtmTotal  = stocks.govBondHtm + htmPortion
      val newBookYield =
        if newHtmTotal > PLN.Zero then Rate((stocks.govBondHtm * b.htmBookYield + htmPortion * currentYield) / newHtmTotal)
        else b.htmBookYield
      (b.copy(htmBookYield = newBookYield), stocks.copy(govBondAfs = stocks.govBondAfs + afsPortion, govBondHtm = newHtmTotal))
    else if amount < PLN.Zero then
      // Redemption: reduce both proportionally (no floor — matches original behavior)
      val total = govBondHoldings(stocks)
      if total <= PLN.Zero then
        (
          b,
          stocks.copy(
            govBondAfs = stocks.govBondAfs + amount * Share(0.5),
            govBondHtm = stocks.govBondHtm + amount * Share(0.5),
          ),
        )
      else
        val afsFrac   = stocks.govBondAfs.ratioTo(total).toShare
        val afsReduce = amount * afsFrac
        val htmReduce = amount - afsReduce
        (b, stocks.copy(govBondAfs = stocks.govBondAfs + afsReduce, govBondHtm = stocks.govBondHtm + htmReduce))
    else (b, stocks)

  /** Result of a bond sale from banks to a single buyer. */
  case class BondSaleResult(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks], actualSold: PLN)

  /** Remove bonds from banks proportional to holdings, transferring to a buyer.
    * Returns updated banks and the actual PLN sold (may be less than requested
    * if banks lack sufficient holdings). Sells AFS first; spills into HTM. SFC
    * invariant: actualSold leaves banks = actualSold arrives at buyer.
    */
  def sellToBuyer(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks], requested: PLN): BondSaleResult =
    require(
      banks.length == financialStocks.length,
      s"Banking.sellToBuyer requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
    if requested <= PLN.Zero then BondSaleResult(banks, financialStocks, PLN.Zero)
    else
      val eligible   = banks.zip(financialStocks).filter((b, stocks) => !b.failed && govBondHoldings(stocks) > PLN.Zero)
      val totalBonds = eligible.map((_, stocks) => govBondHoldings(stocks)).sum
      if totalBonds <= PLN.Zero then BondSaleResult(banks, financialStocks, PLN.Zero)
      else
        val requestedSale  = requested.min(totalBonds)
        val soldMagnitudes = Distribute.distribute(requestedSale.toLong, eligible.map((_, stocks) => govBondHoldings(stocks).toLong).toArray)
        val soldById       = eligible
          .zip(soldMagnitudes.iterator)
          .map { case ((b, _), sold) =>
            b.id -> PLN.fromRaw(sold)
          }
          .toMap
        val resultStocks   = banks.zip(financialStocks).map { case (b, stocks) =>
          soldById.get(b.id).fold(stocks) { sold =>
            val afsReduce = sold.min(stocks.govBondAfs)
            val htmReduce = sold - afsReduce
            stocks.copy(
              govBondAfs = (stocks.govBondAfs - afsReduce).max(PLN.Zero),
              govBondHtm = (stocks.govBondHtm - htmReduce).max(PLN.Zero),
            )
          }
        }
        BondSaleResult(banks, resultStocks, requestedSale)

  // ---------------------------------------------------------------------------
  // HTM forced reclassification (interest rate risk)
  // ---------------------------------------------------------------------------

  /** Result of HTM forced reclassification across all banks. */
  case class HtmForcedSaleResult(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks], totalRealizedLoss: PLN)

  /** Forcibly reclassify HTM bonds to AFS when LCR drops below
    * `htmForcedSaleThreshold × lcrMin`. On reclassification the hidden
    * mark-to-market loss is realized: realizedLoss = reclassified × duration ×
    * max(currentYield − htmBookYield, 0) Total `govBondHoldings` is unchanged —
    * only the AFS/HTM split moves.
    */
  def processHtmForcedSale(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks], currentYield: Rate)(using
      p: SimParams,
  ): HtmForcedSaleResult =
    require(
      banks.length == financialStocks.length,
      s"Banking.processHtmForcedSale requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
    val threshold = p.banking.htmForcedSaleThreshold * p.banking.lcrMin
    var totalLoss = PLN.Zero
    val updated   = banks
      .zip(financialStocks)
      .map: (b, stocks) =>
        if b.failed || stocks.govBondHtm <= PLN.Zero || lcr(stocks) >= threshold then (b, stocks)
        else
          val reclassified = stocks.govBondHtm * p.banking.htmForcedSaleRate
          val yieldGap     = (currentYield - b.htmBookYield).max(Rate.Zero)
          val loss         = reclassified * yieldGap * p.banking.govBondDuration
          totalLoss = totalLoss + loss
          (b.copy(capital = b.capital - loss), stocks.copy(govBondHtm = stocks.govBondHtm - reclassified, govBondAfs = stocks.govBondAfs + reclassified))
    HtmForcedSaleResult(updated.map(_._1), updated.map(_._2), totalLoss)

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
  def reserveInterest(bank: BankState, stocks: BankFinancialStocks, refRate: Rate)(using p: SimParams): PLN =
    if bank.failed || stocks.reserve <= PLN.Zero then PLN.Zero
    else stocks.reserve * (refRate * p.monetary.reserveRateMult.toMultiplier).monthly

  /** Reserve interest for all banks → per-bank amounts + sector total. */
  def computeReserveInterest(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks], refRate: Rate)(using
      SimParams,
  ): PerBankAmounts =
    computeReserveInterestFromBankStocks(banks, financialStocks, refRate)

  def computeReserveInterestFromBankStocks(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks], refRate: Rate)(using
      SimParams,
  ): PerBankAmounts =
    require(
      banks.length == financialStocks.length,
      s"Banking.computeReserveInterestFromBankStocks requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
    val perBank = banks.zip(financialStocks).map((bank, stocks) => reserveInterest(bank, stocks, refRate))
    PerBankAmounts(perBank, perBank.sum)

  /** Standing facility flows (monthly): deposit rate for excess reserves,
    * lombard rate for borrowers. Always-on — the NBP corridor (ref ± 100 bps)
    * is structural, not optional.
    */
  def computeStandingFacilities(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks], refRate: Rate)(using
      p: SimParams,
  ): PerBankAmounts =
    computeStandingFacilitiesFromBankStocks(banks, financialStocks, refRate)

  def computeStandingFacilitiesFromBankStocks(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks], refRate: Rate)(using
      p: SimParams,
  ): PerBankAmounts =
    require(
      banks.length == financialStocks.length,
      s"Banking.computeStandingFacilitiesFromBankStocks requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
    val depositRate = (refRate - p.monetary.depositFacilitySpread).max(Rate.Zero)
    val lombardRate = refRate + p.monetary.lombardSpread
    val perBank     = banks
      .zip(financialStocks)
      .map: (bank, stocks) =>
        if bank.failed then PLN.Zero
        else if stocks.reserve > PLN.Zero then stocks.reserve * depositRate.monthly
        else if stocks.interbankLoan < PLN.Zero then -(stocks.interbankLoan.abs * lombardRate.monthly)
        else PLN.Zero
    PerBankAmounts(perBank, perBank.sum)

  /** Interbank interest flows (monthly). Net zero in aggregate (closed system).
    */
  def interbankInterestFlows(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks], rate: Rate): PerBankAmounts =
    interbankInterestFlowsFromBankStocks(banks, financialStocks, rate)

  def interbankInterestFlowsFromBankStocks(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks], rate: Rate): PerBankAmounts =
    require(
      banks.length == financialStocks.length,
      s"Banking.interbankInterestFlowsFromBankStocks requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
    val perBank = banks
      .zip(financialStocks)
      .map: (bank, stocks) =>
        if bank.failed then PLN.Zero
        else stocks.interbankLoan * rate.monthly
    PerBankAmounts(perBank, perBank.sum)
