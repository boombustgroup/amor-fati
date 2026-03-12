package sfc.agents

import sfc.config.SimParams
import sfc.engine.*
import sfc.engine.mechanisms.{Macroprudential, YieldCurve}
import sfc.types.*
import sfc.util.KahanSum.*

import scala.util.Random

object Banking:

  // ---------------------------------------------------------------------------
  // Named constants (Basel III / KNF calibration)
  // ---------------------------------------------------------------------------

  /** Corp bond risk weight (Basel III, BBB bucket). */
  private val CorpBondRiskWeight = 0.50

  /** Well-capitalised floor for CAR/LCR/NSFR when denominator ≤ threshold. */
  private val SafeRatioFloor = Ratio(10.0)

  /** Minimum balance threshold to avoid division by zero. */
  private val MinBalanceThreshold = 1.0

  // NSFR weights (Basel III §6)
  private val AsfTermWeight   = 0.95
  private val AsfDemandWeight = 0.90
  private val RsfShort        = 0.50
  private val RsfMedium       = 0.65
  private val RsfLong         = 0.85
  private val RsfGovBond      = 0.05
  private val RsfCorpBond     = 0.50

  // Lending rate components
  private val FailedBankSpread     = 0.50
  private val NplSpreadCap         = 0.15
  private val CarPenaltyThreshMult = 1.5
  private val CarPenaltyScale      = 2.0

  // Credit approval
  private val MinApprovalProb    = 0.1
  private val NplApprovalPenalty = 3.0

  // Interbank corridor
  private val DepositSpreadFromRef = 0.01
  private val LombardSpreadFromRef = 0.01

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
      govBondHoldings: PLN, // Treasury bond portfolio (skarbowe papiery wartościowe)
      consumerLoans: PLN,   // Outstanding unsecured household credit
      consumerNpl: PLN,     // Non-performing consumer loan stock
      corpBondHoldings: PLN, // Corporate bond portfolio — bank share only (default 30%, CORPBOND_BANK_SHARE)
  ):
    /** Non-performing loan ratio: nplAmount / totalLoans. Returns Ratio.Zero
      * when loan book is empty.
      */
    def nplRatio: Ratio = if totalLoans.toDouble > 1.0 then Ratio(nplAmount / totalLoans) else Ratio.Zero

    /** Capital adequacy ratio: capital / risk-weighted assets. Corporate bonds
      * carry 50% risk weight (Basel III, BBB bucket). Returns Ratio(10.0)
      * (well-capitalised floor) when risk-weighted assets ≤ 1 to avoid division
      * by zero.
      */
    def car: Ratio =
      val totalRwa = (totalLoans + consumerLoans + corpBondHoldings * 0.50).toDouble
      if totalRwa > 1.0 then Ratio(capital.toDouble / totalRwa) else Ratio(10.0)

  // ---------------------------------------------------------------------------
  // Monetary aggregates (diagnostic, not SFC-relevant)
  // ---------------------------------------------------------------------------

  /** Monetary aggregates — diagnostic, not SFC-relevant. */
  case class MonetaryAggregates(
      m1: PLN,                 // Bank deposits (≈ narrow money)
      monetaryBase: PLN,       // Reserves at NBP
      creditMultiplier: Double, // m1 / monetaryBase
  )
  object MonetaryAggregates:
    val zero: MonetaryAggregates = MonetaryAggregates(PLN.Zero, PLN.Zero, 0)

    def compute(deposits: PLN, reserves: PLN): MonetaryAggregates =
      val base = Math.max(1.0, reserves.toDouble)
      MonetaryAggregates(deposits, reserves, deposits.toDouble / base)

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
      initMarketShare: Ratio,       // deposit-weighted share at t = 0
      initCet1: Ratio,              // initial CET1 ratio (KNF 2024)
      lendingSpread: Rate,          // bank-specific spread over base lending rate
      sectorAffinity: Vector[Ratio], // relative lending preference per sector
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
      id: BankId,           // unique bank identifier (index into banks vector)
      deposits: PLN,        // total customer deposits (HH + firms)
      loans: PLN,           // outstanding corporate loan book
      capital: PLN,         // regulatory capital (Tier 1 + retained earnings)
      nplAmount: PLN,       // non-performing loan stock (KNF Stage 3)
      govBondHoldings: PLN, // treasury bond portfolio (skarbowe papiery wartościowe)
      reservesAtNbp: PLN,   // excess reserves held at NBP
      interbankNet: PLN,    // net interbank position (positive = lender)
      status: BankStatus,   // operational status (Active with CAR counter, or Failed)
      demandDeposits: PLN,  // demand deposits (% split from total deposits)
      termDeposits: PLN,    // term deposits
      loansShort: PLN,      // short-term loans (< 1 year)
      loansMedium: PLN,     // medium-term loans (1–5 years)
      loansLong: PLN,       // long-term loans (> 5 years)
      consumerLoans: PLN,   // outstanding unsecured household credit
      consumerNpl: PLN,     // consumer credit NPL stock
      corpBondHoldings: PLN, // corporate bond holdings (bank share, Basel III BBB)
  ):

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

    /** Non-performing loan ratio: NPL / loans. Returns Ratio.Zero when loan
      * book is empty.
      */
    def nplRatio: Ratio =
      if loans.toDouble > MinBalanceThreshold then Ratio(nplAmount / loans) else Ratio.Zero

    /** Capital adequacy ratio: capital / risk-weighted assets (Basel III). Corp
      * bonds carry [[CorpBondRiskWeight]] risk weight (BBB bucket). Returns
      * [[SafeRatioFloor]] when RWA ≤ [[MinBalanceThreshold]].
      */
    def car: Ratio =
      val totalRwa = loans + consumerLoans + corpBondHoldings * CorpBondRiskWeight
      if totalRwa.toDouble > MinBalanceThreshold then Ratio(capital / totalRwa) else SafeRatioFloor

    /** High-quality liquid assets: reserves + gov bonds (Basel III Level 1). */
    def hqla: PLN = reservesAtNbp + govBondHoldings

    /** Net cash outflows (30-day): demand deposits × runoff rate. */
    def netCashOutflows(using p: SimParams): PLN = demandDeposits * p.banking.demandDepositRunoff.toDouble

    /** Liquidity Coverage Ratio = HQLA / net cash outflows (Basel III). */
    def lcr(using p: SimParams): Ratio =
      if netCashOutflows.toDouble > MinBalanceThreshold then Ratio(hqla / netCashOutflows)
      else SafeRatioFloor

    /** Available Stable Funding (Basel III NSFR numerator). */
    def asf: PLN = capital + termDeposits * AsfTermWeight + demandDeposits * AsfDemandWeight

    /** Required Stable Funding (Basel III NSFR denominator). */
    def rsf: PLN =
      loansShort * RsfShort + loansMedium * RsfMedium + loansLong * RsfLong +
        govBondHoldings * RsfGovBond + corpBondHoldings * RsfCorpBond

    /** Net Stable Funding Ratio = ASF / RSF. */
    def nsfr: Ratio =
      if rsf.toDouble > MinBalanceThreshold then Ratio(asf / rsf) else SafeRatioFloor

  /** State of the entire banking sector. */
  case class State(
      banks: Vector[BankState],
      interbankRate: Rate,
      configs: Vector[Config],
      interbankCurve: Option[YieldCurve.State],
  ):
    def aggregate: Aggregate =
      Aggregate(
        totalLoans = PLN(banks.kahanSumBy(_.loans.toDouble)),
        nplAmount = PLN(banks.kahanSumBy(_.nplAmount.toDouble)),
        capital = PLN(banks.kahanSumBy(_.capital.toDouble)),
        deposits = PLN(banks.kahanSumBy(_.deposits.toDouble)),
        govBondHoldings = PLN(banks.kahanSumBy(_.govBondHoldings.toDouble)),
        consumerLoans = PLN(banks.kahanSumBy(_.consumerLoans.toDouble)),
        consumerNpl = PLN(banks.kahanSumBy(_.consumerNpl.toDouble)),
        corpBondHoldings = PLN(banks.kahanSumBy(_.corpBondHoldings.toDouble)),
      )

  // ---------------------------------------------------------------------------
  // Default configs (7 Polish banks, KNF 2024)
  // ---------------------------------------------------------------------------

  private def affinity(xs: Double*): Vector[Ratio] = xs.map(Ratio(_)).toVector

  val DefaultConfigs: Vector[Config] = Vector(
    Config(BankId(0), "PKO BP", Ratio(0.175), Ratio(0.185), Rate(-0.002), affinity(0.15, 0.15, 0.15, 0.10, 0.30, 0.15)),
    Config(BankId(1), "Pekao", Ratio(0.120), Ratio(0.178), Rate(-0.001), affinity(0.15, 0.20, 0.20, 0.15, 0.15, 0.15)),
    Config(BankId(2), "mBank", Ratio(0.085), Ratio(0.169), Rate(0.000), affinity(0.30, 0.10, 0.25, 0.10, 0.10, 0.15)),
    Config(BankId(3), "ING BSK", Ratio(0.075), Ratio(0.172), Rate(-0.001), affinity(0.15, 0.35, 0.15, 0.10, 0.10, 0.15)),
    Config(BankId(4), "Santander", Ratio(0.070), Ratio(0.170), Rate(0.000), affinity(0.15, 0.10, 0.35, 0.15, 0.10, 0.15)),
    Config(BankId(5), "BPS/Coop", Ratio(0.050), Ratio(0.150), Rate(0.003), affinity(0.05, 0.10, 0.10, 0.05, 0.05, 0.65)),
    Config(BankId(6), "Others", Ratio(0.425), Ratio(0.165), Rate(0.001), affinity(0.15, 0.17, 0.17, 0.17, 0.17, 0.17)),
  )

  // ---------------------------------------------------------------------------
  // Bank assignment
  // ---------------------------------------------------------------------------

  /** Assign a firm to a bank based on sector affinity and market share. */
  def assignBank(firmSector: SectorIdx, configs: Vector[Config], rng: Random): BankId =
    val weights = configs.map(c => c.sectorAffinity(firmSector.toInt).toDouble * c.initMarketShare.toDouble)
    val total   = weights.kahanSum
    if total <= 0.0 then BankId(0)
    else
      val r      = rng.nextDouble() * total
      val cumul  = weights.scanLeft(0.0)(_ + _).tail // cumulative sums
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

  /** Lending rate for a bank: refRate + baseSpread + bankSpread + nplSpread +
    * carPenalty. Failed banks get flat refRate + FailedBankSpread.
    */
  def lendingRate(bank: BankState, cfg: Config, refRate: Rate)(using p: SimParams): Rate =
    if bank.failed then refRate + Rate(FailedBankSpread)
    else
      val nplSpread  = Math.min(NplSpreadCap, (bank.nplRatio * p.banking.nplSpreadFactor).toDouble)
      val carPenalty =
        if bank.car.toDouble < p.banking.minCar.toDouble * CarPenaltyThreshMult then
          Math.max(0.0, (p.banking.minCar.toDouble * CarPenaltyThreshMult - bank.car.toDouble) * CarPenaltyScale)
        else 0.0
      refRate + p.banking.baseSpread + cfg.lendingSpread + Rate(nplSpread + carPenalty)

  /** Interbank rate (WIBOR proxy): deposit rate + stress × (lombard − deposit).
    * stress = aggNplRate / stressThreshold, clipped to [0,1].
    */
  def interbankRate(banks: Vector[BankState], refRate: Rate)(using p: SimParams): Rate =
    val alive       = banks.filterNot(_.failed)
    val aggNpl      = alive.kahanSumBy(_.nplAmount.toDouble)
    val aggLoans    = alive.kahanSumBy(_.loans.toDouble)
    val aggNplRate  = if aggLoans > MinBalanceThreshold then aggNpl / aggLoans else 0.0
    val stress      = Math.max(0.0, Math.min(1.0, aggNplRate / p.banking.stressThreshold.toDouble))
    val depositRate = Math.max(0.0, refRate.toDouble - DepositSpreadFromRef)
    val lombardRate = refRate.toDouble + LombardSpreadFromRef
    Rate(depositRate + stress * (lombardRate - depositRate))

  // ---------------------------------------------------------------------------
  // Credit approval
  // ---------------------------------------------------------------------------

  /** Can this bank lend `amount`? Checks projected CAR, LCR/NSFR, and
    * stochastic approval probability penalised by NPL ratio.
    */
  def canLend(bank: BankState, amount: PLN, rng: Random, ccyb: Rate)(using p: SimParams): Boolean =
    if bank.failed then false
    else
      val projectedCar =
        bank.capital.toDouble / (bank.loans.toDouble + bank.consumerLoans.toDouble + bank.corpBondHoldings.toDouble * CorpBondRiskWeight + amount.toDouble)
      val approvalP    = Math.max(MinApprovalProb, 1.0 - (bank.nplRatio * NplApprovalPenalty).toDouble)
      val minCar       = Macroprudential.effectiveMinCar(bank.id.toInt, ccyb.toDouble)
      val carOk        = projectedCar >= minCar
      val lcrOk        = if p.flags.bankLcr then bank.lcr.toDouble >= p.banking.lcrMin else true
      val nsfrOk       = if p.flags.bankLcr then bank.nsfr.toDouble >= p.banking.nsfrMin else true
      carOk && lcrOk && nsfrOk && rng.nextDouble() < approvalP

  // ---------------------------------------------------------------------------
  // Interbank market
  // ---------------------------------------------------------------------------

  /** Clear the interbank market: excess reserves → lender/borrower netting. */
  def clearInterbank(banks: Vector[BankState], configs: Vector[Config])(using
      p: SimParams,
  ): Vector[BankState] =
    val excess = banks
      .zip(configs)
      .map: (b, _) =>
        if b.failed then 0.0
        else (b.deposits * (1.0 - p.banking.reserveReq.toDouble) - b.loans - b.govBondHoldings).toDouble

    val totalLending   = excess.filter(_ > 0).kahanSum
    val totalBorrowing = -excess.filter(_ < 0).kahanSum

    if totalLending <= 0 || totalBorrowing <= 0 then banks.map(_.copy(interbankNet = PLN.Zero, reservesAtNbp = PLN.Zero))
    else
      val scale = Math.min(1.0, totalLending / totalBorrowing)
      banks
        .zip(excess)
        .map: (b, ex) =>
          if b.failed then b.copy(interbankNet = PLN.Zero, reservesAtNbp = PLN.Zero)
          else if ex > 0 then
            val lent = ex * Math.min(1.0, totalBorrowing / totalLending)
            b.copy(interbankNet = PLN(lent), reservesAtNbp = PLN(ex - lent))
          else if ex < 0 then
            val borrowed = -ex * scale
            b.copy(interbankNet = PLN(-borrowed), reservesAtNbp = PLN.Zero)
          else b.copy(interbankNet = PLN.Zero, reservesAtNbp = PLN.Zero)

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
      ccyb: Rate,       // countercyclical capital buffer
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
          val minCar    = Macroprudential.effectiveMinCar(b.id.toInt, ccyb.toDouble)
          val lowCar    = b.car.toDouble < minCar
          val lcrBreach = p.flags.bankLcr && b.lcr.toDouble < p.banking.lcrMin * 0.5
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
      else b.deposits * p.banking.bfgLevyRate.toDouble / 12.0
    PerBankAmounts(perBank, PLN(perBank.map(_.toDouble).kahanSum))

  /** Bail-in: haircut uninsured deposits on failed banks. Deposits below
    * bfgDepositGuarantee are protected. No-op when flags.bailIn is false.
    */
  def applyBailIn(banks: Vector[BankState])(using p: SimParams): BailInResult =
    if !p.flags.bailIn then BailInResult(banks, PLN.Zero)
    else
      val withHaircut = banks.map: b =>
        if b.failed && b.deposits > PLN.Zero then
          val guaranteed = b.deposits.min(PLN(p.banking.bfgDepositGuarantee.toDouble))
          val uninsured  = b.deposits - guaranteed
          val haircut    = uninsured * p.banking.bailInDepositHaircut.toDouble
          (b.copy(deposits = b.deposits - haircut), haircut)
        else (b, PLN.Zero)
      BailInResult(withHaircut.map(_._1), PLN(withHaircut.map(_._2.toDouble).kahanSum))

  /** BFG P&A resolution: transfer deposits, bonds, performing loans, consumer
    * loans from failed banks to the healthiest surviving bank.
    */
  def resolveFailures(banks: Vector[BankState]): ResolutionResult =
    val newlyFailed = banks.filter(b => b.failed && b.deposits > PLN.Zero)
    if newlyFailed.isEmpty then ResolutionResult(banks, BankId.NoBank)
    else
      val absorberId                                           = healthiestBankId(banks)
      val toAbsorb                                             = newlyFailed.filter(_.id != absorberId)
      // Single-pass aggregation of all flows from failed banks
      val (addDep, addLoans, addBonds, addCorpB, addCC, addIB) =
        toAbsorb.foldLeft((0.0, 0.0, 0.0, 0.0, 0.0, 0.0)):
          case ((dep, ln, bd, cb, cc, ib), f) =>
            (
              dep + f.deposits.toDouble,
              ln + (f.loans - f.nplAmount).toDouble,
              bd + f.govBondHoldings.toDouble,
              cb + f.corpBondHoldings.toDouble,
              cc + f.consumerLoans.toDouble,
              ib + f.interbankNet.toDouble,
            )
      val resolved                                             = banks.map: b =>
        if b.id == absorberId then
          b.copy(
            deposits = b.deposits + PLN(addDep),
            loans = b.loans + PLN(addLoans).max(PLN.Zero),
            govBondHoldings = b.govBondHoldings + PLN(addBonds),
            corpBondHoldings = b.corpBondHoldings + PLN(addCorpB),
            consumerLoans = b.consumerLoans + PLN(addCC),
            interbankNet = b.interbankNet + PLN(addIB),
            status = BankStatus.Active(0),
          )
        else if b.failed && b.deposits > PLN.Zero then
          b.copy(
            deposits = PLN.Zero,
            loans = PLN.Zero,
            govBondHoldings = PLN.Zero,
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
    if alive.isEmpty then banks.maxBy(_.capital.toDouble).id
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

  /** Allocate bond issuance/redemption to banks proportional to deposits. Last
    * alive bank absorbs the residual for exact SFC closure.
    */
  def allocateBonds(banks: Vector[BankState], deficit: PLN): Vector[BankState] =
    if deficit == PLN.Zero then return banks
    val aliveBanks  = banks.filterNot(_.failed)
    val nAlive      = aliveBanks.length
    if nAlive == 0 then return banks
    val totalDep    = aliveBanks.kahanSumBy(_.deposits.toDouble)
    val lastAliveId = aliveBanks.last.id
    val (result, _) = banks.foldLeft((Vector.empty[BankState], PLN.Zero)):
      case ((acc, allocated), b) =>
        if b.failed then (acc :+ b, allocated)
        else if b.id == lastAliveId then (acc :+ b.copy(govBondHoldings = b.govBondHoldings + (deficit - allocated)), deficit)
        else
          val share  = if totalDep > 0 then b.deposits.toDouble / totalDep else 1.0 / nAlive
          val amount = deficit * share
          (acc :+ b.copy(govBondHoldings = b.govBondHoldings + amount), allocated + amount)
    result

  /** Allocate QE purchases from banks proportional to their bond holdings.
    * Capped at each bank's available holdings.
    */
  def allocateQePurchases(banks: Vector[BankState], qeTotal: PLN): Vector[BankState] =
    if qeTotal <= PLN.Zero then banks
    else
      val eligible   = banks.filter(b => !b.failed && b.govBondHoldings > PLN.Zero)
      val totalBonds = eligible.kahanSumBy(_.govBondHoldings.toDouble)
      if totalBonds <= 0 then banks
      else
        val lastEligibleId = eligible.last.id
        val (result, _)    = banks.foldLeft((Vector.empty[BankState], PLN.Zero)):
          case ((acc, allocated), b) =>
            if b.failed || b.govBondHoldings <= PLN.Zero then (acc :+ b, allocated)
            else if b.id == lastEligibleId then
              val sold = b.govBondHoldings.min(qeTotal - allocated)
              (acc :+ b.copy(govBondHoldings = b.govBondHoldings - sold), allocated + sold)
            else
              val share = b.govBondHoldings.toDouble / totalBonds
              val sold  = b.govBondHoldings.min(qeTotal * share)
              (acc :+ b.copy(govBondHoldings = b.govBondHoldings - sold), allocated + sold)
        result

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
    val retain    = p.banking.profitRetention.toDouble
    val losses    =
      in.nplLoss.toDouble + in.mortgageNplLoss.toDouble + in.consumerNplLoss.toDouble + in.corpBondDefaultLoss.toDouble + in.bfgLevy.toDouble
    val retIncome = (in.intIncome.toDouble + in.hhDebtService.toDouble + in.bondIncome.toDouble - in.depositInterest.toDouble
      + in.reserveInterest.toDouble + in.standingFacilityIncome.toDouble + in.interbankInterest.toDouble
      + in.mortgageInterestIncome.toDouble + in.consumerDebtService.toDouble + in.corpBondCoupon.toDouble) * retain
    CapitalPnlOutput(newCapital = PLN(in.prevCapital.toDouble - losses + retIncome))

  // ---------------------------------------------------------------------------
  // Monetary plumbing
  // ---------------------------------------------------------------------------

  /** Monthly reserve interest for a single bank: reserves × refRate × mult / 12.
    */
  def reserveInterest(bank: BankState, refRate: Rate)(using p: SimParams): PLN =
    if bank.failed || bank.reservesAtNbp <= PLN.Zero then PLN.Zero
    else bank.reservesAtNbp * refRate.toDouble * p.monetary.reserveRateMult.toDouble / 12.0

  /** Reserve interest for all banks → per-bank amounts + sector total. */
  def computeReserveInterest(banks: Vector[BankState], refRate: Rate)(using SimParams): PerBankAmounts =
    val perBank = banks.map(b => reserveInterest(b, refRate))
    PerBankAmounts(perBank, PLN(perBank.map(_.toDouble).kahanSum))

  /** Standing facility flows (monthly): deposit rate for excess reserves,
    * lombard rate for borrowers. No-op when flags.nbpStandingFacilities is
    * false.
    */
  def computeStandingFacilities(banks: Vector[BankState], refRate: Rate)(using p: SimParams): PerBankAmounts =
    if !p.flags.nbpStandingFacilities then PerBankAmounts(banks.map(_ => PLN.Zero), PLN.Zero)
    else
      val depositRate = Math.max(0.0, (refRate - p.monetary.depositFacilitySpread).toDouble)
      val lombardRate = refRate + p.monetary.lombardSpread
      val perBank     = banks.map: b =>
        if b.failed then PLN.Zero
        else if b.reservesAtNbp > PLN.Zero then b.reservesAtNbp * depositRate / 12.0
        else if b.interbankNet < PLN.Zero then -(b.interbankNet.abs * lombardRate.monthly)
        else PLN.Zero
      PerBankAmounts(perBank, PLN(perBank.map(_.toDouble).kahanSum))

  /** Interbank interest flows (monthly). Net zero in aggregate (closed system).
    */
  def interbankInterestFlows(banks: Vector[BankState], rate: Rate): PerBankAmounts =
    val perBank = banks.map: b =>
      if b.failed then PLN.Zero
      else b.interbankNet * rate.toDouble / 12.0
    PerBankAmounts(perBank, PLN(perBank.map(_.toDouble).kahanSum))
