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

  def aggregateFromBanks(
      banks: Vector[BankState],
      bankCorpBondHoldings: BankCorpBondHoldings = noBankCorpBondHoldings,
  ): Aggregate =
    aggregateFromBankStocks(banks, banks.map(_.financial), bankCorpBondHoldings)

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

    def compute(banks: Vector[BankState], tfiAum: PLN, corpBondOutstanding: PLN): MonetaryAggregates =
      computeFromBankStocks(banks, banks.map(_.financial), tfiAum, corpBondOutstanding)

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

  /** Result of monthly failure check. */
  case class FailureCheckResult(banks: Vector[BankState], anyFailed: Boolean)

  /** Result of BRRD bail-in on newly failed banks. */
  case class BailInResult(banks: Vector[BankState], totalLoss: PLN)

  /** Result of BFG purchase-and-assumption resolution. */
  case class ResolutionResult(banks: Vector[BankState], absorberId: BankId, bankCorpBondHoldings: Vector[PLN] = Vector.empty)

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

  /** Ledger-contracted bank financial stocks currently carried through banking
    * execution.
    *
    * This is the bank-sector slice that maps to `LedgerFinancialState.banks`.
    * It is kept as one value object so `BankState` can stop owning these stocks
    * as separate fields while the remaining banking algorithms are migrated.
    */
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

  /** State of an individual bank (updated each month).
    *
    * `financial` is the transitional execution-local mirror of the
    * ledger-contracted bank stocks. The other fields are banking policy,
    * credit-quality, maturity, and operational state that are not represented
    * as independent ledger ownership positions.
    */
  case class BankState(
      id: BankId,                                          // unique bank identifier (index into banks vector)
      financial: BankFinancialStocks,                      // ledger-contracted stocks mirrored during banking execution
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

    def deposits: PLN       = financial.totalDeposits
    def loans: PLN          = financial.firmLoan
    def afsBonds: PLN       = financial.govBondAfs
    def htmBonds: PLN       = financial.govBondHtm
    def reservesAtNbp: PLN  = financial.reserve
    def interbankNet: PLN   = financial.interbankLoan
    def demandDeposits: PLN = financial.demandDeposit
    def termDeposits: PLN   = financial.termDeposit
    def consumerLoans: PLN  = financial.consumerLoan

    def withFinancial(update: BankFinancialStocks => BankFinancialStocks): BankState =
      copy(financial = update(financial))

    /** Total government bond holdings (AFS + HTM). All existing read-only
      * references (hqla, rsf, canLend, etc.) use this derived method.
      */
    def govBondHoldings: PLN = Banking.govBondHoldings(financial)

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

    /** Non-performing loan ratio: NPL / loans. Returns Share.Zero when loan
      * book is empty.
      */
    def nplRatio: Share =
      Banking.nplRatio(this, financial)

    /** Capital adequacy ratio: capital / risk-weighted assets (Basel III).
      * Corporate bonds are ledger-owned, so callers must pass this bank's
      * current corporate-bond holdings explicitly.
      */
    def car(corpBondHoldings: PLN): Multiplier =
      Banking.car(this, financial, corpBondHoldings)

    /** High-quality liquid assets: reserves + gov bonds (Basel III Level 1). */
    def hqla: PLN = Banking.hqla(financial)

    /** Net cash outflows (30-day): demand deposits × runoff rate. */
    def netCashOutflows(using p: SimParams): PLN = Banking.netCashOutflows(financial)

    /** Liquidity Coverage Ratio = HQLA / net cash outflows (Basel III). */
    def lcr(using p: SimParams): Multiplier =
      Banking.lcr(financial)

    /** Available Stable Funding (Basel III NSFR numerator). */
    def asf: PLN = Banking.asf(this, financial)

    /** Required Stable Funding (Basel III NSFR denominator). */
    def rsf(corpBondHoldings: PLN): PLN =
      Banking.rsf(this, financial, corpBondHoldings)

    /** Net Stable Funding Ratio = ASF / RSF. */
    def nsfr(corpBondHoldings: PLN): Multiplier =
      Banking.nsfr(this, financial, corpBondHoldings)

  def withFinancialStocks(banks: Vector[BankState], financialStocks: Vector[BankFinancialStocks]): Vector[BankState] =
    require(
      banks.length == financialStocks.length,
      s"Banking.withFinancialStocks requires aligned banks and financial stocks, got ${banks.length} banks and ${financialStocks.length} stock rows",
    )
    banks.zip(financialStocks).map((bank, stocks) => bank.copy(financial = stocks))

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
  def lendingRate(bank: BankState, cfg: Config, refRate: Rate, bondYield: Rate, corpBondHoldings: PLN)(using p: SimParams): Rate =
    if bank.failed then refRate + FailedBankSpread
    else
      val nplSpread   = (bank.nplRatio * p.banking.nplSpreadFactor).toRate.min(NplSpreadCap)
      val carThresh   = p.banking.minCar * CarPenaltyThreshMult
      val bankCar     = bank.car(corpBondHoldings)
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
  def interbankRate(banks: Vector[BankState], refRate: Rate)(using p: SimParams): Rate =
    val alive       = banks.filterNot(_.failed)
    val aggNpl      = alive.map(_.nplAmount).sum
    val aggLoans    = alive.map(_.loans).sum
    val aggDeposits = alive.map(_.deposits).sum
    val aggReserves = alive.map(_.reservesAtNbp).sum

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
  def canLend(bank: BankState, amount: PLN, rng: RandomStream, ccyb: Multiplier, corpBondHoldings: PLN)(using p: SimParams): Boolean =
    if bank.failed then false
    else
      val projectedRwa = bank.loans + bank.consumerLoans + corpBondHoldings * CorpBondRiskWeight + amount
      val projectedCar = if projectedRwa > MinBalanceThreshold then bank.capital.ratioTo(projectedRwa).toMultiplier else SafeRatioFloor
      val minCar       = Macroprudential.effectiveMinCar(bank.id.toInt, ccyb)
      val carOk        = projectedCar >= minCar
      val lcrOk        = bank.lcr >= p.banking.lcrMin
      val nsfrOk       = bank.nsfr(corpBondHoldings) >= p.banking.nsfrMin
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

    if totalLending <= 0L || totalBorrowing <= 0L then banks.map(_.withFinancial(_.copy(interbankLoan = PLN.Zero, reserve = PLN.Zero)))
    else
      val matched        = math.min(totalLending, totalBorrowing)
      val lenderLoans    = Distribute.distribute(matched, lenderCaps.toArray)
      val borrowerLoans  = Distribute.distribute(matched, borrowerNeeds.toArray)
      val lenderLoanById = lenderIdxs.zip(lenderLoans.iterator).toMap
      val borrowerById   = borrowerIdxs.zip(borrowerLoans.iterator).toMap

      banks.indices
        .map: i =>
          val b = banks(i)
          if b.failed then b.withFinancial(_.copy(interbankLoan = PLN.Zero, reserve = PLN.Zero))
          else
            lenderLoanById.get(i) match
              case Some(lent) =>
                b.withFinancial(_.copy(interbankLoan = PLN.fromRaw(lent), reserve = excess(i) - PLN.fromRaw(lent)))
              case None       =>
                borrowerById.get(i) match
                  case Some(borrowed) =>
                    b.withFinancial(_.copy(interbankLoan = PLN.fromRaw(-borrowed), reserve = PLN.Zero))
                  case None           =>
                    b.withFinancial(_.copy(interbankLoan = PLN.Zero, reserve = PLN.Zero))
        .toVector

  // ---------------------------------------------------------------------------
  // Failure detection and resolution
  // ---------------------------------------------------------------------------

  /** Check for bank failures: CAR < effectiveMinCar for 3 consecutive months,
    * or LCR breach at 50% of minimum. Already-failed banks pass through.
    */
  def checkFailures(
      banks: Vector[BankState],
      month: ExecutionMonth, // execution month (recorded in BankStatus.Failed)
      enabled: Boolean,      // whether failure mechanism is active
      ccyb: Multiplier,      // countercyclical capital buffer
      bankCorpBondHoldings: BankCorpBondHoldings = noBankCorpBondHoldings,
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
          val lowCar    = b.car(bankCorpBondHoldings(b.id)) < minCar
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
    PerBankAmounts(perBank, perBank.sum)

  /** Bail-in: haircut uninsured deposits on failed banks. Deposits below
    * bfgDepositGuarantee are protected.
    */
  def applyBailIn(banks: Vector[BankState])(using p: SimParams): BailInResult =
    val withHaircut = banks.map: b =>
      if b.failed && b.deposits > PLN.Zero then
        val guaranteed = b.deposits.min(p.banking.bfgDepositGuarantee)
        val uninsured  = b.deposits - guaranteed
        val haircut    = uninsured * p.banking.bailInDepositHaircut
        (b.withFinancial(_.copy(totalDeposits = b.deposits - haircut)), haircut)
      else (b, PLN.Zero)
    BailInResult(withHaircut.map(_._1), withHaircut.map(_._2).sum)

  /** BFG P&A resolution: transfer deposits, bonds, performing loans, consumer
    * loans from failed banks to the healthiest surviving bank.
    */
  def resolveFailures(banks: Vector[BankState], bankCorpBondHoldings: Vector[PLN] = Vector.empty): ResolutionResult =
    val holderBalances = Vector.tabulate(banks.length)(index => bankCorpBondHoldings.lift(index).getOrElse(PLN.Zero))
    val newlyFailed    = banks.filter(b => b.failed && b.deposits > PLN.Zero)
    if newlyFailed.isEmpty then ResolutionResult(banks, BankId.NoBank, holderBalances)
    else
      val absorberId = healthiestBankId(banks, bankCorpBondHoldingsFromVector(holderBalances))
      val toAbsorb   = newlyFailed.filter(_.id != absorberId)
      // Single-pass PLN aggregation of all flows from failed banks (exact Long addition)
      val addDep     = toAbsorb.map(_.deposits).sum
      val addLoans   = toAbsorb.map(f => f.loans - f.nplAmount).sum
      val addAfs     = toAbsorb.map(_.afsBonds).sum
      val addHtm     = toAbsorb.map(_.htmBonds).sum
      val addCorpB   = toAbsorb.flatMap(bank => holderBalances.lift(bank.id.toInt)).sum
      val addCC      = toAbsorb.map(_.consumerLoans).sum
      val addIB      = toAbsorb.map(_.interbankNet).sum
      // Weighted yield: Σ(htmBonds × htmBookYield) — PLN × Rate → PLN
      val htmYieldWt = toAbsorb.map(f => f.htmBonds * f.htmBookYield).sum

      val resolved          = banks.map: b =>
        if b.id == absorberId then
          val combinedHtm      = b.htmBonds + addHtm
          val combinedHtmYield =
            if combinedHtm > PLN.Zero then Rate((b.htmBonds * b.htmBookYield + htmYieldWt) / combinedHtm)
            else b.htmBookYield
          b
            .withFinancial(
              _.copy(
                totalDeposits = b.deposits + addDep,
                firmLoan = (b.loans + addLoans).max(PLN.Zero),
                govBondAfs = b.afsBonds + addAfs,
                govBondHtm = combinedHtm,
                consumerLoan = b.consumerLoans + addCC,
                interbankLoan = b.interbankNet + addIB,
              ),
            )
            .copy(
              htmBookYield = combinedHtmYield,
              status = BankStatus.Active(0),
            )
        else if b.failed && b.deposits > PLN.Zero then
          b
            .withFinancial(
              _.copy(
                totalDeposits = PLN.Zero,
                firmLoan = PLN.Zero,
                govBondAfs = PLN.Zero,
                govBondHtm = PLN.Zero,
                reserve = PLN.Zero,
                interbankLoan = PLN.Zero,
                consumerLoan = PLN.Zero,
              ),
            )
            .copy(
              htmBookYield = Rate.Zero,
              nplAmount = PLN.Zero,
              consumerNpl = PLN.Zero,
            )
        else b
      val resolvedCorpBonds = resolved.zipWithIndex.map: (bank, index) =>
        if bank.id == absorberId then holderBalances(index) + addCorpB
        else if bank.failed && bank.deposits == PLN.Zero then PLN.Zero
        else holderBalances(index)
      ResolutionResult(resolved, absorberId, resolvedCorpBonds)

  /** Find the healthiest (highest CAR) surviving bank. Falls back to highest
    * capital if all banks have failed.
    */
  def healthiestBankId(banks: Vector[BankState], bankCorpBondHoldings: BankCorpBondHoldings = noBankCorpBondHoldings): BankId =
    val alive = banks.filterNot(_.failed)
    if alive.isEmpty then banks.maxBy(_.capital.toLong).id
    else alive.maxBy(bank => bank.car(bankCorpBondHoldings(bank.id))).id

  /** Reassign a firm/household from a failed bank to the healthiest surviving
    * bank.
    */
  def reassignBankId(currentBankId: BankId, banks: Vector[BankState], bankCorpBondHoldings: BankCorpBondHoldings = noBankCorpBondHoldings): BankId =
    if currentBankId.toInt < banks.length && !banks(currentBankId.toInt).failed then currentBankId
    else healthiestBankId(banks, bankCorpBondHoldings)

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
      b.withFinancial(_.copy(govBondAfs = b.afsBonds + afsPortion, govBondHtm = newHtmTotal))
        .copy(htmBookYield = newBookYield)
    else if amount < PLN.Zero then
      // Redemption: reduce both proportionally (no floor — matches original behavior)
      val total = b.govBondHoldings
      if total <= PLN.Zero then
        b.withFinancial(
          _.copy(
            govBondAfs = b.afsBonds + amount * Share(0.5),
            govBondHtm = b.htmBonds + amount * Share(0.5),
          ),
        )
      else
        val afsFrac   = b.afsBonds.ratioTo(total).toShare
        val afsReduce = amount * afsFrac
        val htmReduce = amount - afsReduce
        b.withFinancial(_.copy(govBondAfs = b.afsBonds + afsReduce, govBondHtm = b.htmBonds + htmReduce))
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
      val totalBonds = eligible.map(_.govBondHoldings).sum
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
            b.withFinancial(
              _.copy(
                govBondAfs = (b.afsBonds - afsReduce).max(PLN.Zero),
                govBondHtm = (b.htmBonds - htmReduce).max(PLN.Zero),
              ),
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
        val loss         = reclassified * yieldGap * p.banking.govBondDuration
        totalLoss = totalLoss + loss
        b.withFinancial(_.copy(govBondHtm = b.htmBonds - reclassified, govBondAfs = b.afsBonds + reclassified))
          .copy(capital = b.capital - loss)
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
    reserveInterest(bank, bank.financial, refRate)

  def reserveInterest(bank: BankState, stocks: BankFinancialStocks, refRate: Rate)(using p: SimParams): PLN =
    if bank.failed || stocks.reserve <= PLN.Zero then PLN.Zero
    else stocks.reserve * (refRate * p.monetary.reserveRateMult.toMultiplier).monthly

  /** Reserve interest for all banks → per-bank amounts + sector total. */
  def computeReserveInterest(banks: Vector[BankState], refRate: Rate)(using SimParams): PerBankAmounts =
    val perBank = banks.map(b => reserveInterest(b, refRate))
    PerBankAmounts(perBank, perBank.sum)

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
  def computeStandingFacilities(banks: Vector[BankState], refRate: Rate)(using p: SimParams): PerBankAmounts =
    computeStandingFacilitiesFromBankStocks(banks, banks.map(_.financial), refRate)

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
  def interbankInterestFlows(banks: Vector[BankState], rate: Rate): PerBankAmounts =
    interbankInterestFlowsFromBankStocks(banks, banks.map(_.financial), rate)

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
