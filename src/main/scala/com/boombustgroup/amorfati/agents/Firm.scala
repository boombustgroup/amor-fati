package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.{OperationalSignals, World}
import com.boombustgroup.amorfati.fp.FixedPointBase.bankerRound
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

// ---- Domain types ----

/** Reason a firm exited the simulation — carried in `TechState.Bankrupt` and
  * `Decision.UpgradeFailed`.
  */
sealed trait BankruptReason
object BankruptReason:
  case object AiDebtTrap          extends BankruptReason
  case object HybridInsolvency    extends BankruptReason
  case object AiImplFailure       extends BankruptReason
  case object HybridImplFailure   extends BankruptReason
  case object LaborCostInsolvency extends BankruptReason
  case class Other(msg: String)   extends BankruptReason

/** Technology regime of a firm. Determines worker count, capacity, and cost
  * structure.
  */
sealed trait TechState
object TechState:
  case class Traditional(workers: Int)                      extends TechState
  case class Hybrid(workers: Int, aiEfficiency: Multiplier) extends TechState
  case class Automated(efficiency: Multiplier)              extends TechState
  case class Bankrupt(reason: BankruptReason)               extends TechState

/** Firm agent: stateless functions operating on `State`. Entry point:
  * `process`.
  */
object Firm:

  final case class HiringDiagnostics(
      workers: Int,
      desiredWorkers: Int,
      feasibleWorkers: Int,
      desiredGap: Int,
      feasibleGap: Int,
      hiringThreshold: Int,
      firingThreshold: Int,
      shouldAdjust: Boolean,
      proposedAdjustment: Int,
      proposedWorkers: Int,
      signalMonths: Int,
      requiredSignalMonths: Int,
  )

  // ---- Calibration constants ----
  // Named here rather than inline. Candidates for SimParams if they need
  // to vary across experiments.

  // Financing splits: fraction of CAPEX financed by loan vs down payment
  private val FullAiLoanShare      = 0.85 // full-AI: 85% loan, 15% down
  private val FullAiDownShare      = 0.15
  private val HybridLoanShare      = 0.80 // hybrid: 80% loan, 20% down
  private val HybridDownShare      = 0.20
  private val HybridToFullCapexMul = 0.6  // hybrid→full-AI upgrade costs 60% of greenfield

  // Profitability thresholds: upgrade must be this much cheaper than status quo
  private val FullAiProfitMargin = 1.1  // full-AI must save ≥10%
  private val HybridProfitMargin = 1.05 // hybrid must save ≥5%

  // Adoption probability weights
  private val RiskWeightFullAi     = 0.1   // risk profile weight in full-AI adoption
  private val RiskWeightHybrid     = 0.04  // risk profile weight in hybrid adoption
  private val RiskWeightHybUpgrade = 0.15  // risk profile weight when hybrid→full-AI
  private val AutoRatioWeight      = 0.3   // automation ratio weight when hybrid→full-AI
  private val LocalPanicWeight     = 0.4   // local neighbor automation pressure
  private val GlobalPanicWeight    = 0.4   // global automation pressure
  private val HybridPanicDiscount  = 0.5   // hybrid counts 50% in global panic
  private val DesperationBonus     = 0.2   // bonus prob when firm is loss-making
  private val StrategicAdoptBase   = 0.005 // strategic adoption when AI not yet profitable

  // Baseline willingness ramp for automation adoption.
  private val UncertaintyBase  = 0.15 // initial willingness multiplier
  private val UncertaintySlope = 0.15 // additional willingness after ramp saturation

  // Implementation failure rates
  private val FullAiBaseFailRate   = 0.05 // base failure prob for full-AI upgrade
  private val FullAiFailDrSens     = 0.10 // additional failure from low digital readiness
  private val HybridBaseFailRate   = 0.03 // base failure prob for hybrid upgrade
  private val HybridFailDrSens     = 0.07 // additional failure from low digital readiness
  private val CatastrophicFailFrac = 0.4  // fraction of failures that are catastrophic (vs partial)

  // Partial cost on failed upgrade: firm absorbs fraction of planned CAPEX/loan/down
  private val FailCapexFrac = 0.5
  private val FailLoanFrac  = 0.3
  private val FailDownFrac  = 0.5

  // Efficiency draw ranges on successful upgrade
  private val HybToFullEffMin    = 0.2  // hybrid→full-AI efficiency range
  private val HybToFullEffMax    = 0.6
  private val TradToFullEffMin   = 0.05 // traditional→full-AI efficiency range
  private val TradToFullEffMax   = 0.6
  private val BadHybridEffBase   = 0.85 // partial-failure hybrid efficiency base
  private val BadHybridEffRange  = 0.20 // partial-failure hybrid efficiency range
  private val GoodHybridEffBase  = 0.05 // good hybrid efficiency base boost
  private val GoodHybridEffRange = 0.15 // good hybrid efficiency range
  private val GoodHybridDrBlend  = 0.5  // DR contribution weight in good hybrid efficiency

  // Labor adjustment: firm converges toward MR=MC optimal headcount
  private val LaborAdjustFrac                   = 0.10 // fraction of gap closed per month (smooth, no overshoot)
  private val WorkingCapitalGraceMonths         = 1.5  // tolerate short-lived cash gaps for otherwise profitable firms
  private val HiringWorkingCapitalMonths        = 1.0  // allow modest short-term hiring financed by one wage-month buffer
  private val StartupHiringWorkingCapitalMonths = 2.0  // startups get a slightly longer hiring runway while staffing up
  private val StartupRunwayCashShare            = 0.35 // entrants may burn part of startup cash during the startup window
  private val StartupDownsizeSpeedMultiplier    = 0.5  // startups should cut headcount more cautiously than incumbents
  private val StartupRunwayMonths               = 4    // should match entrant startup ramp-up window
  private val StartupCostFloor                  = 0.50 // entrants ramp operating overhead in gradually
  private val MicroFirmHiringThreshold          = 1    // micro firms should react to a +1 worker gap
  private val HiringSignalPersistenceMonths     = 2    // non-micro firms require sustained demand before hiring
  private val SmallFirmDesiredAddCap            = 1    // firms up to 10 workers plan at most +1 worker per month
  private val MidFirmDesiredAddCap              = 2    // firms up to 25 workers plan at most +2 workers per month
  private val LargeFirmDesiredGrowthShare       = 0.05 // larger firms expand plans gradually rather than jumping to MR=MC headcount
  private val HiringPressureBlend               = 0.35 // uncapped sector pressure is informative but should not dominate planning
  private val NegativeCashHiringPenalty         = 0.5  // cash-negative but profitable firms scale back desired hiring materially

  // Capacity blend: hybrid production = labor share + AI share
  private val HybridLaborCapShare = 0.4
  private val HybridAiCapShare    = 0.6

  // Opex domestic/import split: 60% domestic (price-sensitive), 40% imported
  private val OpexDomesticShare = 0.60
  private val OpexImportShare   = 0.40

  // CAPEX/opex size scaling exponents
  private val CapexSizeExponent = 0.6  // CAPEX scales sublinearly (economies of scale)
  private val OpexSizeExponent  = 0.5  // opex/digi-invest scales sublinearly
  private val SkeletonCrewFrac  = 0.02 // automated firm retains 2% of initial headcount

  // Digital readiness
  private val HybridMonthlyDrDrift = 0.005 // hybrid firms gain DR passively each month
  private val DigiInvestCashMult   = 2.0   // must have 2× digi-invest cost in cash to afford

  // sigma threshold formula (sigmaThreshold function)
  private val SigmaThreshBase  = 0.88
  private val SigmaThreshScale = 0.075

  // ---- Data types ----

  /** Ledger-contracted firm financial stocks passed into firm execution by the
    * ledger boundary.
    *
    * Corporate bonds stay in `LedgerFinancialState` because issuance,
    * absorption, and default settlement happen outside individual
    * `Firm.process`.
    */
  case class FinancialStocks(
      cash: PLN,     // cash or deposit-like liquidity owned by the firm
      firmLoan: PLN, // outstanding bank-loan principal owed by the firm
      equity: PLN,   // listed equity issued by the firm
  )
  object FinancialStocks:
    val zero: FinancialStocks = FinancialStocks(PLN.Zero, PLN.Zero, PLN.Zero)

  /** Operational state of a single firm, carried across simulation months.
    * Financial ownership lives in `LedgerFinancialState` and enters firm
    * execution explicitly as `FinancialStocks`.
    */
  case class State(
      id: FirmId,                          // Unique firm identifier (index into firms vector)
      tech: TechState,                     // Current technology regime
      riskProfile: Share,                  // Propensity to invest / adopt technology [0,1]
      innovationCostFactor: Multiplier,    // Firm-specific CAPEX multiplier (drawn at creation)
      digitalReadiness: Share,             // Digital readiness score [0,1], gates tech upgrades
      sector: SectorIdx,                   // Index into p.sectorDefs
      neighbors: Vector[FirmId],           // Network adjacency (firm IDs)
      bankId: BankId,                      // Multi-bank: index into the explicit bank vector
      initialSize: Int,                    // Firm size at creation (heterogeneous when FIRM_SIZE_DIST=gus)
      capitalStock: PLN,                   // Physical capital stock (PLN)
      foreignOwned: Boolean,               // FDI: subject to profit shifting & repatriation
      stateOwned: Boolean = false,         // SOE: Skarb Państwa ownership (dividend/employment/investment policy)
      inventory: PLN,                      // Inventory stock (PLN)
      greenCapital: PLN,                   // Green capital stock (PLN)
      accumulatedLoss: PLN,                // CIT loss carryforward stock (Art. 7 ustawy o CIT)
      markup: Multiplier = Multiplier.One, // Calvo pricing: firm-specific markup over marginal cost
      region: Region = Region.Central,     // NUTS-1 macroregion
      startupMonthsLeft: Int = 0,          // startup grace/ramp-up window for new entrants
      startupTargetWorkers: Int = 0,       // small team size targeted during startup phase
      startupFilledWorkers: Int = 0,       // employed workers currently filling the startup team
      hiringSignalMonths: Int = 0,         // consecutive months with positive desired hiring gap
  )

  /** Output of `process` for one firm in one month — updated state + flow
    * variables.
    */
  case class Result(
      firm: State,                      // Updated firm state after this month
      financialStocks: FinancialStocks, // Closing ledger-contracted financial stocks
      taxPaid: PLN,                     // CIT actually paid (after informal evasion)
      realizedPostTaxProfit: PLN,       // realized monthly profit after tax, floored at zero for payout logic
      capexSpent: PLN,                  // Technology upgrade CAPEX (AI or hybrid)
      techImports: PLN,                 // Import content of CAPEX (forex demand)
      newLoan: PLN,                     // New bank loan taken for upgrade
      equityIssuance: PLN,              // GPW equity raised this month (filled by S4)
      grossInvestment: PLN,             // Physical capital investment this month
      bondIssuance: PLN,                // Corporate bond issuance (filled by S4)
      profitShiftCost: PLN,             // FDI profit shifting outflow
      fdiRepatriation: PLN,             // FDI dividend repatriation outflow
      inventoryChange: PLN,             // Net inventory change (+ accumulation, - drawdown)
      citEvasion: PLN,                  // CIT evaded via informal economy
      energyCost: PLN,                  // Total energy + ETS cost this month
      greenInvestment: PLN,             // Green capital investment this month
      principalRepaid: PLN,             // Monthly firm loan principal repayment
  )
  object Result:
    /** Convenience factory for tests — all flow fields set to `PLN.Zero`. */
    def zero(firm: State, financialStocks: FinancialStocks = FinancialStocks.zero): Result =
      Result(
        firm,
        financialStocks,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
      )

  /** Monthly profit-and-loss breakdown, computed by `computePnL`. */
  case class PnL(
      revenue: PLN,           // Gross revenue (capacity × demand × price)
      costs: PLN,             // Total costs including profit shifting
      tax: PLN,               // CIT after loss carryforward offset
      netAfterTax: PLN,       // Profit minus tax (can be negative)
      profitShiftCost: PLN,   // FDI profit shifting cost (zero if not foreign-owned)
      energyCost: PLN,        // Energy + ETS carbon surcharge
      newAccumulatedLoss: PLN, // Updated loss carryforward stock for next month
  )
  object PnL:
    val zero: PnL = PnL(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  /** Intermediate ADT from `decide` → `execute`. Separates stochastic choices
    * from state mutation.
    */
  sealed trait Decision
  object Decision:
    case object StayBankrupt                                                                                                  extends Decision
    case class Survive(pnl: PnL, newCash: PLN, drUpdate: Option[Share] = None)                                                extends Decision
    case class GoBankrupt(pnl: PnL, cash: PLN, reason: BankruptReason)                                                        extends Decision
    case class Upgrade(pnl: PnL, newTech: TechState, capex: PLN, loan: PLN, downPayment: PLN, drUpdate: Option[Share] = None) extends Decision
    case class UpgradeFailed(pnl: PnL, reason: BankruptReason, capex: PLN, loan: PLN, down: PLN)                              extends Decision
    case class Downsize(pnl: PnL, newWorkers: Int, adjustedCash: PLN, newTech: TechState, drUpdate: Option[Share] = None)     extends Decision
    case class Upsize(pnl: PnL, newWorkers: Int, newCash: PLN, newTech: TechState)                                            extends Decision
    case class DigiInvest(pnl: PnL, cost: PLN, newDR: Share)                                                                  extends Decision

  // ---- Queries ----

  /** True unless the firm is in `Bankrupt` tech state. */
  def isAlive(f: State): Boolean = f.tech match
    case _: TechState.Bankrupt => false
    case _                     => true

  /** Headcount: workers for Traditional/Hybrid, skeleton crew for Automated, 0
    * for Bankrupt.
    */
  def workerCount(f: State)(using SimParams): Int = f.tech match
    case TechState.Traditional(w) => w
    case TechState.Hybrid(w, _)   => w
    case _: TechState.Automated   => skeletonCrew(f)
    case _: TechState.Bankrupt    => 0

  /** Skeleton crew for automated firms — scales with firm size. */
  def skeletonCrew(f: State)(using p: SimParams): Int =
    Math.max(p.firm.autoSkeletonCrew, (f.initialSize * SkeletonCrewFrac).toInt)

  def isInStartup(f: State): Boolean = f.startupMonthsLeft > 0

  /** Effective wage multiplier including union wage premium. */
  def effectiveWageMult(sectorIdx: SectorIdx)(using p: SimParams): Multiplier =
    val base = p.sectorDefs(sectorIdx.toInt).wageMultiplier
    base + base * (p.labor.unionWagePremium * p.labor.unionDensity(sectorIdx.toInt))

  /** Monthly production capacity in PLN. Scales with tech, sector, firm size;
    * augmented by physical capital via CES production function when enabled.
    *
    * CES: Y = A × [α·K^ρ + (1-α)·L^ρ]^(1/ρ) where ρ = (σ-1)/σ. High-σ sectors
    * (BPO=50) substitute K/L easily; low-σ (Public=1) resist.
    */
  def computeCapacity(f: State)(using p: SimParams): PLN =
    val sec       = p.sectorDefs(f.sector.toInt)
    val sizeScale = Scalar.fraction(f.initialSize, p.pop.workersPerFirm).toMultiplier
    val laborEff  = f.tech match
      case TechState.Traditional(w) => Scalar.fraction(w, f.initialSize).toMultiplier
      case TechState.Hybrid(w, eff) =>
        Share(HybridLaborCapShare).toMultiplier * Scalar.fraction(w, f.initialSize).toMultiplier + Share(HybridAiCapShare).toMultiplier * eff
      case TechState.Automated(eff) => eff
      case _: TechState.Bankrupt    => Multiplier.Zero
    val tfp       = sizeScale * sec.revenueMultiplier
    if f.capitalStock > PLN.Zero && laborEff > Multiplier.Zero then
      val targetK: PLN  = workerCount(f) * p.capital.klRatios(f.sector.toInt)
      val k: Multiplier =
        (if targetK > PLN.Zero then f.capitalStock.ratioTo(targetK).toMultiplier else Multiplier.One).clamp(Multiplier(0.1), Multiplier(2.0))
      val alpha: Share  = p.capital.prodElast
      p.firm.baseRevenue * tfp * cesOutput(alpha, k, laborEff, sec.sigma)
    else p.firm.baseRevenue * tfp * laborEff

  /** CES aggregator: [α·K^ρ + (1-α)·L^ρ]^(1/ρ). Degrades gracefully: σ→1 ≈
    * Cobb-Douglas, σ→∞ ≈ linear (perfect substitutes).
    */
  private[amorfati] def cesOutput(alpha: Share, k: Multiplier, l: Multiplier, sigma: Sigma): Multiplier =
    if !(sigma > Sigma(1.001)) then // near-Leontief/Cobb-Douglas boundary — use Cobb-Douglas
      k.pow(alpha.toScalar) * l.pow((Share.One - alpha).toScalar)
    else
      val rho   = (sigma.toScalar - Scalar.One).ratioTo(sigma.toScalar)
      val kTerm = alpha * k.pow(rho)
      val lTerm = (Share.One - alpha) * l.pow(rho)
      (kTerm + lTerm).pow(rho.reciprocal)

  /** Desired worker count from a one-period MR≈MC comparison.
    *
    * Numerically searches for the headcount where adding one more worker would
    * cost more (wage) than the revenue gained (∂capacity × demand × price).
    * Bounded by [minWorkersRetained, 3 × initialSize], then compressed by an
    * economy-wide labor-slack factor when aggregate plans exceed available
    * labor supply.
    */
  private def desiredWorkers(f: State, w: World, operationalSignals: OperationalSignals)(using p: SimParams): Int =
    if isInStartup(f) then return Math.max(workerCount(f), f.startupTargetWorkers)
    val sectorDemand = operationalSignals.sectorDemandMult(f.sector.toInt)
    val hiringSignal = operationalSignals.sectorHiringSignal(f.sector.toInt)
    val demandMult   = sectorDemand + (hiringSignal - sectorDemand).max(Multiplier.Zero) * Share(HiringPressureBlend)
    val price        = w.priceLevel.toMultiplier
    val wage         = w.householdMarket.marketWage * effectiveWageMult(f.sector)
    val minW         = p.firm.minWorkersRetained
    val maxW         = f.initialSize * 3

    // Binary search: find largest w where marginal revenue > marginal cost
    var lo = minW; var hi = maxW
    while lo < hi do
      val mid     = (lo + hi + 1) / 2
      val capMid  = computeCapacity(f.copy(tech = TechState.Traditional(mid)))
      val capPrev = computeCapacity(f.copy(tech = TechState.Traditional(mid - 1)))
      val mr      = (capMid - capPrev) * (demandMult * price)
      if mr > wage then lo = mid else hi = mid - 1
    applyOperationalHiringSlack(lo, minW, operationalSignals.operationalHiringSlack.toMultiplier)

  private def monthlyHiringHeadroom(workers: Int): Int =
    if workers <= 5 then 1
    else if workers <= 10 then SmallFirmDesiredAddCap
    else if workers <= 25 then MidFirmDesiredAddCap
    else Math.max(MidFirmDesiredAddCap, Math.ceil(workers * LargeFirmDesiredGrowthShare).toInt)

  private def requiredHiringSignalMonths(workers: Int): Int =
    if workers <= 5 then 1 else HiringSignalPersistenceMonths

  private def nextHiringSignalMonths(firm: State, desired: Int, workers: Int): Int =
    if desired > workers then firm.hiringSignalMonths + 1 else 0

  private def feasibleWorkers(
      firm: State,
      workers: Int,
      desired: Int,
      pnl: PnL,
      cashAfterDecision: PLN,
  )(using p: SimParams): Int =
    if desired <= workers then desired
    else
      val signalMonths  = nextHiringSignalMonths(firm, desired, workers)
      val persistenceOk = signalMonths >= requiredHiringSignalMonths(workers) || isInStartup(firm)
      if !persistenceOk then workers
      else
        val headroom                = monthlyHiringHeadroom(workers)
        val structurallyConstrained = Math.min(desired, workers + headroom)
        val liquidityConstrained    =
          if firm.stateOwned || cashAfterDecision >= PLN.Zero then structurallyConstrained
          else if isInStartup(firm) && cashAfterDecision.abs <= startupRunwayLimit(firm) then structurallyConstrained
          else if pnl.netAfterTax > PLN.Zero then workers + Math.max(1, Math.floor(headroom * NegativeCashHiringPenalty).toInt)
          else workers
        Math.max(workers, liquidityConstrained)

  private[amorfati] def applyOperationalHiringSlack(rawTarget: Int, minWorkers: Int, slackFactor: Multiplier): Int =
    Math.max(minWorkers, bankerRound(BigInt(rawTarget.toLong) * BigInt(slackFactor.clamp(Multiplier.Zero, Multiplier.One).toLong)).toInt)

  private[amorfati] def hiringDiagnostics(
      firm: State,
      financialStocks: FinancialStocks,
      w: World,
      operationalSignals: OperationalSignals,
  )(using p: SimParams): HiringDiagnostics =
    val workers         = workerCount(firm)
    val desiredW        = desiredWorkers(firm, w, operationalSignals)
    val nc              = financialStocks.cash
    val feasibleW       = feasibleWorkers(firm, workers, desiredW, PnL.zero, nc)
    val desiredGap      = desiredW - workers
    val feasibleGap     = feasibleW - workers
    val hiringThresh    = if workers <= 5 then MicroFirmHiringThreshold else Math.max(2, (workers * 0.10).toInt)
    val firingThresh    = Math.max(2, (workers * 0.10).toInt)
    val shouldAdjust    = if feasibleGap > 0 then feasibleGap >= hiringThresh else -feasibleGap >= firingThresh
    val proposedAdjust  =
      if shouldAdjust then Math.max(1, (Math.abs(feasibleGap) * LaborAdjustFrac).toInt) * (if feasibleGap > 0 then 1 else -1)
      else 0
    val proposedWorkers = (workers + proposedAdjust).max(p.firm.minWorkersRetained)
    HiringDiagnostics(
      workers = workers,
      desiredWorkers = desiredW,
      feasibleWorkers = feasibleW,
      desiredGap = desiredGap,
      feasibleGap = feasibleGap,
      hiringThreshold = hiringThresh,
      firingThreshold = firingThresh,
      shouldAdjust = shouldAdjust,
      proposedAdjustment = proposedAdjust,
      proposedWorkers = proposedWorkers,
      signalMonths = nextHiringSignalMonths(firm, desiredW, workers),
      requiredSignalMonths = requiredHiringSignalMonths(workers),
    )

  /** Effective AI CAPEX for sector — sublinear in firm size (exponent 0.6),
    * digital readiness discount.
    */
  def computeAiCapex(f: State)(using p: SimParams): PLN =
    val sizeFactor   = Scalar.fraction(f.initialSize, p.pop.workersPerFirm).pow(Scalar(CapexSizeExponent)).toMultiplier
    val digiDiscount = Share.One - p.firm.digiCapexDiscount * f.digitalReadiness
    p.firm.aiCapex * p.sectorDefs(f.sector.toInt).aiCapexMultiplier * digiDiscount * (f.innovationCostFactor * sizeFactor)

  /** Hybrid upgrade CAPEX — same scaling as AI CAPEX but using hybrid
    * multipliers.
    */
  def computeHybridCapex(f: State)(using p: SimParams): PLN =
    val sizeFactor   = Scalar.fraction(f.initialSize, p.pop.workersPerFirm).pow(Scalar(CapexSizeExponent)).toMultiplier
    val digiDiscount = Share.One - p.firm.digiCapexDiscount * f.digitalReadiness
    p.firm.hybridCapex * p.sectorDefs(f.sector.toInt).hybridCapexMultiplier * digiDiscount * (f.innovationCostFactor * sizeFactor)

  /** Digital investment cost — sublinear in firm size (exponent 0.5). */
  def computeDigiInvestCost(f: State)(using p: SimParams): PLN =
    val sizeFactor = Scalar.fraction(f.initialSize, p.pop.workersPerFirm).pow(Scalar(OpexSizeExponent)).toMultiplier
    p.firm.digiInvestCost * sizeFactor

  /** Fraction of a firm's network neighbors that have adopted automation
    * (Automated or Hybrid tech).
    *
    * Used in technology adoption decisions: firms with more automated neighbors
    * face stronger competitive pressure to digitalize (network externality /
    * peer effect). Returns 0.0 for firms with no neighbors (isolates).
    */
  def computeLocalAutoRatio(firm: Firm.State, firms: Vector[Firm.State]): Share =
    val neighbors = firm.neighbors
    if neighbors.isEmpty then return Share.Zero
    val autoCount = neighbors.count: nid =>
      val nf = firms(nid.toInt)
      nf.tech.isInstanceOf[TechState.Automated] || nf.tech.isInstanceOf[TechState.Hybrid]
    Share.fraction(autoCount, neighbors.length)

  /** sigma-based threshold modifier: high sigma sectors find automation
    * profitable at lower cost gap. Only used for profitability threshold, NOT
    * for probability multiplier. Mapping: sigma=2->0.91, sigma=5->0.95,
    * sigma=10->0.98, sigma=50->1.00 At equilibrium P~1.1: Manufacturing
    * marginal, Healthcare blocked.
    */
  def sigmaThreshold(sigma: Sigma): Multiplier =
    (Multiplier(SigmaThreshBase) + Multiplier(SigmaThreshScale) * sigma.toScalar.log10.toMultiplier).min(Multiplier.One)

  // ---- Entry point ----

  /** Monthly entry point. Pipeline: decide → execute → greenInvest → invest →
    * digiDrift → inventory → FDI → informal evasion.
    */
  def process(
      firm: State,
      financialStocks: FinancialStocks,
      w: World,
      executionMonth: ExecutionMonth,
      operationalSignals: OperationalSignals,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
      allFirms: Vector[State],
      rng: RandomStream,
      corpBondDebt: PLN,
  )(using p: SimParams): Result =
    val decision = decide(firm, financialStocks, w, executionMonth, operationalSignals, lendRate, bankCanLend, allFirms, rng, corpBondDebt)
    val r0       = updateHiringSignalState(execute(firm, financialStocks, decision), firm, w, operationalSignals)
    val r0a      = applyLoanAmortization(r0)
    val r1       = applyGreenInvestment(r0a)
    val r2       = applyInvestment(r1)
    val r3       = applyDigitalDrift(r2)
    val r4       = applyInventory(r3, sectorDemandMult = operationalSignals.sectorDemandMult(firm.sector.toInt))
    val r5       = applyFdiFlows(r4)
    applyInformalCitEvasion(r5, Share(w.mechanisms.informalCyclicalAdj))

  // ---- Decide (all match logic + RandomStream rolls) ----

  /** Dispatch to tech-specific decision logic. Contains all RandomStream calls.
    */
  private def decide(
      firm: State,
      financialStocks: FinancialStocks,
      w: World,
      executionMonth: ExecutionMonth,
      operationalSignals: OperationalSignals,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
      allFirms: Vector[State],
      rng: RandomStream,
      corpBondDebt: PLN,
  )(using p: SimParams): Decision =
    firm.tech match
      case _: TechState.Bankrupt         => Decision.StayBankrupt
      case _: TechState.Automated        => decideAutomated(firm, financialStocks, w, executionMonth, operationalSignals, lendRate, corpBondDebt)
      case TechState.Hybrid(wkrs, aiEff) =>
        decideHybrid(firm, financialStocks, w, executionMonth, operationalSignals, lendRate, bankCanLend, wkrs, aiEff, rng, corpBondDebt)
      case TechState.Traditional(wkrs)   =>
        decideTraditional(firm, financialStocks, w, executionMonth, operationalSignals, lendRate, bankCanLend, allFirms, wkrs, rng, corpBondDebt)

  /** Smooth labor adjustment: Δworkers = λ × (target − current), with severance
    * costs. Target = break-even headcount from P&L. If adjustment insufficient
    * to restore solvency, escalates to bankruptcy.
    */
  private[amorfati] def attemptDownsize(
      firm: State,
      pnl: PnL,
      nc: PLN,
      workers: Int,
      newTech: Int => TechState,
      wage: PLN,
      reason: BankruptReason,
      drUpdate: Option[Share] = None,
  )(using p: SimParams): Decision =
    val minRetained         = p.firm.minWorkersRetained
    if workers <= minRetained then
      return if hasWorkingCapitalGrace(firm, pnl, nc) then Decision.Survive(pnl, nc, drUpdate = drUpdate) else Decision.GoBankrupt(pnl, nc, reason)
    val laborPerWorker: PLN = wage * effectiveWageMult(firm.sector)
    // Target headcount: workers needed for revenue to cover non-labor costs
    val nonLaborCost: PLN   = (pnl.costs - workers * laborPerWorker).max(PLN.Zero)
    val revenuePerWorker    = if workers > 0 then pnl.revenue / workers else PLN.Zero
    val contributionMargin  = (revenuePerWorker - laborPerWorker).max(PLN.Zero)
    val targetWorkers       =
      if contributionMargin > PLN.Zero then Math.ceil(nonLaborCost / contributionMargin).toInt
      else minRetained
    // Smooth adjustment: cut λ of the gap, not the entire excess
    val gap                 = workers - Math.max(minRetained, targetWorkers)
    val cutSpeedRaw         =
      if firm.stateOwned then (p.firm.laborAdjustSpeed * StateOwned.firingReduction).toLong
      else if isInStartup(firm) then (p.firm.laborAdjustSpeed * Multiplier(StartupDownsizeSpeedMultiplier)).toLong
      else p.firm.laborAdjustSpeed.toLong
    val cut                 = Math.max(1, bankerRound(BigInt(gap.toLong) * BigInt(cutSpeedRaw)).toInt)
    val newWkrs             = Math.max(minRetained, workers - cut)
    // Severance cost = fired workers × wage × severanceMonths
    val fired               = workers - newWkrs
    val severancePay: PLN   = fired * laborPerWorker * p.firm.severanceMonths
    val laborSaved: PLN     = fired * laborPerWorker
    val revRatio: Share     = Share.fraction(newWkrs, workers).sqrt
    val revLost: PLN        = pnl.revenue * (Share.One - revRatio)
    val adjustedNc          = nc + laborSaved - revLost - severancePay
    if adjustedNc >= PLN.Zero || hasWorkingCapitalGrace(firm, pnl, adjustedNc) then
      Decision.Downsize(pnl, newWkrs, adjustedNc, newTech(newWkrs), drUpdate = drUpdate)
    else Decision.GoBankrupt(pnl, nc, reason)

  private[amorfati] def startupRunwayLimit(firm: State)(using p: SimParams): PLN =
    if !isInStartup(firm) then PLN.Zero
    else
      val remainingShare = Share.fraction(firm.startupMonthsLeft, StartupRunwayMonths).clamp(Share.Zero, Share.One)
      p.firm.entryStartupCash * (Multiplier(StartupRunwayCashShare) * remainingShare)

  private def startupProgress(firm: State): Share =
    if !isInStartup(firm) then Share.One
    else Share.One - Share.fraction(firm.startupMonthsLeft, StartupRunwayMonths).clamp(Share.Zero, Share.One)

  private def startupCostMultiplier(firm: State): Multiplier =
    if !isInStartup(firm) then Multiplier.One
    else Multiplier(StartupCostFloor) + (Multiplier.One - Multiplier(StartupCostFloor)) * startupProgress(firm).toMultiplier

  private[amorfati] def hasWorkingCapitalGrace(firm: State, pnl: PnL, cashAfterDecision: PLN)(using p: SimParams): Boolean =
    firm.stateOwned ||
      (cashAfterDecision < PLN.Zero &&
        pnl.netAfterTax > PLN.Zero &&
        cashAfterDecision.abs <= pnl.netAfterTax * Multiplier(WorkingCapitalGraceMonths)) ||
      (isInStartup(firm) &&
        cashAfterDecision < PLN.Zero &&
        cashAfterDecision.abs <= startupRunwayLimit(firm))

  /** Estimate monthly operating cost for a hypothetical tech configuration.
    * Used by `decideHybrid` and `decideTraditional` to compare current costs
    * against upgrade costs.
    */
  private def estimateMonthlyCost(
      firm: State,
      financialStocks: FinancialStocks,
      opex: PLN,
      laborWorkers: Int,
      additionalDebt: PLN,
      wage: PLN,
      lendRate: Rate,
      domesticPrice: PriceIndex,
      importPrice: PriceIndex,
  )(using p: SimParams): PLN =
    val opexSizeFactor  = Scalar.fraction(firm.initialSize, p.pop.workersPerFirm).pow(Scalar(OpexSizeExponent)).toMultiplier
    val otherSizeFactor = Scalar.fraction(firm.initialSize, p.pop.workersPerFirm).toMultiplier
    val wMult           = effectiveWageMult(firm.sector)
    opex * ((domesticPrice * Multiplier(OpexDomesticShare) + importPrice * Multiplier(OpexImportShare)).toMultiplier * opexSizeFactor) +
      (financialStocks.firmLoan + additionalDebt) * lendRate.monthly +
      laborWorkers * (wage * wMult) +
      p.firm.otherCosts * (domesticPrice.toMultiplier * otherSizeFactor)

  /** Automated firm: compute PnL, survive or go bankrupt (AI debt trap). */
  private def decideAutomated(
      firm: State,
      financialStocks: FinancialStocks,
      w: World,
      executionMonth: ExecutionMonth,
      operationalSignals: OperationalSignals,
      lendRate: Rate,
      corpBondDebt: PLN,
  )(using p: SimParams): Decision =
    val pnl = computePnL(
      firm,
      financialStocks,
      w.householdMarket.marketWage,
      operationalSignals.sectorDemandMult(firm.sector.toInt),
      w.priceLevel,
      w.external.gvc.importCostIndex,
      w.external.gvc.commodityPriceIndex,
      lendRate,
      executionMonth,
      corpBondDebt,
    )
    val nc  = financialStocks.cash + pnl.netAfterTax
    if nc < PLN.Zero then Decision.GoBankrupt(pnl, nc, BankruptReason.AiDebtTrap)
    else Decision.Survive(pnl, nc)

  /** Hybrid firm: attempt full-AI upgrade, else survive/downsize/bankrupt. */
  private def decideHybrid(
      firm: State,
      financialStocks: FinancialStocks,
      w: World,
      executionMonth: ExecutionMonth,
      operationalSignals: OperationalSignals,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
      workers: Int,
      aiEff: Multiplier,
      rng: RandomStream,
      corpBondDebt: PLN,
  )(using p: SimParams): Decision =
    val pnl    = computePnL(
      firm,
      financialStocks,
      w.householdMarket.marketWage,
      operationalSignals.sectorDemandMult(firm.sector.toInt),
      w.priceLevel,
      w.external.gvc.importCostIndex,
      w.external.gvc.commodityPriceIndex,
      lendRate,
      executionMonth,
      corpBondDebt,
    )
    val ready2 = (firm.digitalReadiness + Share(HybridMonthlyDrDrift)).min(Share.One)

    if isInStartup(firm) then return startupFallbackDecision(firm, financialStocks, pnl, workers, w => TechState.Hybrid(w, aiEff), w.householdMarket.marketWage)

    val upCapex    = computeAiCapex(firm) * Share(HybridToFullCapexMul)
    val upLoan     = upCapex * Share(FullAiLoanShare)
    val upDown     = upCapex * Share(FullAiDownShare)
    val upCost     = estimateMonthlyCost(
      firm,
      financialStocks,
      p.firm.aiOpex,
      skeletonCrew(firm),
      upLoan,
      w.householdMarket.marketWage,
      lendRate,
      w.priceLevel,
      w.external.gvc.importCostIndex,
    )
    val profitable = pnl.costs > upCost * Multiplier(FullAiProfitMargin)
    val canPay     = financialStocks.cash > upDown
    val ready      = firm.digitalReadiness >= p.firm.fullAiReadinessMin
    val bankOk     = bankCanLend(upLoan)

    val prob: Share =
      if profitable && canPay && ready && bankOk then
        ((firm.riskProfile * Share(RiskWeightHybUpgrade)) + (w.real.automationRatio * Share(AutoRatioWeight))) * firm.digitalReadiness
      else Share.Zero

    if prob.sampleBelow(rng) then
      val eff = Multiplier.One + (Scalar.randomBetween(Scalar(HybToFullEffMin), Scalar(HybToFullEffMax), rng) * firm.digitalReadiness.toScalar).toMultiplier
      Decision.Upgrade(pnl, TechState.Automated(eff), upCapex, upLoan, upDown, drUpdate = Some(ready2))
    else
      val nc = financialStocks.cash + pnl.netAfterTax
      if nc < PLN.Zero then
        attemptDownsize(
          firm,
          pnl,
          nc,
          workers,
          w => TechState.Hybrid(w, aiEff),
          w.householdMarket.marketWage,
          BankruptReason.HybridInsolvency,
          drUpdate = Some(ready2),
        )
      else Decision.Survive(pnl, nc, drUpdate = Some(ready2))

  /** Upgrade feasibility for one tech path (full-AI or hybrid). */
  private case class UpgradeCandidate(
      capex: PLN,
      loan: PLN,
      down: PLN,
      profitable: Boolean,
      canPay: Boolean,
      ready: Boolean,
      bankOk: Boolean,
  ):
    def feasible: Boolean = profitable && canPay && ready && bankOk

  /** Evaluate full-AI upgrade feasibility for a traditional firm. */
  private def evaluateFullAi(
      firm: State,
      financialStocks: FinancialStocks,
      pnl: PnL,
      w: World,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
  )(using p: SimParams): UpgradeCandidate =
    val capex = computeAiCapex(firm)
    val loan  = capex * Share(FullAiLoanShare)
    val down  = capex * Share(FullAiDownShare)
    val cost  =
      estimateMonthlyCost(
        firm,
        financialStocks,
        p.firm.aiOpex,
        skeletonCrew(firm),
        loan,
        w.householdMarket.marketWage,
        lendRate,
        w.priceLevel,
        w.external.gvc.importCostIndex,
      )
    UpgradeCandidate(
      capex,
      loan,
      down,
      profitable = pnl.costs > (cost * Multiplier(FullAiProfitMargin)) / sigmaThreshold(w.currentSigmas(firm.sector.toInt)),
      canPay = financialStocks.cash > down,
      ready = firm.digitalReadiness >= p.firm.fullAiReadinessMin,
      bankOk = bankCanLend(loan),
    )

  /** Evaluate hybrid upgrade feasibility for a traditional firm. */
  private def evaluateHybrid(
      firm: State,
      financialStocks: FinancialStocks,
      pnl: PnL,
      workers: Int,
      w: World,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
  )(using p: SimParams): (UpgradeCandidate, Int) =
    val capex = computeHybridCapex(firm)
    val loan  = capex * Share(HybridLoanShare)
    val down  = capex * Share(HybridDownShare)
    val hWkrs = Math.max(3, p.sectorDefs(firm.sector.toInt).hybridRetainFrac.applyTo(workers))
    val cost  = estimateMonthlyCost(
      firm,
      financialStocks,
      p.firm.hybridOpex,
      hWkrs,
      loan,
      w.householdMarket.marketWage,
      lendRate,
      w.priceLevel,
      w.external.gvc.importCostIndex,
    )
    val cand  = UpgradeCandidate(
      capex,
      loan,
      down,
      profitable = pnl.costs > (cost * Multiplier(HybridProfitMargin)) / sigmaThreshold(w.currentSigmas(firm.sector.toInt)),
      canPay = financialStocks.cash > down,
      ready = firm.digitalReadiness >= p.firm.hybridReadinessMin,
      bankOk = bankCanLend(loan),
    )
    (cand, hWkrs)

  /** Compute adoption probabilities for full-AI and hybrid upgrades. Blends
    * network mimetic pressure, desperation, and uncertainty discount.
    */
  private def adoptionProbabilities(
      firm: State,
      pnl: PnL,
      fullAi: UpgradeCandidate,
      hybrid: UpgradeCandidate,
      executionMonth: ExecutionMonth,
      w: World,
      allFirms: Vector[State],
  )(using p: SimParams): (Share, Share) =
    val localAuto   = computeLocalAutoRatio(firm, allFirms)
    val globalPanic = (w.real.automationRatio + w.real.hybridRatio * Share(HybridPanicDiscount)) * Share(HybridPanicDiscount)
    val panic       = localAuto * Share(LocalPanicWeight) + globalPanic * Share(GlobalPanicWeight)
    val desper      = if pnl.netAfterTax < PLN.Zero then Share(DesperationBonus) else Share.Zero
    val strat       =
      if !fullAi.profitable && fullAi.canPay && fullAi.ready && fullAi.bankOk then firm.riskProfile * firm.digitalReadiness * Share(StrategicAdoptBase)
      else Share.Zero

    val willingnessMultiplier = adoptionWillingnessMultiplier(executionMonth, localAuto)

    val rawFull = willingnessMultiplier *
      (if fullAi.feasible then (firm.riskProfile * Share(RiskWeightFullAi) + panic + desper) * firm.digitalReadiness
       else strat)
    val rawHyb  = willingnessMultiplier *
      (if hybrid.feasible then
         (firm.riskProfile * Share(RiskWeightHybrid) + panic * Share(HybridPanicDiscount) + desper * Share(HybridPanicDiscount)) * firm.digitalReadiness
       else Share.Zero)

    val pFull = rawFull.min(Share.One)
    val pHyb  = rawHyb.min(Share.One - pFull)
    (pFull, pHyb)

  private[amorfati] def adoptionWillingnessMultiplier(month: ExecutionMonth, localAuto: Share)(using p: SimParams): Share =
    val elapsedMonths = month.toInt - 1
    val rampFrac      = Scalar.fraction(elapsedMonths, p.firm.adoptionRampMonths).clamp(Scalar.Zero, Scalar.One).toShare
    val baseLevel     = Share(UncertaintyBase) + Share(UncertaintySlope) * rampFrac
    val demoBoost     =
      if localAuto > p.firm.demoEffectThresh then p.firm.demoEffectBoost * (localAuto - p.firm.demoEffectThresh)
      else Share.Zero
    (baseLevel + demoBoost).min(Share.One)

  /** Roll for full-AI upgrade: success (with random efficiency) or
    * implementation failure.
    */
  private def rollFullAiUpgrade(firm: State, pnl: PnL, ai: UpgradeCandidate, rng: RandomStream): Decision =
    val failRate = Share(FullAiBaseFailRate) + (Share.One - firm.digitalReadiness) * Share(FullAiFailDrSens)
    if failRate.sampleBelow(rng) then
      Decision.UpgradeFailed(pnl, BankruptReason.AiImplFailure, ai.capex * Share(FailCapexFrac), ai.loan * Share(FailLoanFrac), ai.down * Share(FailDownFrac))
    else
      val eff = Multiplier.One + (Scalar.randomBetween(Scalar(TradToFullEffMin), Scalar(TradToFullEffMax), rng) * firm.digitalReadiness.toScalar).toMultiplier
      Decision.Upgrade(pnl, TechState.Automated(eff), ai.capex, ai.loan, ai.down)

  /** Roll for hybrid upgrade: catastrophic failure, partial failure (bad
    * efficiency), or success (good efficiency).
    */
  private def rollHybridUpgrade(firm: State, pnl: PnL, hyb: UpgradeCandidate, hWkrs: Int, rng: RandomStream): Decision =
    val failRate = Share(HybridBaseFailRate) + (Share.One - firm.digitalReadiness) * Share(HybridFailDrSens)
    val draw     = Share.random(rng)
    if draw < failRate * Share(CatastrophicFailFrac) then
      Decision.UpgradeFailed(
        pnl,
        BankruptReason.HybridImplFailure,
        hyb.capex * Share(FailCapexFrac),
        hyb.loan * Share(FailLoanFrac),
        hyb.down * Share(FailDownFrac),
      )
    else if draw < failRate then
      val badEff = Multiplier(BadHybridEffBase) + Scalar.randomBetween(Scalar.Zero, Scalar(BadHybridEffRange), rng).toMultiplier
      Decision.Upgrade(pnl, TechState.Hybrid(hWkrs, badEff), hyb.capex, hyb.loan, hyb.down)
    else
      val goodEff = Multiplier.One +
        (Scalar(GoodHybridEffBase) + Scalar.randomBetween(Scalar.Zero, Scalar(GoodHybridEffRange), rng)) *
        (Share(GoodHybridDrBlend) + firm.digitalReadiness * Share(GoodHybridDrBlend)).toScalar.toMultiplier
      Decision.Upgrade(pnl, TechState.Hybrid(hWkrs, goodEff), hyb.capex, hyb.loan, hyb.down)

  /** Try upsize, digital readiness investment, downsize, or survive — fallback
    * when neither full-AI nor hybrid upgrade was chosen.
    */
  private def fallbackDecision(
      firm: State,
      financialStocks: FinancialStocks,
      pnl: PnL,
      w: World,
      operationalSignals: OperationalSignals,
      workers: Int,
      rng: RandomStream,
  )(using p: SimParams): Decision =
    val nc            = financialStocks.cash + pnl.netAfterTax
    // Firms now distinguish between a one-period desired workforce target,
    // a feasible near-term target, and the actual monthly adjustment.
    val desiredW      = desiredWorkers(firm, w, operationalSignals)
    val feasibleW     = feasibleWorkers(firm, workers, desiredW, pnl, nc)
    val gap           = feasibleW - workers
    val hiringThresh  = if workers <= 5 then MicroFirmHiringThreshold else Math.max(2, (workers * 0.10).toInt)
    val firingThresh  = Math.max(2, (workers * 0.10).toInt)
    val shouldAdjust  = if gap > 0 then gap >= hiringThresh else -gap >= firingThresh
    if shouldAdjust then
      val adj     = Math.max(1, (Math.abs(gap) * LaborAdjustFrac).toInt) * (if gap > 0 then 1 else -1)
      val newWkrs = (workers + adj).max(p.firm.minWorkersRetained)
      if newWkrs > workers && canFundUpsize(firm, pnl, nc, newWkrs - workers, w.householdMarket.marketWage) then
        return Decision.Upsize(pnl, newWkrs, nc, TechState.Traditional(newWkrs))
      else if newWkrs < workers then return Decision.Downsize(pnl, newWkrs, nc, TechState.Traditional(newWkrs))
    val digiCost: PLN = computeDigiInvestCost(firm)
    val canAfford     = nc > digiCost * Multiplier(DigiInvestCashMult)
    val competitive   = w.real.automationRatio + w.real.hybridRatio * Share(0.5)
    val diminishing   = Share.One - firm.digitalReadiness
    val digiProb      = (p.firm.digiInvestBaseProb * firm.riskProfile * diminishing * (Share(0.5) + competitive)).min(Share.One)
    if canAfford && digiProb.sampleBelow(rng) then
      val boost = p.firm.digiInvestBoost * diminishing
      val newDR = (firm.digitalReadiness + boost).min(Share.One)
      Decision.DigiInvest(pnl, digiCost, newDR)
    else if nc < PLN.Zero then
      attemptDownsize(firm, pnl, nc, workers, TechState.Traditional(_), w.householdMarket.marketWage, BankruptReason.LaborCostInsolvency)
    else Decision.Survive(pnl, nc)

  private def startupFallbackDecision(
      firm: State,
      financialStocks: FinancialStocks,
      pnl: PnL,
      currentWorkers: Int,
      nextTech: Int => TechState,
      wage: PLN,
  )(using p: SimParams): Decision =
    val nc            = financialStocks.cash + pnl.netAfterTax
    val targetWorkers = Math.max(currentWorkers, firm.startupTargetWorkers)
    if currentWorkers < targetWorkers && canFundUpsize(firm, pnl, nc, 1, wage) then Decision.Upsize(pnl, currentWorkers + 1, nc, nextTech(currentWorkers + 1))
    else if nc < PLN.Zero then
      if hasWorkingCapitalGrace(firm, pnl, nc) then Decision.Survive(pnl, nc)
      else attemptDownsize(firm, pnl, nc, currentWorkers, nextTech, wage, BankruptReason.LaborCostInsolvency)
    else Decision.Survive(pnl, nc)

  private[amorfati] def canFundUpsize(
      firm: State,
      pnl: PnL,
      cashAfterDecision: PLN,
      addedWorkers: Int,
      marketWage: PLN,
  )(using p: SimParams): Boolean =
    cashAfterDecision >= PLN.Zero ||
      firm.stateOwned ||
      (isInStartup(firm) &&
        cashAfterDecision.abs <= startupRunwayLimit(firm) +
        addedWorkers * (marketWage * effectiveWageMult(firm.sector)) * Multiplier(StartupHiringWorkingCapitalMonths)) ||
      (pnl.netAfterTax >= PLN.Zero &&
        cashAfterDecision.abs <= addedWorkers * (marketWage * effectiveWageMult(firm.sector)) * Multiplier(HiringWorkingCapitalMonths))

  /** Traditional firm: evaluate full-AI, hybrid, downsize, digital invest, or
    * survive/bankrupt. Dispatches to sub-evaluators.
    */
  private def decideTraditional(
      firm: State,
      financialStocks: FinancialStocks,
      w: World,
      executionMonth: ExecutionMonth,
      operationalSignals: OperationalSignals,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
      allFirms: Vector[State],
      workers: Int,
      rng: RandomStream,
      corpBondDebt: PLN,
  )(using p: SimParams): Decision =
    val pnl           = computePnL(
      firm,
      financialStocks,
      w.householdMarket.marketWage,
      operationalSignals.sectorDemandMult(firm.sector.toInt),
      w.priceLevel,
      w.external.gvc.importCostIndex,
      w.external.gvc.commodityPriceIndex,
      lendRate,
      executionMonth,
      corpBondDebt,
    )
    if isInStartup(firm) then return startupFallbackDecision(firm, financialStocks, pnl, workers, TechState.Traditional(_), w.householdMarket.marketWage)
    val ai            = evaluateFullAi(firm, financialStocks, pnl, w, lendRate, bankCanLend)
    val (hyb, hWkrs)  = evaluateHybrid(firm, financialStocks, pnl, workers, w, lendRate, bankCanLend)
    val (pFull, pHyb) = adoptionProbabilities(firm, pnl, ai, hyb, executionMonth, w, allFirms)
    val roll          = Share.random(rng)

    if roll < pFull then rollFullAiUpgrade(firm, pnl, ai, rng)
    else if roll < pFull + pHyb then rollHybridUpgrade(firm, pnl, hyb, hWkrs, rng)
    else fallbackDecision(firm, financialStocks, pnl, w, operationalSignals, workers, rng)

  // ---- Execute (pure dispatch, zero RandomStream calls) ----

  /** Pure dispatch: map `Decision` → `Result`. No RandomStream calls, no side
    * effects.
    */
  private def execute(firm: State, financialStocks: FinancialStocks, d: Decision)(using p: SimParams): Result =
    d match
      case Decision.StayBankrupt =>
        buildResult(firm, financialStocks, PnL.zero)

      case Decision.Survive(pnl, newCash, drUpdate) =>
        val stocks = financialStocks.copy(cash = newCash)
        buildResult(drUpdate.fold(firm)(dr => firm.copy(digitalReadiness = dr)), stocks, pnl)

      case Decision.GoBankrupt(pnl, cash, reason) =>
        buildResult(firm.copy(tech = TechState.Bankrupt(reason)), financialStocks.copy(cash = cash), pnl)

      case Decision.Upgrade(pnl, newTech, capex, loan, downPayment, drUpdate) =>
        val tImp   = capex * p.forex.techImportShare
        val f      = firm.copy(tech = newTech)
        val stocks = financialStocks.copy(
          firmLoan = financialStocks.firmLoan + loan,
          cash = financialStocks.cash + pnl.netAfterTax + loan - downPayment,
        )
        buildResult(
          drUpdate.fold(f)(dr => f.copy(digitalReadiness = dr)),
          stocks,
          pnl,
          capex = capex,
          techImports = tImp,
          newLoan = loan,
        )

      case Decision.UpgradeFailed(pnl, reason, capex, loan, down) =>
        val tImp = capex * p.forex.techImportShare
        buildResult(
          firm.copy(tech = TechState.Bankrupt(reason)),
          financialStocks.copy(
            cash = financialStocks.cash + pnl.netAfterTax + loan - down,
            firmLoan = financialStocks.firmLoan + loan,
          ),
          pnl,
          capex = capex,
          techImports = tImp,
          newLoan = loan,
        )

      case Decision.Downsize(pnl, _, adjustedCash, newTech, drUpdate) =>
        val f = firm.copy(tech = newTech)
        buildResult(drUpdate.fold(f)(dr => f.copy(digitalReadiness = dr)), financialStocks.copy(cash = adjustedCash), pnl)

      case Decision.Upsize(pnl, _, newCash, newTech) =>
        buildResult(firm.copy(tech = newTech), financialStocks.copy(cash = newCash), pnl)

      case Decision.DigiInvest(pnl, cost, newDR) =>
        val nc = financialStocks.cash + pnl.netAfterTax
        buildResult(firm.copy(digitalReadiness = newDR), financialStocks.copy(cash = nc - cost), pnl)

  /** Assemble `Result` from updated `State` and `PnL`. Flow fields not set by
    * the decision (equity, investment, inventory, FDI, evasion) default to zero
    * — filled by post-processing steps.
    */
  private def buildResult(
      firm: State,
      financialStocks: FinancialStocks,
      pnl: PnL,
      capex: PLN = PLN.Zero,
      techImports: PLN = PLN.Zero,
      newLoan: PLN = PLN.Zero,
  ): Result =
    Result(
      firm = advanceStartupLifecycle(firm.copy(accumulatedLoss = pnl.newAccumulatedLoss)),
      financialStocks = financialStocks,
      taxPaid = pnl.tax,
      realizedPostTaxProfit = pnl.netAfterTax.max(PLN.Zero),
      capexSpent = capex,
      techImports = techImports,
      newLoan = newLoan,
      equityIssuance = PLN.Zero,
      grossInvestment = PLN.Zero,
      bondIssuance = PLN.Zero,
      profitShiftCost = pnl.profitShiftCost,
      fdiRepatriation = PLN.Zero,
      inventoryChange = PLN.Zero,
      citEvasion = PLN.Zero,
      energyCost = pnl.energyCost,
      greenInvestment = PLN.Zero,
      principalRepaid = PLN.Zero,
    )

  private def advanceStartupLifecycle(firm: State): State =
    if isAlive(firm) && firm.startupMonthsLeft > 0 then firm.copy(startupMonthsLeft = firm.startupMonthsLeft - 1)
    else firm

  private def updateHiringSignalState(result: Result, prior: State, w: World, operationalSignals: OperationalSignals)(using p: SimParams): Result =
    if !isAlive(result.firm) then result
    else
      val currentWorkers = workerCount(prior)
      val desired        = desiredWorkers(prior, w, operationalSignals)
      val nextSignal     = nextHiringSignalMonths(prior, desired, currentWorkers)
      result.copy(firm = result.firm.copy(hiringSignalMonths = nextSignal))

  // ---- Post-processing pipeline ----

  /** Scheduled loan principal repayment: debt × amortRate per month. Reduces
    * the firm-loan and cash stocks; reports flow for SFC accounting. Bankrupt
    * firms and firms with zero debt skip.
    */
  private def applyLoanAmortization(r: Result)(using p: SimParams): Result =
    val f         = r.firm
    val stocks    = r.financialStocks
    if !isAlive(f) || stocks.firmLoan <= PLN.Zero then return r
    val principal = stocks.firmLoan * p.banking.firmLoanAmortRate
    val paid      = principal.min(stocks.cash.max(PLN.Zero))
    r.copy(
      financialStocks = stocks.copy(firmLoan = stocks.firmLoan - paid, cash = stocks.cash - paid),
      principalRepaid = paid,
    )

  /** Apply natural digital drift to all living firms (always-on). */
  private def applyDigitalDrift(r: Result)(using p: SimParams): Result =
    if p.firm.digiDrift <= Share.Zero then return r
    val f     = r.firm
    if !isAlive(f) then return r
    val newDR = (f.digitalReadiness + p.firm.digiDrift).min(Share.One)
    r.copy(firm = f.copy(digitalReadiness = newDR))

  /** Apply physical capital investment after firm decision. Depreciation,
    * replacement + expansion investment, cash-constrained.
    */
  private def applyInvestment(r: Result)(using p: SimParams): Result =
    val f          = r.firm
    if !isAlive(f) then return r.copy(firm = f.copy(capitalStock = PLN.Zero))
    val stocks     = r.financialStocks
    val depRate    = p.capital.depRates(f.sector.toInt).monthly
    val depn: PLN  = f.capitalStock * depRate
    val postDepK   = f.capitalStock - depn
    val targetK    = workerCount(f) * p.capital.klRatios(f.sector.toInt)
    val gap        = (targetK - postDepK).max(PLN.Zero)
    val invMult    = if f.stateOwned then StateOwned.directedInvestmentMultiplier(f.sector.toInt) else Multiplier.One
    val desiredInv = depn + (gap * p.capital.adjustSpeed * invMult)
    val actualInv  = desiredInv.min(stocks.cash.max(PLN.Zero))
    val newK       = postDepK + actualInv
    r.copy(
      firm = f.copy(capitalStock = newK),
      financialStocks = stocks.copy(cash = stocks.cash - actualInv),
      grossInvestment = actualInv,
    )

  // ---- PnL computation ----

  /** Residual "other" operating costs — base scaled by price and firm size,
    * reduced when physical capital, energy, or inventory costs are explicit.
    */
  private def otherCosts(firm: State, domesticPrice: PriceIndex)(using p: SimParams): PLN =
    val sizeFactor = Scalar.fraction(firm.initialSize, p.pop.workersPerFirm).toMultiplier
    val raw: PLN   = (domesticPrice * p.firm.otherCosts) * sizeFactor
    val afterCap   = raw * (Share.One - p.capital.costReplace)
    val afterEnerg = afterCap * (Share.One - p.climate.energyCostReplace)
    val adjusted   = afterEnerg * (Share.One - p.capital.inventoryCostReplace)
    adjusted * startupCostMultiplier(firm)

  /** AI/hybrid maintenance opex — domestic + imported split, sublinear in firm
    * size.
    */
  private def aiMaintenanceCost(firm: State, domesticPrice: PriceIndex, importPrice: PriceIndex)(using p: SimParams): PLN =
    val opexSizeFactor = Scalar.fraction(firm.initialSize, p.pop.workersPerFirm).pow(Scalar(OpexSizeExponent)).toMultiplier
    val priceFactor    = (domesticPrice * Multiplier(OpexDomesticShare) + importPrice * Multiplier(OpexImportShare)).toMultiplier
    firm.tech match
      case _: TechState.Automated => p.firm.aiOpex * priceFactor * opexSizeFactor * startupCostMultiplier(firm)
      case _: TechState.Hybrid    => p.firm.hybridOpex * priceFactor * opexSizeFactor * startupCostMultiplier(firm)
      case _                      => PLN.Zero

  /** Energy cost including EU ETS carbon surcharge, net of green capital
    * discount.
    */
  private def energyAndEtsCost(firm: State, revenue: PLN, month: ExecutionMonth, commodityPrice: PriceIndex)(using p: SimParams): PLN =
    val baseEnergy: PLN      = revenue * p.climate.energyCostShares(firm.sector.toInt)
    val monthsElapsed        = month.previousCompleted
    val etsGrowth            = (Scalar.One + p.climate.etsPriceDrift.monthly.toScalar).pow(monthsElapsed.toInt)
    val carbonSurcharge      = p.climate.carbonIntensity(firm.sector.toInt) * (etsGrowth - Scalar.One)
    val greenDiscount: Share = if firm.greenCapital > PLN.Zero then
      val targetGK = workerCount(firm) * p.climate.greenKLRatios(firm.sector.toInt)
      if targetGK > PLN.Zero then p.climate.greenMaxDiscount * firm.greenCapital.ratioTo(targetGK).toShare.clamp(Share.Zero, Share.One)
      else Share.Zero
    else Share.Zero
    val discountedEnergy     = commodityPrice * (baseEnergy * (Share.One - greenDiscount))
    discountedEnergy * (Multiplier.One + carbonSurcharge.max(Scalar.Zero).toMultiplier)

  /** Monthly P&L: revenue minus all cost categories, CIT on positive profit. */
  private[amorfati] def computePnL(
      firm: State,
      financialStocks: FinancialStocks,
      wage: PLN,
      sectorDemandMult: Multiplier,
      domesticPrice: PriceIndex,
      importPrice: PriceIndex,
      commodityPrice: PriceIndex,
      lendRate: Rate,
      month: ExecutionMonth,
      corpBondDebt: PLN = PLN.Zero,
  )(using p: SimParams): PnL =
    val revenue: PLN         = (domesticPrice * computeCapacity(firm)) * sectorDemandMult
    val labor: PLN           = workerCount(firm) * (wage * effectiveWageMult(firm.sector))
    val depnCost: PLN        = firm.capitalStock * p.capital.depRates(firm.sector.toInt).monthly
    val interest: PLN        = (financialStocks.firmLoan + corpBondDebt) * lendRate.monthly
    val inventoryCost: PLN   = firm.inventory * p.capital.inventoryCarryingCost.monthly
    val energyCost: PLN      = energyAndEtsCost(firm, revenue, month, commodityPrice)
    val prePsCosts           =
      labor + otherCosts(firm, domesticPrice) + depnCost + aiMaintenanceCost(firm, domesticPrice, importPrice) + interest + inventoryCost + energyCost
    val grossProfit          = revenue - prePsCosts
    val profitShiftCost: PLN =
      if firm.foreignOwned then grossProfit.max(PLN.Zero) * p.fdi.profitShiftRate
      else PLN.Zero
    val costs                = prePsCosts + profitShiftCost
    val profit               = revenue - costs

    // CIT with loss carryforward (Art. 7 ustawy o CIT):
    // - Losses accumulate when profit < 0
    // - When profitable: offset up to 50% of profit from accumulated losses
    // - Losses expire gradually (~5 year horizon via monthly decay)
    val (tax, newAccLoss) =
      if profit <= PLN.Zero then (PLN.Zero, firm.accumulatedLoss + profit.abs)
      else
        val maxOffset = profit * p.fiscal.citCarryforwardMaxShare
        val offset    = maxOffset.min(firm.accumulatedLoss)
        val taxable   = profit - offset
        val remaining = (firm.accumulatedLoss - offset) * (Multiplier.One - p.fiscal.citCarryforwardDecay.toMultiplier)
        (taxable * p.fiscal.citRate, remaining.max(PLN.Zero))

    PnL(revenue, costs, tax, profit - tax, profitShiftCost, energyCost, newAccLoss)

  private[amorfati] def realizedPostTaxProfit(
      firm: State,
      financialStocks: FinancialStocks,
      wage: PLN,
      sectorDemandMult: Multiplier,
      domesticPrice: PriceIndex,
      importPrice: PriceIndex,
      commodityPrice: PriceIndex,
      lendRate: Rate,
      month: ExecutionMonth,
      corpBondDebt: PLN = PLN.Zero,
  )(using p: SimParams): PLN =
    computePnL(
      firm,
      financialStocks,
      wage,
      sectorDemandMult,
      domesticPrice,
      importPrice,
      commodityPrice,
      lendRate,
      month,
      corpBondDebt,
    ).netAfterTax.max(PLN.Zero)

  /** Apply green capital investment — separate cash pool. Firms earmark
    * GreenBudgetShare of cash for green investment; physical capital
    * (applyInvestment) uses the remainder.
    */
  private def applyGreenInvestment(r: Result)(using p: SimParams): Result =
    val f           = r.firm
    if !isAlive(f) then return r.copy(firm = f.copy(greenCapital = PLN.Zero))
    val stocks      = r.financialStocks
    val depRate     = p.climate.greenDepRate.monthly
    val depn: PLN   = f.greenCapital * depRate
    val postDepGK   = f.greenCapital - depn
    val targetGK    = workerCount(f) * p.climate.greenKLRatios(f.sector.toInt)
    val gap         = (targetGK - postDepGK).max(PLN.Zero)
    val invMult     = if f.stateOwned then StateOwned.directedInvestmentMultiplier(f.sector.toInt) else Multiplier.One
    val desiredInv  = depn + (gap * p.climate.greenAdjustSpeed * invMult)
    val greenBudget = stocks.cash.max(PLN.Zero) * p.climate.greenBudgetShare
    val actualInv   = desiredInv.min(greenBudget)
    val newGK       = postDepGK + actualInv
    r.copy(
      firm = f.copy(greenCapital = newGK),
      financialStocks = stocks.copy(cash = stocks.cash - actualInv),
      greenInvestment = actualInv,
    )

  /** Apply inventory accumulation/drawdown after firm decision. Includes
    * sector-specific spoilage, target-based adjustment toward desired inventory
    * level, and stress liquidation at a discount when the firm is
    * cash-negative.
    */
  private def applyInventory(r: Result, sectorDemandMult: Multiplier)(using p: SimParams): Result =
    val f                     = r.firm
    if !isAlive(f) then return r.copy(firm = f.copy(inventory = PLN.Zero))
    val stocks                = r.financialStocks
    val cap                   = computeCapacity(f)
    val productionValue       = cap * p.capital.inventoryCostFraction
    val salesValue            = productionValue * sectorDemandMult.toShare.clamp(Share.Zero, Share.One)
    val unsoldValue           = (productionValue - salesValue).max(PLN.Zero)
    // Spoilage
    val spoilRate             = p.capital.inventorySpoilageRates(f.sector.toInt).monthly
    val postSpoilage          = f.inventory - f.inventory * spoilRate
    // Target-based adjustment
    val revenue               = cap * sectorDemandMult
    val targetInv             = revenue * p.capital.inventoryTargetRatios(f.sector.toInt)
    val desired               = (targetInv - postSpoilage) * p.capital.inventoryAdjustSpeed
    // Accumulate unsold + adjust toward target
    val rawChange             = unsoldValue + desired
    // Can't draw down more than available
    val invChange             = rawChange.max(-postSpoilage)
    val newInv                = (postSpoilage + invChange).max(PLN.Zero)
    // Stress liquidation: if cash < 0, sell inventory at discount
    val (finalInv, cashBoost) = if stocks.cash < PLN.Zero && newInv > PLN.Zero then
      val liquidate = newInv.min(stocks.cash.abs / p.capital.inventoryLiquidationDisc)
      (newInv - liquidate, liquidate * p.capital.inventoryLiquidationDisc)
    else (newInv, PLN.Zero)
    val actualChange          = finalInv - f.inventory
    r.copy(
      firm = f.copy(inventory = finalInv),
      financialStocks = stocks.copy(cash = stocks.cash + cashBoost),
      inventoryChange = actualChange,
    )

  /** Effective shadow share for a sector — base share + cyclical adjustment,
    * clamped to [0, 1].
    */
  private def effectiveShadowShare(sector: SectorIdx, carriedInformalAdj: Share)(using p: SimParams): Share =
    (p.informal.sectorShares(sector.toInt) + carriedInformalAdj).min(Share.One)

  /** CIT evasion fraction for a sector — shadow share × CIT evasion rate. */
  private def citEvasionFrac(sector: SectorIdx, carriedInformalAdj: Share)(using p: SimParams): Share =
    effectiveShadowShare(sector, carriedInformalAdj) * p.informal.citEvasion

  /** Apply informal CIT evasion using the carried current-step shadow-economy
    * adjustment from world state.
    */
  private def applyInformalCitEvasion(r: Result, carriedInformalAdj: Share)(using p: SimParams): Result =
    if !isAlive(r.firm) || r.taxPaid <= PLN.Zero then return r
    val evaded = r.taxPaid * citEvasionFrac(r.firm.sector, carriedInformalAdj)
    r.copy(
      financialStocks = r.financialStocks.copy(cash = r.financialStocks.cash + evaded),
      taxPaid = r.taxPaid - evaded,
      citEvasion = evaded,
    )

  /** Apply FDI dividend repatriation for foreign-owned firms (post-tax,
    * cash-constrained).
    */
  private def applyFdiFlows(r: Result)(using p: SimParams): Result =
    if !r.firm.foreignOwned || !isAlive(r.firm) then return r
    val afterTaxProfit: PLN =
      if p.fiscal.citRate > Rate.Zero && r.taxPaid > PLN.Zero then r.taxPaid * Multiplier(Rate(1.0) / p.fiscal.citRate - 1.0)
      else PLN.Zero
    val repatriation: PLN   =
      (afterTaxProfit.max(PLN.Zero) * p.fdi.repatriationRate).min(r.financialStocks.cash.max(PLN.Zero))
    if repatriation <= PLN.Zero then return r
    r.copy(financialStocks = r.financialStocks.copy(cash = r.financialStocks.cash - repatriation), fdiRepatriation = repatriation)
