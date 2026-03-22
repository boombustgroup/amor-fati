package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.types.*

import scala.util.Random

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
  case class Traditional(workers: Int)                  extends TechState
  case class Hybrid(workers: Int, aiEfficiency: Double) extends TechState
  case class Automated(efficiency: Double)              extends TechState
  case class Bankrupt(reason: BankruptReason)           extends TechState

/** Firm agent: stateless functions operating on `State`. Entry point:
  * `process`.
  */
object Firm:

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

  // Uncertainty discount: linearly increasing willingness over simulation horizon
  private val UncertaintyBase  = 0.15 // initial discount (month 0)
  private val UncertaintySlope = 0.15 // additional discount at end of simulation

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
  private val LaborAdjustFrac = 0.10 // fraction of gap closed per month (smooth, no overshoot)

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

  /** Full mutable state of a single firm, carried across simulation months. */
  case class State(
      id: FirmId,                          // Unique firm identifier (index into firms vector)
      cash: PLN,                           // Current cash balance (can be negative)
      debt: PLN,                           // Outstanding bank loan debt
      tech: TechState,                     // Current technology regime
      riskProfile: Share,                  // Propensity to invest / adopt technology [0,1]
      innovationCostFactor: Double,        // Firm-specific CAPEX multiplier (drawn at creation)
      digitalReadiness: Share,             // Digital readiness score [0,1], gates tech upgrades
      sector: SectorIdx,                   // Index into p.sectorDefs
      neighbors: Vector[FirmId],           // Network adjacency (firm IDs)
      bankId: BankId,                      // Multi-bank: index into Banking.State.banks
      equityRaised: PLN,                   // GPW: cumulative equity raised via IPO/SPO
      initialSize: Int,                    // Firm size at creation (heterogeneous when FIRM_SIZE_DIST=gus)
      capitalStock: PLN,                   // Physical capital stock (PLN)
      bondDebt: PLN,                       // Outstanding corporate bond debt
      foreignOwned: Boolean,               // FDI: subject to profit shifting & repatriation
      stateOwned: Boolean = false,         // SOE: Skarb Państwa ownership (dividend/employment/investment policy)
      inventory: PLN,                      // Inventory stock (PLN)
      greenCapital: PLN,                   // Green capital stock (PLN)
      accumulatedLoss: PLN,                // CIT loss carryforward stock (Art. 7 ustawy o CIT)
      markup: Multiplier = Multiplier.One, // Calvo pricing: firm-specific markup over marginal cost
      region: Region = Region.Central,     // NUTS-1 macroregion
  )

  /** Output of `process` for one firm in one month — updated state + flow
    * variables.
    */
  case class Result(
      firm: State,          // Updated firm state after this month
      taxPaid: PLN,         // CIT actually paid (after informal evasion)
      capexSpent: PLN,      // Technology upgrade CAPEX (AI or hybrid)
      techImports: PLN,     // Import content of CAPEX (forex demand)
      newLoan: PLN,         // New bank loan taken for upgrade
      equityIssuance: PLN,  // GPW equity raised this month (filled by S4)
      grossInvestment: PLN, // Physical capital investment this month
      bondIssuance: PLN,    // Corporate bond issuance (filled by S4)
      profitShiftCost: PLN, // FDI profit shifting outflow
      fdiRepatriation: PLN, // FDI dividend repatriation outflow
      inventoryChange: PLN, // Net inventory change (+ accumulation, - drawdown)
      citEvasion: PLN,      // CIT evaded via informal economy
      energyCost: PLN,      // Total energy + ETS cost this month
      greenInvestment: PLN, // Green capital investment this month
      principalRepaid: PLN, // Monthly firm loan principal repayment
  )
  object Result:
    /** Convenience factory for tests — all flow fields set to `PLN.Zero`. */
    def zero(firm: State): Result =
      Result(firm, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

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

  /** Effective wage multiplier including union wage premium. */
  def effectiveWageMult(sectorIdx: SectorIdx)(using p: SimParams): Multiplier =
    val base = p.sectorDefs(sectorIdx.toInt).wageMultiplier
    if p.flags.unions then base + base * (p.labor.unionWagePremium * p.labor.unionDensity(sectorIdx.toInt))
    else base

  /** Monthly production capacity in PLN. Scales with tech, sector, firm size;
    * augmented by physical capital via CES production function when enabled.
    *
    * CES: Y = A × [α·K^ρ + (1-α)·L^ρ]^(1/ρ) where ρ = (σ-1)/σ. High-σ sectors
    * (BPO=50) substitute K/L easily; low-σ (Public=1) resist.
    */
  @computationBoundary
  def computeCapacity(f: State)(using p: SimParams): PLN =
    import ComputationBoundary.toDouble
    val sec       = p.sectorDefs(f.sector.toInt)
    val sizeScale = Multiplier(f.initialSize.toDouble / p.pop.workersPerFirm)
    val laborEff  = f.tech match
      case TechState.Traditional(w) => Multiplier(w.toDouble / f.initialSize)
      case TechState.Hybrid(w, eff) => Multiplier(HybridLaborCapShare * (w.toDouble / f.initialSize) + HybridAiCapShare * eff)
      case TechState.Automated(eff) => Multiplier(eff)
      case _: TechState.Bankrupt    => Multiplier.Zero
    val tfp       = sizeScale * sec.revenueMultiplier
    if p.flags.physCap && f.capitalStock > PLN.Zero && laborEff > Multiplier.Zero then
      val targetK: PLN  = workerCount(f) * p.capital.klRatios(f.sector.toInt)
      val k: Multiplier = Multiplier(if targetK > PLN.Zero then f.capitalStock / targetK else 1.0).clamp(Multiplier(0.1), Multiplier(2.0))
      val alpha: Share  = p.capital.prodElast
      val sigma: Double = toDouble(sec.sigma)
      p.firm.baseRevenue * tfp * cesOutput(alpha, k, laborEff, sigma)
    else p.firm.baseRevenue * tfp * laborEff

  /** CES aggregator: [α·K^ρ + (1-α)·L^ρ]^(1/ρ). Degrades gracefully: σ→1 ≈
    * Cobb-Douglas, σ→∞ ≈ linear (perfect substitutes).
    */
  @computationBoundary
  private[amorfati] def cesOutput(alpha: Share, k: Multiplier, l: Multiplier, sigma: Double): Multiplier =
    import ComputationBoundary.toDouble
    if sigma <= 1.001 then // near-Leontief/Cobb-Douglas boundary — use Cobb-Douglas
      Multiplier(Math.pow(toDouble(k), toDouble(alpha)) * Math.pow(toDouble(l), 1.0 - toDouble(alpha)))
    else
      val rho   = (sigma - 1.0) / sigma
      val kTerm = toDouble(alpha) * Math.pow(toDouble(k), rho)
      val lTerm = (1.0 - toDouble(alpha)) * Math.pow(toDouble(l), rho)
      Multiplier(Math.pow(kTerm + lTerm, 1.0 / rho))

  /** Optimal worker count where marginal revenue ≈ marginal cost.
    *
    * Numerically searches for the headcount where adding one more worker would
    * cost more (wage) than the revenue gained (∂capacity × demand × price).
    * Bounded by [minWorkersRetained, 3 × initialSize].
    */
  @computationBoundary
  private def optimalWorkers(f: State, w: World)(using p: SimParams): Int =
    import ComputationBoundary.toDouble
    val demandMult = w.flows.sectorDemandMult(f.sector.toInt)
    val price      = w.priceLevel
    val wage       = toDouble(w.hhAgg.marketWage) * toDouble(effectiveWageMult(f.sector))
    val maxW       = f.initialSize * 3
    val minW       = p.firm.minWorkersRetained

    // Binary search: find largest w where marginal revenue > marginal cost
    var lo = minW; var hi = maxW
    while lo < hi do
      val mid     = (lo + hi + 1) / 2
      val capMid  = computeCapacity(f.copy(tech = TechState.Traditional(mid)))
      val capPrev = computeCapacity(f.copy(tech = TechState.Traditional(mid - 1)))
      val mr      = toDouble(capMid - capPrev) * demandMult * price
      if mr > wage then lo = mid else hi = mid - 1
    lo

  /** Effective AI CAPEX for sector — sublinear in firm size (exponent 0.6),
    * digital readiness discount.
    */
  @computationBoundary
  def computeAiCapex(f: State)(using p: SimParams): PLN =
    import ComputationBoundary.toDouble
    val sizeFactor   = Math.pow(f.initialSize.toDouble / p.pop.workersPerFirm, CapexSizeExponent)
    val digiDiscount = toDouble(Share.One - p.firm.digiCapexDiscount * f.digitalReadiness)
    PLN(toDouble(p.firm.aiCapex) * toDouble(p.sectorDefs(f.sector.toInt).aiCapexMultiplier) * f.innovationCostFactor * sizeFactor * digiDiscount)

  /** Hybrid upgrade CAPEX — same scaling as AI CAPEX but using hybrid
    * multipliers.
    */
  @computationBoundary
  def computeHybridCapex(f: State)(using p: SimParams): PLN =
    import ComputationBoundary.toDouble
    val sizeFactor   = Math.pow(f.initialSize.toDouble / p.pop.workersPerFirm, CapexSizeExponent)
    val digiDiscount = toDouble(Share.One - p.firm.digiCapexDiscount * f.digitalReadiness)
    PLN(toDouble(p.firm.hybridCapex) * toDouble(p.sectorDefs(f.sector.toInt).hybridCapexMultiplier) * f.innovationCostFactor * sizeFactor * digiDiscount)

  /** Digital investment cost — sublinear in firm size (exponent 0.5). */
  def computeDigiInvestCost(f: State)(using p: SimParams): PLN =
    val sizeFactor = Math.pow(f.initialSize.toDouble / p.pop.workersPerFirm, OpexSizeExponent)
    p.firm.digiInvestCost * Multiplier(sizeFactor)

  /** Fraction of a firm's network neighbors that have adopted automation
    * (Automated or Hybrid tech).
    *
    * Used in technology adoption decisions: firms with more automated neighbors
    * face stronger competitive pressure to digitalize (network externality /
    * peer effect). Returns 0.0 for firms with no neighbors (isolates).
    */
  def computeLocalAutoRatio(firm: Firm.State, firms: Vector[Firm.State]): Double =
    val neighbors = firm.neighbors
    if neighbors.isEmpty then return 0.0
    val autoCount = neighbors.count: nid =>
      val nf = firms(nid.toInt)
      nf.tech.isInstanceOf[TechState.Automated] || nf.tech.isInstanceOf[TechState.Hybrid]
    autoCount.toDouble / neighbors.length

  /** sigma-based threshold modifier: high sigma sectors find automation
    * profitable at lower cost gap. Only used for profitability threshold, NOT
    * for probability multiplier. Mapping: sigma=2->0.91, sigma=5->0.95,
    * sigma=10->0.98, sigma=50->1.00 At equilibrium P~1.1: Manufacturing
    * marginal, Healthcare blocked.
    */
  def sigmaThreshold(sigma: Double): Double =
    Math.min(1.0, SigmaThreshBase + SigmaThreshScale * Math.log(sigma) / Math.log(10.0))

  // ---- Entry point ----

  /** Monthly entry point. Pipeline: decide → execute → greenInvest → invest →
    * digiDrift → inventory → FDI → informal evasion.
    */
  def process(
      firm: State,
      w: World,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
      allFirms: Vector[State],
      rng: Random,
  )(using p: SimParams): Result =
    val decision = decide(firm, w, lendRate, bankCanLend, allFirms, rng)
    val r0       = execute(firm, decision)
    val r0a      = applyLoanAmortization(r0)
    val r1       = applyGreenInvestment(r0a)
    val r2       = applyInvestment(r1)
    val r3       = applyDigitalDrift(r2)
    val r4       = applyInventory(r3, sectorDemandMult = w.flows.sectorDemandMult(firm.sector.toInt))
    val r5       = applyFdiFlows(r4)
    applyInformalCitEvasion(r5, w.mechanisms.informalCyclicalAdj)

  // ---- Decide (all match logic + Random rolls) ----

  /** Dispatch to tech-specific decision logic. Contains all Random calls. */
  private def decide(
      firm: State,
      w: World,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
      allFirms: Vector[State],
      rng: Random,
  )(using p: SimParams): Decision =
    firm.tech match
      case _: TechState.Bankrupt         => Decision.StayBankrupt
      case _: TechState.Automated        => decideAutomated(firm, w, lendRate)
      case TechState.Hybrid(wkrs, aiEff) => decideHybrid(firm, w, lendRate, bankCanLend, wkrs, aiEff, rng)
      case TechState.Traditional(wkrs)   => decideTraditional(firm, w, lendRate, bankCanLend, allFirms, wkrs, rng)

  /** Smooth labor adjustment: Δworkers = λ × (target − current), with severance
    * costs. Target = break-even headcount from P&L. If adjustment insufficient
    * to restore solvency, escalates to bankruptcy.
    */
  @computationBoundary
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
    import ComputationBoundary.toDouble
    val minRetained         = p.firm.minWorkersRetained
    if workers <= minRetained then return Decision.GoBankrupt(pnl, nc, reason)
    val laborPerWorker: PLN = wage * effectiveWageMult(firm.sector)
    // Target headcount: workers needed for revenue to cover non-labor costs
    val nonLaborCost: PLN   = (pnl.costs - workers * laborPerWorker).max(PLN.Zero)
    val revenuePerWorker    = if workers > 0 then pnl.revenue / workers else PLN.Zero
    val targetWorkers       = if revenuePerWorker > PLN.Zero then Math.ceil(nonLaborCost / revenuePerWorker).toInt else minRetained
    // Smooth adjustment: cut λ of the gap, not the entire excess
    val gap                 = workers - Math.max(minRetained, targetWorkers)
    val cut                 = Math.max(1, (gap * toDouble(p.firm.laborAdjustSpeed)).toInt)
    val newWkrs             = Math.max(minRetained, workers - cut)
    // Severance cost = fired workers × wage × severanceMonths
    val fired               = workers - newWkrs
    val severancePay: PLN   = fired * laborPerWorker * Multiplier(p.firm.severanceMonths)
    val laborSaved: PLN     = fired * laborPerWorker
    val revRatio: Share     = Share.fraction(newWkrs, workers).sqrt
    val revLost: PLN        = pnl.revenue * (Share.One - revRatio)
    val adjustedNc          = nc + laborSaved - revLost - severancePay
    if adjustedNc >= PLN.Zero then Decision.Downsize(pnl, newWkrs, adjustedNc, newTech(newWkrs), drUpdate = drUpdate)
    else Decision.GoBankrupt(pnl, nc, reason)

  /** Estimate monthly operating cost for a hypothetical tech configuration.
    * Used by `decideHybrid` and `decideTraditional` to compare current costs
    * against upgrade costs.
    */
  @computationBoundary
  private def estimateMonthlyCost(
      firm: State,
      opex: PLN,
      laborWorkers: Int,
      additionalDebt: PLN,
      wage: PLN,
      lendRate: Rate,
      domesticPrice: PriceIndex,
      importPrice: PriceIndex,
  )(using p: SimParams): PLN =
    import ComputationBoundary.toDouble
    val opexSizeFactor  = Math.pow(firm.initialSize.toDouble / p.pop.workersPerFirm, OpexSizeExponent)
    val otherSizeFactor = firm.initialSize.toDouble / p.pop.workersPerFirm
    val wMult           = effectiveWageMult(firm.sector)
    opex * Multiplier((OpexDomesticShare * toDouble(domesticPrice) + OpexImportShare * toDouble(importPrice)) * opexSizeFactor) +
      (firm.debt + additionalDebt) * lendRate.monthly +
      PLN(toDouble(wage) * toDouble(wMult) * laborWorkers) +
      p.firm.otherCosts * Multiplier(toDouble(domesticPrice) * otherSizeFactor)

  /** Automated firm: compute PnL, survive or go bankrupt (AI debt trap). */
  private def decideAutomated(
      firm: State,
      w: World,
      lendRate: Rate,
  )(using p: SimParams): Decision =
    val pnl = computePnL(
      firm,
      w.hhAgg.marketWage,
      w.flows.sectorDemandMult(firm.sector.toInt),
      PriceIndex(w.priceLevel),
      w.external.gvc.importCostIndex,
      w.external.gvc.commodityPriceIndex,
      lendRate,
      w.month,
    )
    val nc  = firm.cash + pnl.netAfterTax
    if nc < PLN.Zero then Decision.GoBankrupt(pnl, nc, BankruptReason.AiDebtTrap)
    else Decision.Survive(pnl, nc)

  /** Hybrid firm: attempt full-AI upgrade, else survive/downsize/bankrupt. */
  @computationBoundary
  private def decideHybrid(
      firm: State,
      w: World,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
      workers: Int,
      aiEff: Double,
      rng: Random,
  )(using p: SimParams): Decision =
    import ComputationBoundary.toDouble
    val pnl    = computePnL(
      firm,
      w.hhAgg.marketWage,
      w.flows.sectorDemandMult(firm.sector.toInt),
      PriceIndex(w.priceLevel),
      w.external.gvc.importCostIndex,
      w.external.gvc.commodityPriceIndex,
      lendRate,
      w.month,
    )
    val ready2 = (firm.digitalReadiness + Share(HybridMonthlyDrDrift)).min(Share.One)

    val upCapex    = computeAiCapex(firm) * Share(HybridToFullCapexMul)
    val upLoan     = upCapex * Share(FullAiLoanShare)
    val upDown     = upCapex * Share(FullAiDownShare)
    val upCost     = estimateMonthlyCost(
      firm,
      p.firm.aiOpex,
      skeletonCrew(firm),
      upLoan,
      w.hhAgg.marketWage,
      lendRate,
      PriceIndex(w.priceLevel),
      w.external.gvc.importCostIndex,
    )
    val profitable = pnl.costs > upCost * Multiplier(FullAiProfitMargin)
    val canPay     = firm.cash > upDown
    val ready      = firm.digitalReadiness >= p.firm.fullAiReadinessMin
    val bankOk     = bankCanLend(upLoan)

    val prob =
      if profitable && canPay && ready && bankOk then
        (toDouble(firm.riskProfile) * RiskWeightHybUpgrade + toDouble(w.real.automationRatio) * AutoRatioWeight) * toDouble(firm.digitalReadiness)
      else 0.0

    if rng.nextDouble() < prob then
      val eff = 1.0 + rng.between(HybToFullEffMin, HybToFullEffMax) * toDouble(firm.digitalReadiness)
      Decision.Upgrade(pnl, TechState.Automated(eff), upCapex, upLoan, upDown, drUpdate = Some(ready2))
    else
      val nc = firm.cash + pnl.netAfterTax
      if nc < PLN.Zero then
        attemptDownsize(firm, pnl, nc, workers, w => TechState.Hybrid(w, aiEff), w.hhAgg.marketWage, BankruptReason.HybridInsolvency, drUpdate = Some(ready2))
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
  @computationBoundary
  private def evaluateFullAi(
      firm: State,
      pnl: PnL,
      w: World,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
  )(using p: SimParams): UpgradeCandidate =
    import ComputationBoundary.toDouble
    val capex = computeAiCapex(firm)
    val loan  = capex * Share(FullAiLoanShare)
    val down  = capex * Share(FullAiDownShare)
    val cost  =
      estimateMonthlyCost(firm, p.firm.aiOpex, skeletonCrew(firm), loan, w.hhAgg.marketWage, lendRate, PriceIndex(w.priceLevel), w.external.gvc.importCostIndex)
    UpgradeCandidate(
      capex,
      loan,
      down,
      profitable = pnl.costs > cost * Multiplier(FullAiProfitMargin / sigmaThreshold(toDouble(w.currentSigmas(firm.sector.toInt)))),
      canPay = firm.cash > down,
      ready = firm.digitalReadiness >= p.firm.fullAiReadinessMin,
      bankOk = bankCanLend(loan),
    )

  /** Evaluate hybrid upgrade feasibility for a traditional firm. */
  @computationBoundary
  private def evaluateHybrid(
      firm: State,
      pnl: PnL,
      workers: Int,
      w: World,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
  )(using p: SimParams): (UpgradeCandidate, Int) =
    import ComputationBoundary.toDouble
    val capex = computeHybridCapex(firm)
    val loan  = capex * Share(HybridLoanShare)
    val down  = capex * Share(HybridDownShare)
    val hWkrs = Math.max(3, (workers * toDouble(p.sectorDefs(firm.sector.toInt).hybridRetainFrac)).toInt)
    val cost  = estimateMonthlyCost(firm, p.firm.hybridOpex, hWkrs, loan, w.hhAgg.marketWage, lendRate, PriceIndex(w.priceLevel), w.external.gvc.importCostIndex)
    val cand  = UpgradeCandidate(
      capex,
      loan,
      down,
      profitable = pnl.costs > cost * Multiplier(HybridProfitMargin / sigmaThreshold(toDouble(w.currentSigmas(firm.sector.toInt)))),
      canPay = firm.cash > down,
      ready = firm.digitalReadiness >= p.firm.hybridReadinessMin,
      bankOk = bankCanLend(loan),
    )
    (cand, hWkrs)

  /** Compute adoption probabilities for full-AI and hybrid upgrades. Blends
    * network mimetic pressure, desperation, and uncertainty discount.
    */
  @computationBoundary
  private def adoptionProbabilities(
      firm: State,
      pnl: PnL,
      fullAi: UpgradeCandidate,
      hybrid: UpgradeCandidate,
      w: World,
      allFirms: Vector[State],
  )(using p: SimParams): (Double, Double) =
    import ComputationBoundary.toDouble
    val localAuto   = computeLocalAutoRatio(firm, allFirms)
    val globalPanic = (toDouble(w.real.automationRatio) + toDouble(w.real.hybridRatio) * HybridPanicDiscount) * HybridPanicDiscount
    val panic       = localAuto * LocalPanicWeight + globalPanic * GlobalPanicWeight
    val desper      = if pnl.netAfterTax < PLN.Zero then DesperationBonus else 0.0
    val strat       =
      if !fullAi.profitable && fullAi.canPay && fullAi.ready && fullAi.bankOk then
        toDouble(firm.riskProfile) * toDouble(firm.digitalReadiness) * StrategicAdoptBase
      else 0.0

    val baseDiscount        = UncertaintyBase + UncertaintySlope * (w.month.toDouble / p.timeline.duration.toDouble)
    val demoBoost           =
      if localAuto > toDouble(p.firm.demoEffectThresh) then toDouble(p.firm.demoEffectBoost) * (localAuto - toDouble(p.firm.demoEffectThresh))
      else 0.0
    val uncertaintyDiscount = Math.min(1.0, baseDiscount + demoBoost)

    val pFull = uncertaintyDiscount *
      (if fullAi.feasible then (toDouble(firm.riskProfile) * RiskWeightFullAi + panic + desper) * toDouble(firm.digitalReadiness)
       else strat)
    val pHyb  = uncertaintyDiscount *
      (if hybrid.feasible then
         (toDouble(firm.riskProfile) * RiskWeightHybrid + panic * HybridPanicDiscount + desper * HybridPanicDiscount) * toDouble(firm.digitalReadiness)
       else 0.0)
    (pFull, pHyb)

  /** Roll for full-AI upgrade: success (with random efficiency) or
    * implementation failure.
    */
  @computationBoundary
  private def rollFullAiUpgrade(firm: State, pnl: PnL, ai: UpgradeCandidate, rng: Random): Decision =
    import ComputationBoundary.toDouble
    val failRate = FullAiBaseFailRate + toDouble(Share.One - firm.digitalReadiness) * FullAiFailDrSens
    if rng.nextDouble() < failRate then
      Decision.UpgradeFailed(pnl, BankruptReason.AiImplFailure, ai.capex * Share(FailCapexFrac), ai.loan * Share(FailLoanFrac), ai.down * Share(FailDownFrac))
    else
      val eff = 1.0 + rng.between(TradToFullEffMin, TradToFullEffMax) * toDouble(firm.digitalReadiness)
      Decision.Upgrade(pnl, TechState.Automated(eff), ai.capex, ai.loan, ai.down)

  /** Roll for hybrid upgrade: catastrophic failure, partial failure (bad
    * efficiency), or success (good efficiency).
    */
  @computationBoundary
  private def rollHybridUpgrade(firm: State, pnl: PnL, hyb: UpgradeCandidate, hWkrs: Int, rng: Random): Decision =
    import ComputationBoundary.toDouble
    val failRate = HybridBaseFailRate + toDouble(Share.One - firm.digitalReadiness) * HybridFailDrSens
    val ir       = rng.nextDouble()
    if ir < failRate * CatastrophicFailFrac then
      Decision.UpgradeFailed(
        pnl,
        BankruptReason.HybridImplFailure,
        hyb.capex * Share(FailCapexFrac),
        hyb.loan * Share(FailLoanFrac),
        hyb.down * Share(FailDownFrac),
      )
    else if ir < failRate then
      val badEff = BadHybridEffBase + rng.between(0.0, BadHybridEffRange)
      Decision.Upgrade(pnl, TechState.Hybrid(hWkrs, badEff), hyb.capex, hyb.loan, hyb.down)
    else
      val goodEff = 1.0 + (GoodHybridEffBase + rng.between(0.0, GoodHybridEffRange)) *
        (GoodHybridDrBlend + toDouble(firm.digitalReadiness) * GoodHybridDrBlend)
      Decision.Upgrade(pnl, TechState.Hybrid(hWkrs, goodEff), hyb.capex, hyb.loan, hyb.down)

  /** Try upsize, digital readiness investment, downsize, or survive — fallback
    * when neither full-AI nor hybrid upgrade was chosen.
    */
  @computationBoundary
  private def fallbackDecision(
      firm: State,
      pnl: PnL,
      w: World,
      workers: Int,
      rng: Random,
  )(using p: SimParams): Decision =
    import ComputationBoundary.toDouble
    val nc            = firm.cash + pnl.netAfterTax
    // Marginal-cost labor adjustment: converge toward optimal headcount.
    // Only adjust when gap is material (≥ 2 workers) to avoid blocking
    // digi-invest and other decisions for trivial headcount differences.
    val optW          = optimalWorkers(firm, w)
    val gap           = optW - workers
    if Math.abs(gap) >= Math.max(2, (workers * 0.10).toInt) then
      val adj     = Math.max(1, (Math.abs(gap) * LaborAdjustFrac).toInt) * (if gap > 0 then 1 else -1)
      val newWkrs = (workers + adj).max(p.firm.minWorkersRetained)
      if newWkrs > workers && nc > PLN.Zero then return Decision.Upsize(pnl, newWkrs, nc, TechState.Traditional(newWkrs))
      else if newWkrs < workers then return Decision.Downsize(pnl, newWkrs, nc, TechState.Traditional(newWkrs))
    val digiCost: PLN = computeDigiInvestCost(firm)
    val canAfford     = nc > digiCost * Multiplier(DigiInvestCashMult)
    val competitive   = toDouble(w.real.automationRatio) + toDouble(w.real.hybridRatio) * 0.5
    val diminishing   = toDouble(Share.One - firm.digitalReadiness)
    val digiProb      = toDouble(p.firm.digiInvestBaseProb * firm.riskProfile) *
      diminishing * (0.5 + competitive)
    if canAfford && rng.nextDouble() < digiProb then
      val boost = toDouble(p.firm.digiInvestBoost) * diminishing
      val newDR = (firm.digitalReadiness + Share(boost)).min(Share.One)
      Decision.DigiInvest(pnl, digiCost, newDR)
    else if nc < PLN.Zero then attemptDownsize(firm, pnl, nc, workers, TechState.Traditional(_), w.hhAgg.marketWage, BankruptReason.LaborCostInsolvency)
    else Decision.Survive(pnl, nc)

  /** Traditional firm: evaluate full-AI, hybrid, downsize, digital invest, or
    * survive/bankrupt. Dispatches to sub-evaluators.
    */
  private def decideTraditional(
      firm: State,
      w: World,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
      allFirms: Vector[State],
      workers: Int,
      rng: Random,
  )(using p: SimParams): Decision =
    val pnl           = computePnL(
      firm,
      w.hhAgg.marketWage,
      w.flows.sectorDemandMult(firm.sector.toInt),
      PriceIndex(w.priceLevel),
      w.external.gvc.importCostIndex,
      w.external.gvc.commodityPriceIndex,
      lendRate,
      w.month,
    )
    val ai            = evaluateFullAi(firm, pnl, w, lendRate, bankCanLend)
    val (hyb, hWkrs)  = evaluateHybrid(firm, pnl, workers, w, lendRate, bankCanLend)
    val (pFull, pHyb) = adoptionProbabilities(firm, pnl, ai, hyb, w, allFirms)
    val roll          = rng.nextDouble()

    if roll < pFull then rollFullAiUpgrade(firm, pnl, ai, rng)
    else if roll < pFull + pHyb then rollHybridUpgrade(firm, pnl, hyb, hWkrs, rng)
    else fallbackDecision(firm, pnl, w, workers, rng)

  // ---- Execute (pure dispatch, zero Random calls) ----

  /** Pure dispatch: map `Decision` → `Result`. No Random calls, no side
    * effects.
    */
  private def execute(firm: State, d: Decision)(using p: SimParams): Result =
    d match
      case Decision.StayBankrupt =>
        buildResult(firm, PnL.zero)

      case Decision.Survive(pnl, newCash, drUpdate) =>
        val f = firm.copy(cash = newCash)
        buildResult(drUpdate.fold(f)(dr => f.copy(digitalReadiness = dr)), pnl)

      case Decision.GoBankrupt(pnl, cash, reason) =>
        buildResult(firm.copy(cash = cash, tech = TechState.Bankrupt(reason)), pnl)

      case Decision.Upgrade(pnl, newTech, capex, loan, downPayment, drUpdate) =>
        val tImp = capex * p.forex.techImportShare
        val f    = firm.copy(
          tech = newTech,
          debt = firm.debt + loan,
          cash = firm.cash + pnl.netAfterTax + loan - downPayment,
        )
        buildResult(
          drUpdate.fold(f)(dr => f.copy(digitalReadiness = dr)),
          pnl,
          capex = capex,
          techImports = tImp,
          newLoan = loan,
        )

      case Decision.UpgradeFailed(pnl, reason, capex, loan, down) =>
        val tImp = capex * p.forex.techImportShare
        buildResult(
          firm.copy(
            cash = firm.cash + pnl.netAfterTax + loan - down,
            debt = firm.debt + loan,
            tech = TechState.Bankrupt(reason),
          ),
          pnl,
          capex = capex,
          techImports = tImp,
          newLoan = loan,
        )

      case Decision.Downsize(pnl, _, adjustedCash, newTech, drUpdate) =>
        val f = firm.copy(cash = adjustedCash, tech = newTech)
        buildResult(drUpdate.fold(f)(dr => f.copy(digitalReadiness = dr)), pnl)

      case Decision.Upsize(pnl, _, newCash, newTech) =>
        buildResult(firm.copy(cash = newCash, tech = newTech), pnl)

      case Decision.DigiInvest(pnl, cost, newDR) =>
        val nc = firm.cash + pnl.netAfterTax
        buildResult(firm.copy(cash = nc - cost, digitalReadiness = newDR), pnl)

  /** Assemble `Result` from updated `State` and `PnL`. Flow fields not set by
    * the decision (equity, investment, inventory, FDI, evasion) default to zero
    * — filled by post-processing steps.
    */
  private def buildResult(
      firm: State,
      pnl: PnL,
      capex: PLN = PLN.Zero,
      techImports: PLN = PLN.Zero,
      newLoan: PLN = PLN.Zero,
  ): Result =
    Result(
      firm = firm.copy(accumulatedLoss = pnl.newAccumulatedLoss),
      taxPaid = pnl.tax,
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

  // ---- Post-processing pipeline ----

  /** Scheduled loan principal repayment: debt × amortRate per month. Reduces
    * firm.debt and firm.cash; reports flow for SFC accounting. Bankrupt firms
    * and firms with zero debt skip.
    */
  private def applyLoanAmortization(r: Result)(using p: SimParams): Result =
    val f         = r.firm
    if !isAlive(f) || f.debt <= PLN.Zero then return r
    val principal = f.debt * p.banking.firmLoanAmortRate
    val paid      = principal.min(f.cash.max(PLN.Zero))
    r.copy(
      firm = f.copy(debt = f.debt - paid, cash = f.cash - paid),
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
    if !p.flags.physCap then return r
    val f          = r.firm
    if !isAlive(f) then return r.copy(firm = f.copy(capitalStock = PLN.Zero))
    val depRate    = p.capital.depRates(f.sector.toInt).monthly
    val depn: PLN  = f.capitalStock * depRate
    val postDepK   = f.capitalStock - depn
    val targetK    = workerCount(f) * p.capital.klRatios(f.sector.toInt)
    val gap        = (targetK - postDepK).max(PLN.Zero)
    val desiredInv = depn + gap * p.capital.adjustSpeed
    val actualInv  = desiredInv.min(f.cash.max(PLN.Zero))
    val newK       = postDepK + actualInv
    r.copy(firm = f.copy(cash = f.cash - actualInv, capitalStock = newK), grossInvestment = actualInv)

  // ---- PnL computation ----

  /** Residual "other" operating costs — base scaled by price and firm size,
    * reduced when physical capital, energy, or inventory costs are explicit.
    */
  @computationBoundary
  private def otherCosts(firm: State, domesticPrice: PriceIndex)(using p: SimParams): PLN =
    import ComputationBoundary.toDouble
    val sizeFactor = firm.initialSize.toDouble / p.pop.workersPerFirm
    val raw: PLN   = p.firm.otherCosts * Multiplier(toDouble(domesticPrice) * sizeFactor)
    val afterCap   = if p.flags.physCap then raw * (Share.One - p.capital.costReplace) else raw
    val afterEnerg = if p.flags.energy then afterCap * (Share.One - p.climate.energyCostReplace) else afterCap
    if p.flags.inventory then afterEnerg * (Share.One - p.capital.inventoryCostReplace) else afterEnerg

  /** AI/hybrid maintenance opex — domestic + imported split, sublinear in firm
    * size.
    */
  @computationBoundary
  private def aiMaintenanceCost(firm: State, domesticPrice: PriceIndex, importPrice: PriceIndex)(using p: SimParams): PLN =
    import ComputationBoundary.toDouble
    val opexSizeFactor = Math.pow(firm.initialSize.toDouble / p.pop.workersPerFirm, OpexSizeExponent)
    val priceFactor    = OpexDomesticShare * toDouble(domesticPrice) + OpexImportShare * toDouble(importPrice)
    firm.tech match
      case _: TechState.Automated => p.firm.aiOpex * Multiplier(priceFactor * opexSizeFactor)
      case _: TechState.Hybrid    => p.firm.hybridOpex * Multiplier(priceFactor * opexSizeFactor)
      case _                      => PLN.Zero

  /** Energy cost including EU ETS carbon surcharge, net of green capital
    * discount.
    */
  @computationBoundary
  private def energyAndEtsCost(firm: State, revenue: PLN, month: Int, commodityPrice: PriceIndex)(using p: SimParams): PLN =
    import ComputationBoundary.toDouble
    if !p.flags.energy then return PLN.Zero
    val baseEnergy: PLN      = revenue * p.climate.energyCostShares(firm.sector.toInt)
    val etsPrice             = p.climate.etsBasePrice * Math.pow(1.0 + toDouble(p.climate.etsPriceDrift) / 12.0, month.toDouble)
    val carbonSurcharge      = p.climate.carbonIntensity(firm.sector.toInt) * (etsPrice / p.climate.etsBasePrice - 1.0)
    val greenDiscount: Share = if firm.greenCapital > PLN.Zero then
      val targetGK = workerCount(firm) * p.climate.greenKLRatios(firm.sector.toInt)
      if targetGK > PLN.Zero then p.climate.greenMaxDiscount * Share(Math.min(1.0, firm.greenCapital / targetGK))
      else Share.Zero
    else Share.Zero
    PLN(toDouble(baseEnergy) * (1.0 + Math.max(0.0, carbonSurcharge)) * toDouble(Share.One - greenDiscount) * toDouble(commodityPrice))

  /** Monthly P&L: revenue minus all cost categories, CIT on positive profit. */
  @computationBoundary
  private def computePnL(
      firm: State,
      wage: PLN,
      sectorDemandMult: Double,
      domesticPrice: PriceIndex,
      importPrice: PriceIndex,
      commodityPrice: PriceIndex,
      lendRate: Rate,
      month: Int,
  )(using p: SimParams): PnL =
    import ComputationBoundary.toDouble
    val revenue: PLN         = computeCapacity(firm) * Multiplier(sectorDemandMult * toDouble(domesticPrice))
    val labor: PLN           = PLN(toDouble(wage) * workerCount(firm) * toDouble(effectiveWageMult(firm.sector)))
    val depnCost: PLN        =
      if p.flags.physCap then firm.capitalStock * p.capital.depRates(firm.sector.toInt).monthly
      else PLN.Zero
    val interest: PLN        = (firm.debt + firm.bondDebt) * lendRate.monthly
    val inventoryCost: PLN   =
      if p.flags.inventory then firm.inventory * p.capital.inventoryCarryingCost.monthly else PLN.Zero
    val energyCost: PLN      = energyAndEtsCost(firm, revenue, month, commodityPrice)
    val prePsCosts           =
      labor + otherCosts(firm, domesticPrice) + depnCost + aiMaintenanceCost(firm, domesticPrice, importPrice) + interest + inventoryCost + energyCost
    val grossProfit          = revenue - prePsCosts
    val profitShiftCost: PLN =
      if p.flags.fdi && firm.foreignOwned then grossProfit.max(PLN.Zero) * p.fdi.profitShiftRate
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
        val remaining = (firm.accumulatedLoss - offset) * Share(1.0 - toDouble(p.fiscal.citCarryforwardDecay))
        (taxable * p.fiscal.citRate, remaining.max(PLN.Zero))

    PnL(revenue, costs, tax, profit - tax, profitShiftCost, energyCost, newAccLoss)

  /** Apply green capital investment — separate cash pool. Firms earmark
    * GreenBudgetShare of cash for green investment; physical capital
    * (applyInvestment) uses the remainder.
    */
  private def applyGreenInvestment(r: Result)(using p: SimParams): Result =
    if !p.flags.energy then return r
    val f           = r.firm
    if !isAlive(f) then return r.copy(firm = f.copy(greenCapital = PLN.Zero))
    val depRate     = p.climate.greenDepRate.monthly
    val depn: PLN   = f.greenCapital * depRate
    val postDepGK   = f.greenCapital - depn
    val targetGK    = workerCount(f) * p.climate.greenKLRatios(f.sector.toInt)
    val gap         = (targetGK - postDepGK).max(PLN.Zero)
    val desiredInv  = depn + gap * p.climate.greenAdjustSpeed
    val greenBudget = f.cash.max(PLN.Zero) * p.climate.greenBudgetShare
    val actualInv   = desiredInv.min(greenBudget)
    val newGK       = postDepGK + actualInv
    r.copy(firm = f.copy(cash = f.cash - actualInv, greenCapital = newGK), greenInvestment = actualInv)

  /** Apply inventory accumulation/drawdown after firm decision. Includes
    * sector-specific spoilage, target-based adjustment toward desired inventory
    * level, and stress liquidation at a discount when the firm is
    * cash-negative.
    */
  private def applyInventory(r: Result, sectorDemandMult: Double)(using p: SimParams): Result =
    if !p.flags.inventory then return r
    val f                     = r.firm
    if !isAlive(f) then return r.copy(firm = f.copy(inventory = PLN.Zero))
    val cap                   = computeCapacity(f)
    val productionValue       = cap * p.capital.inventoryCostFraction
    val salesValue            = productionValue * Share(Math.min(1.0, sectorDemandMult))
    val unsoldValue           = (productionValue - salesValue).max(PLN.Zero)
    // Spoilage
    val spoilRate             = p.capital.inventorySpoilageRates(f.sector.toInt).monthly
    val postSpoilage          = f.inventory - f.inventory * spoilRate
    // Target-based adjustment
    val revenue               = cap * Multiplier(sectorDemandMult)
    val targetInv             = revenue * p.capital.inventoryTargetRatios(f.sector.toInt)
    val desired               = (targetInv - postSpoilage) * p.capital.inventoryAdjustSpeed
    // Accumulate unsold + adjust toward target
    val rawChange             = unsoldValue + desired
    // Can't draw down more than available
    val invChange             = rawChange.max(-postSpoilage)
    val newInv                = (postSpoilage + invChange).max(PLN.Zero)
    // Stress liquidation: if cash < 0, sell inventory at discount
    val (finalInv, cashBoost) = if f.cash < PLN.Zero && newInv > PLN.Zero then
      val liquidate = newInv.min(f.cash.abs / p.capital.inventoryLiquidationDisc)
      (newInv - liquidate, liquidate * p.capital.inventoryLiquidationDisc)
    else (newInv, PLN.Zero)
    val actualChange          = finalInv - f.inventory
    r.copy(
      firm = f.copy(inventory = finalInv, cash = f.cash + cashBoost),
      inventoryChange = actualChange,
    )

  /** Effective shadow share for a sector — base share + cyclical adjustment,
    * clamped to [0, 1].
    */
  private def effectiveShadowShare(sector: SectorIdx, cyclicalAdj: Double)(using p: SimParams): Share =
    (p.informal.sectorShares(sector.toInt) + Share(cyclicalAdj)).min(Share.One)

  /** CIT evasion fraction for a sector — shadow share × CIT evasion rate. */
  private def citEvasionFrac(sector: SectorIdx, cyclicalAdj: Double)(using p: SimParams): Share =
    effectiveShadowShare(sector, cyclicalAdj) * p.informal.citEvasion

  /** Apply informal CIT evasion — firm keeps evaded tax. */
  private def applyInformalCitEvasion(r: Result, cyclicalAdj: Double)(using p: SimParams): Result =
    if !p.flags.informal || !isAlive(r.firm) || r.taxPaid <= PLN.Zero then return r
    val evaded = r.taxPaid * citEvasionFrac(r.firm.sector, cyclicalAdj)
    r.copy(
      firm = r.firm.copy(cash = r.firm.cash + evaded),
      taxPaid = r.taxPaid - evaded,
      citEvasion = evaded,
    )

  /** Apply FDI dividend repatriation for foreign-owned firms (post-tax,
    * cash-constrained).
    */
  private def applyFdiFlows(r: Result)(using p: SimParams): Result =
    if !p.flags.fdi || !r.firm.foreignOwned || !isAlive(r.firm) then return r
    val afterTaxProfit: PLN =
      if p.fiscal.citRate > Rate.Zero && r.taxPaid > PLN.Zero then r.taxPaid * Multiplier(Rate(1.0) / p.fiscal.citRate - 1.0)
      else PLN.Zero
    val repatriation: PLN   =
      (afterTaxProfit.max(PLN.Zero) * p.fdi.repatriationRate).min(r.firm.cash.max(PLN.Zero))
    if repatriation <= PLN.Zero then return r
    r.copy(firm = r.firm.copy(cash = r.firm.cash - repatriation), fdiRepatriation = repatriation)
