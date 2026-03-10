package sfc.agents

import sfc.config.{SectorDefs, SimParams}
import sfc.engine.World
import sfc.types.*

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

  // ---- Data types ----

  /** Full mutable state of a single firm, carried across simulation months. */
  case class State(
      id: FirmId,                   // Unique firm identifier (index into firms vector)
      cash: PLN,                    // Current cash balance (can be negative)
      debt: PLN,                    // Outstanding bank loan debt
      tech: TechState,              // Current technology regime
      riskProfile: Ratio,           // Propensity to invest / adopt technology [0,1]
      innovationCostFactor: Double, // Firm-specific CAPEX multiplier (drawn at creation)
      digitalReadiness: Ratio,      // Digital readiness score [0,1], gates tech upgrades
      sector: SectorIdx,            // Index into SectorDefs
      neighbors: Array[FirmId],     // Network adjacency (firm IDs)
      bankId: BankId,               // Multi-bank: index into Banking.State.banks
      equityRaised: PLN,            // GPW: cumulative equity raised via IPO/SPO
      initialSize: Int,             // Firm size at creation (heterogeneous when FIRM_SIZE_DIST=gus)
      capitalStock: PLN,            // Physical capital stock (PLN)
      bondDebt: PLN,                // Outstanding corporate bond debt
      foreignOwned: Boolean,        // FDI: subject to profit shifting & repatriation
      inventory: PLN,               // Inventory stock (PLN)
      greenCapital: PLN,            // Green capital stock (PLN)
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
  )
  object Result:
    /** Convenience factory for tests — all flow fields set to `PLN.Zero`. */
    def zero(firm: State): Result =
      Result(firm, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  /** Monthly profit-and-loss breakdown, computed by `computePnL`. */
  case class PnL(
      revenue: PLN,         // Gross revenue (capacity × demand × price)
      costs: PLN,           // Total costs including profit shifting
      tax: PLN,             // CIT on positive profit
      netAfterTax: PLN,     // Profit minus tax (can be negative)
      profitShiftCost: PLN, // FDI profit shifting cost (zero if not foreign-owned)
      energyCost: PLN,      // Energy + ETS carbon surcharge
  )
  object PnL:
    val zero: PnL = PnL(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  /** Intermediate ADT from `decide` → `execute`. Separates stochastic choices
    * from state mutation.
    */
  sealed trait Decision
  object Decision:
    case object StayBankrupt                                                                                                  extends Decision
    case class Survive(pnl: PnL, newCash: PLN, drUpdate: Option[Ratio] = None)                                                extends Decision
    case class GoBankrupt(pnl: PnL, cash: PLN, reason: BankruptReason)                                                        extends Decision
    case class Upgrade(pnl: PnL, newTech: TechState, capex: PLN, loan: PLN, downPayment: PLN, drUpdate: Option[Ratio] = None) extends Decision
    case class UpgradeFailed(pnl: PnL, reason: BankruptReason, capex: PLN, loan: PLN, down: PLN)                              extends Decision
    case class Downsize(pnl: PnL, newWorkers: Int, adjustedCash: PLN, newTech: TechState, drUpdate: Option[Ratio] = None)     extends Decision
    case class DigiInvest(pnl: PnL, cost: PLN, newDR: Ratio)                                                                  extends Decision

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
    Math.max(p.firm.autoSkeletonCrew, (f.initialSize * 0.02).toInt)

  /** Effective wage multiplier including union wage premium. */
  def effectiveWageMult(sectorIdx: SectorIdx)(using p: SimParams): Ratio =
    val base = Ratio(SectorDefs(sectorIdx.toInt).wageMultiplier)
    if p.flags.unions then base + base * (p.labor.unionWagePremium * p.labor.unionDensity(sectorIdx.toInt))
    else base

  /** Monthly production capacity in PLN. Scales with tech, sector, firm size;
    * augmented by physical capital (Cobb-Douglas) when enabled.
    */
  def computeCapacity(f: State)(using p: SimParams): PLN =
    val sec       = SectorDefs(f.sector.toInt)
    val sizeScale = f.initialSize.toDouble / p.pop.workersPerFirm
    val base: PLN = f.tech match
      case TechState.Traditional(w) => p.firm.baseRevenue * (sizeScale * sec.revenueMultiplier * Math.sqrt(w.toDouble / f.initialSize))
      case TechState.Hybrid(w, eff) => p.firm.baseRevenue * (sizeScale * sec.revenueMultiplier * (0.4 * Math.sqrt(w.toDouble / f.initialSize) + 0.6 * eff))
      case TechState.Automated(eff) => p.firm.baseRevenue * (sizeScale * sec.revenueMultiplier * eff)
      case _: TechState.Bankrupt    => PLN.Zero
    if p.flags.physCap && f.capitalStock > PLN.Zero && base > PLN.Zero then
      val targetK       = workerCount(f).toDouble * p.capital.klRatios.map(_.toDouble)(f.sector.toInt)
      val kRatio        = if targetK > 0 then f.capitalStock.toDouble / targetK else 1.0
      val capitalFactor = Math.pow(Math.min(2.0, Math.max(0.1, kRatio)), p.capital.prodElast.toDouble)
      base * capitalFactor
    else base

  /** Effective AI CAPEX for sector — sublinear in firm size (exponent 0.6),
    * digital readiness discount.
    */
  def computeAiCapex(f: State)(using p: SimParams): PLN =
    val sizeFactor   = Math.pow(f.initialSize.toDouble / p.pop.workersPerFirm, 0.6)
    val digiDiscount = (Ratio.One - p.firm.digiCapexDiscount * f.digitalReadiness).toDouble
    p.firm.aiCapex * (SectorDefs(f.sector.toInt).aiCapexMultiplier * f.innovationCostFactor * sizeFactor * digiDiscount)

  /** Hybrid upgrade CAPEX — same scaling as AI CAPEX but using hybrid
    * multipliers.
    */
  def computeHybridCapex(f: State)(using p: SimParams): PLN =
    val sizeFactor   = Math.pow(f.initialSize.toDouble / p.pop.workersPerFirm, 0.6)
    val digiDiscount = (Ratio.One - p.firm.digiCapexDiscount * f.digitalReadiness).toDouble
    p.firm.hybridCapex * (SectorDefs(f.sector.toInt).hybridCapexMultiplier * f.innovationCostFactor * sizeFactor * digiDiscount)

  /** Digital investment cost — sublinear in firm size (exponent 0.5). */
  def computeDigiInvestCost(f: State)(using p: SimParams): PLN =
    val sizeFactor = Math.pow(f.initialSize.toDouble / p.pop.workersPerFirm, 0.5)
    p.firm.digiInvestCost * sizeFactor

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
    Math.min(1.0, 0.88 + 0.075 * Math.log(sigma) / Math.log(10.0))

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
  )(using p: SimParams): Result =
    val decision = decide(firm, w, lendRate, bankCanLend, allFirms)
    val r0       = execute(firm, decision)
    val r1       = applyGreenInvestment(r0)
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
  )(using p: SimParams): Decision =
    firm.tech match
      case _: TechState.Bankrupt         => Decision.StayBankrupt
      case _: TechState.Automated        => decideAutomated(firm, w, lendRate)
      case TechState.Hybrid(wkrs, aiEff) => decideHybrid(firm, w, lendRate, bankCanLend, wkrs, aiEff)
      case TechState.Traditional(wkrs)   => decideTraditional(firm, w, lendRate, bankCanLend, allFirms, wkrs)

  /** Attempt forced downsizing: cut up to 30% of workers, survive if net cash
    * turns non-negative after labor savings minus lost revenue, else bankrupt.
    */
  private def attemptDownsize(
      firm: State,
      pnl: PnL,
      nc: PLN,
      workers: Int,
      newTech: Int => TechState,
      wage: PLN,
      reason: BankruptReason,
      drUpdate: Option[Ratio] = None,
  )(using SimParams): Decision =
    if workers > 3 then
      val laborPerWorker: PLN = wage * effectiveWageMult(firm.sector)
      val maxCut              = Math.max(1, (workers * 0.30).toInt)
      val newWkrs             = Math.max(3, workers - maxCut)
      val laborSaved          = laborPerWorker * (workers - newWkrs).toDouble
      val revRatio            = Math.sqrt(newWkrs.toDouble / workers.toDouble)
      val revLost             = pnl.revenue * (1.0 - revRatio)
      val adjustedNc          = nc + laborSaved - revLost
      if adjustedNc >= PLN.Zero then Decision.Downsize(pnl, newWkrs, adjustedNc, newTech(newWkrs), drUpdate = drUpdate)
      else Decision.GoBankrupt(pnl, nc, reason)
    else Decision.GoBankrupt(pnl, nc, reason)

  /** Automated firm: compute PnL, survive or go bankrupt (AI debt trap). */
  private def decideAutomated(
      firm: State,
      w: World,
      lendRate: Rate,
  )(using p: SimParams): Decision =
    val pnl = computePnL(firm, w.hh.marketWage, w.flows.sectorDemandMult(firm.sector.toInt), w.priceLevel, lendRate, w.month)
    val nc  = firm.cash + pnl.netAfterTax
    if nc < PLN.Zero then Decision.GoBankrupt(pnl, nc, BankruptReason.AiDebtTrap)
    else Decision.Survive(pnl, nc)

  /** Hybrid firm: attempt full-AI upgrade, else survive/downsize/bankrupt. */
  private def decideHybrid(
      firm: State,
      w: World,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
      workers: Int,
      aiEff: Double,
  )(using p: SimParams): Decision =
    val pnl    = computePnL(firm, w.hh.marketWage, w.flows.sectorDemandMult(firm.sector.toInt), w.priceLevel, lendRate, w.month)
    val ready2 = Ratio(Math.min(1.0, firm.digitalReadiness.toDouble + 0.005))

    val upCapex: PLN      = computeAiCapex(firm) * 0.6
    val upLoan: PLN       = upCapex * 0.85
    val upDown: PLN       = upCapex * 0.15
    val wMult             = effectiveWageMult(firm.sector)
    val upOpexSizeFactor  = Math.pow(firm.initialSize.toDouble / p.pop.workersPerFirm, 0.5)
    val upOtherSizeFactor = firm.initialSize.toDouble / p.pop.workersPerFirm
    val upCost: PLN       = p.firm.aiOpex * ((0.60 + 0.40 * w.priceLevel) * upOpexSizeFactor) +
      (firm.debt + upLoan) * (lendRate / 12.0) +
      w.hh.marketWage * (wMult * skeletonCrew(firm).toDouble) +
      p.firm.otherCosts * (w.priceLevel * upOtherSizeFactor)
    val profitable        = pnl.costs > upCost * 1.1
    val canPay            = firm.cash > upDown
    val ready             = firm.digitalReadiness >= p.firm.fullAiReadinessMin
    val bankOk            = bankCanLend(upLoan)

    val prob =
      if profitable && canPay && ready && bankOk then ((firm.riskProfile * 0.15 + w.real.automationRatio * 0.3) * firm.digitalReadiness).toDouble
      else 0.0

    if Random.nextDouble() < prob then
      val eff = 1.0 + Random.between(0.2, 0.6) * firm.digitalReadiness.toDouble
      Decision.Upgrade(pnl, TechState.Automated(eff), upCapex, upLoan, upDown, drUpdate = Some(ready2))
    else
      val nc = firm.cash + pnl.netAfterTax
      if nc < PLN.Zero then
        attemptDownsize(firm, pnl, nc, workers, w => TechState.Hybrid(w, aiEff), w.hh.marketWage, BankruptReason.HybridInsolvency, drUpdate = Some(ready2))
      else Decision.Survive(pnl, nc, drUpdate = Some(ready2))

  /** Traditional firm: evaluate full-AI, hybrid, downsize, digital invest, or
    * survive/bankrupt. Most complex decision path.
    */
  private def decideTraditional(
      firm: State,
      w: World,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
      allFirms: Vector[State],
      workers: Int,
  )(using p: SimParams): Decision =
    val pnl = computePnL(firm, w.hh.marketWage, w.flows.sectorDemandMult(firm.sector.toInt), w.priceLevel, lendRate, w.month)

    // Full AI
    val sWm              = effectiveWageMult(firm.sector)
    val fCapex: PLN      = computeAiCapex(firm)
    val fLoan: PLN       = fCapex * 0.85
    val fDown: PLN       = fCapex * 0.15
    val fOpexSizeFactor  = Math.pow(firm.initialSize.toDouble / p.pop.workersPerFirm, 0.5)
    val fOtherSizeFactor = firm.initialSize.toDouble / p.pop.workersPerFirm
    val fCost: PLN       = p.firm.aiOpex * ((0.60 + 0.40 * w.priceLevel) * fOpexSizeFactor) +
      (firm.debt + fLoan) * (lendRate / 12.0) +
      w.hh.marketWage * (sWm * skeletonCrew(firm).toDouble) +
      p.firm.otherCosts * (w.priceLevel * fOtherSizeFactor)
    val fProf            = pnl.costs > fCost * (1.1 / sigmaThreshold(w.currentSigmas(firm.sector.toInt)))
    val fPay             = firm.cash > fDown
    val fReady           = firm.digitalReadiness >= p.firm.fullAiReadinessMin
    val fBank            = bankCanLend(fLoan)

    // Hybrid -- sector-specific worker retention
    val hCapex: PLN      = computeHybridCapex(firm)
    val hLoan: PLN       = hCapex * 0.80
    val hDown: PLN       = hCapex * 0.20
    val hWkrs            = Math.max(3, (workers * SectorDefs(firm.sector.toInt).hybridRetainFrac.toDouble).toInt)
    val hOpexSizeFactor  = Math.pow(firm.initialSize.toDouble / p.pop.workersPerFirm, 0.5)
    val hOtherSizeFactor = firm.initialSize.toDouble / p.pop.workersPerFirm
    val hCost: PLN       =
      w.hh.marketWage * (sWm * hWkrs.toDouble) +
        p.firm.hybridOpex * ((0.60 + 0.40 * w.priceLevel) * hOpexSizeFactor) +
        (firm.debt + hLoan) * (lendRate / 12.0) +
        p.firm.otherCosts * (w.priceLevel * hOtherSizeFactor)
    val hProf            = pnl.costs > hCost * (1.05 / sigmaThreshold(w.currentSigmas(firm.sector.toInt)))
    val hPay             = firm.cash > hDown
    val hReady           = firm.digitalReadiness >= p.firm.hybridReadinessMin
    val hBank            = bankCanLend(hLoan)

    // Network-aware mimetic pressure: blend local + global with moderate weights
    val localAuto   = computeLocalAutoRatio(firm, allFirms)
    val globalPanic = (w.real.automationRatio + w.real.hybridRatio * 0.5).toDouble * 0.5
    val panic       = localAuto * 0.4 + globalPanic * 0.4 // Balanced local/global
    val desper      = if pnl.netAfterTax < PLN.Zero then 0.2 else 0.0
    val strat       =
      if !fProf && fPay && fReady && fBank then (firm.riskProfile * firm.digitalReadiness).toDouble * 0.005
      else 0.0

    // Uncertainty discount with network demonstration effect
    val baseDiscount        =
      0.15 + 0.15 * (w.month.toDouble / p.timeline.duration.toDouble)
    // Network demonstration: if many neighbors automated, reduce hesitation
    val demoBoost           =
      if localAuto > p.firm.demoEffectThresh.toDouble then p.firm.demoEffectBoost.toDouble * (localAuto - p.firm.demoEffectThresh.toDouble)
      else 0.0
    val uncertaintyDiscount = Math.min(1.0, baseDiscount + demoBoost)

    val pFull = uncertaintyDiscount *
      (if fProf && fPay && fReady && fBank then (firm.riskProfile.toDouble * 0.1 + panic + desper) * firm.digitalReadiness.toDouble
       else strat)

    val pHyb = uncertaintyDiscount *
      (if hProf && hPay && hReady && hBank then (firm.riskProfile.toDouble * 0.04 + panic * 0.5 + desper * 0.5) * firm.digitalReadiness.toDouble
       else 0.0)

    val canReduce = workers > 3 && pnl.netAfterTax < PLN.Zero
    val roll      = Random.nextDouble()

    if roll < pFull then
      val failRate = 0.05 + (Ratio.One - firm.digitalReadiness).toDouble * 0.10
      if Random.nextDouble() < failRate then Decision.UpgradeFailed(pnl, BankruptReason.AiImplFailure, fCapex * 0.5, fLoan * 0.3, fDown * 0.5)
      else
        val eff = 1.0 + Random.between(0.05, 0.6) * firm.digitalReadiness.toDouble
        Decision.Upgrade(pnl, TechState.Automated(eff), fCapex, fLoan, fDown)
    else if roll < pFull + pHyb then
      val failRate = 0.03 + (Ratio.One - firm.digitalReadiness).toDouble * 0.07
      val ir       = Random.nextDouble()
      if ir < failRate * 0.4 then Decision.UpgradeFailed(pnl, BankruptReason.HybridImplFailure, hCapex * 0.5, hLoan * 0.3, hDown * 0.5)
      else if ir < failRate then
        val badEff = 0.85 + Random.between(0.0, 0.20)
        Decision.Upgrade(pnl, TechState.Hybrid(hWkrs, badEff), hCapex, hLoan, hDown)
      else
        val goodEff = 1.0 + (0.05 + Random.between(0.0, 0.15)) *
          (0.5 + (firm.digitalReadiness * 0.5).toDouble)
        Decision.Upgrade(pnl, TechState.Hybrid(hWkrs, goodEff), hCapex, hLoan, hDown)
    else if canReduce && Random.nextDouble() < 0.10 then
      val reductionAmt = Math.max(1, (workers * 0.05).toInt)
      val newWkrs      = Math.max(3, workers - reductionAmt)
      Decision.Downsize(pnl, newWkrs, firm.cash + pnl.netAfterTax, TechState.Traditional(newWkrs))
    else
      // Digital investment attempt
      val digiCost: PLN = computeDigiInvestCost(firm)
      val nc            = firm.cash + pnl.netAfterTax
      val canAfford     = nc > digiCost * 2.0
      val competitive   = (w.real.automationRatio + w.real.hybridRatio * 0.5).toDouble
      val diminishing   = (Ratio.One - firm.digitalReadiness).toDouble
      val digiProb      = (p.firm.digiInvestBaseProb * firm.riskProfile).toDouble *
        diminishing * (0.5 + competitive)
      if canAfford && Random.nextDouble() < digiProb then
        val boost = p.firm.digiInvestBoost.toDouble * diminishing
        val newDR = Ratio(Math.min(1.0, firm.digitalReadiness.toDouble + boost))
        Decision.DigiInvest(pnl, digiCost, newDR)
      else if nc < PLN.Zero then attemptDownsize(firm, pnl, nc, workers, TechState.Traditional(_), w.hh.marketWage, BankruptReason.LaborCostInsolvency)
      else Decision.Survive(pnl, nc)

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
          cash = firm.cash + pnl.netAfterTax - downPayment,
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
            cash = firm.cash + pnl.netAfterTax - down,
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
      firm = firm,
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
    )

  // ---- Post-processing pipeline ----

  /** Apply natural digital drift to all living firms (always-on). */
  private def applyDigitalDrift(r: Result)(using p: SimParams): Result =
    if p.firm.digiDrift <= Ratio.Zero then return r
    val f     = r.firm
    if !isAlive(f) then return r
    val newDR = Ratio(Math.min(1.0, f.digitalReadiness.toDouble + p.firm.digiDrift.toDouble))
    r.copy(firm = f.copy(digitalReadiness = newDR))

  /** Apply physical capital investment after firm decision. Depreciation,
    * replacement + expansion investment, cash-constrained.
    */
  private def applyInvestment(r: Result)(using p: SimParams): Result =
    if !p.flags.physCap then return r
    val f          = r.firm
    if !isAlive(f) then return r.copy(firm = f.copy(capitalStock = PLN.Zero))
    val depRate    = p.capital.depRates(f.sector.toInt) / 12.0
    val depn: PLN  = f.capitalStock * depRate
    val postDepK   = f.capitalStock - depn
    val targetK    = p.capital.klRatios(f.sector.toInt) * workerCount(f).toDouble
    val gap        = (targetK - postDepK).max(PLN.Zero)
    val desiredInv = depn + gap * p.capital.adjustSpeed
    val actualInv  = desiredInv.min(f.cash.max(PLN.Zero))
    val newK       = postDepK + actualInv
    r.copy(firm = f.copy(cash = f.cash - actualInv, capitalStock = newK), grossInvestment = actualInv)

  // ---- PnL computation ----

  /** Monthly P&L: revenue (capacity × demand × price) minus labor, other costs,
    * depreciation, AI/hybrid opex, interest, inventory carrying, energy/ETS,
    * FDI profit shifting. CIT on positive profit.
    */
  private def computePnL(
      firm: State,
      wage: PLN,
      sectorDemandMult: Double,
      price: Double,
      lendRate: Rate,
      month: Int,
  )(using p: SimParams): PnL =
    val revenue: PLN         = computeCapacity(firm) * (sectorDemandMult * price)
    val labor: PLN           = wage * workerCount(firm).toDouble * effectiveWageMult(firm.sector)
    val sizeFactor           = firm.initialSize.toDouble / p.pop.workersPerFirm
    val rawOther: PLN        = p.firm.otherCosts * (price * sizeFactor)
    val depnCost: PLN        =
      if p.flags.physCap then firm.capitalStock * (p.capital.depRates(firm.sector.toInt) / 12.0)
      else PLN.Zero
    val otherAfterCap        = if p.flags.physCap then rawOther * (Ratio.One - p.capital.costReplace) else rawOther
    // Reduce other costs when energy is explicit
    val other                = if p.flags.energy then otherAfterCap * (Ratio.One - p.climate.energyCostReplace) else otherAfterCap
    // AI/hybrid opex is partially imported (40%) -- not fully domestic price-sensitive
    // OPEX scales sublinearly with firm size (exponent 0.5)
    val opexSizeFactor       = Math.pow(firm.initialSize.toDouble / p.pop.workersPerFirm, 0.5)
    val aiMaint: PLN         = firm.tech match
      case _: TechState.Automated => p.firm.aiOpex * ((0.60 + 0.40 * price) * opexSizeFactor)
      case _: TechState.Hybrid    => p.firm.hybridOpex * ((0.60 + 0.40 * price) * opexSizeFactor)
      case _                      => PLN.Zero
    val interest: PLN        = (firm.debt + firm.bondDebt) * (lendRate / 12.0)
    // Inventory carrying cost: storage, insurance, obsolescence
    val inventoryCost: PLN   =
      if p.flags.inventory then firm.inventory * (p.capital.inventoryCarryingCost / 12.0) else PLN.Zero
    // Reduce other costs when inventory is explicit
    val otherAfterInv        = if p.flags.inventory then other * (Ratio.One - p.capital.inventoryCostReplace) else other
    // Energy cost + EU ETS carbon surcharge
    val energyCost: PLN      = if p.flags.energy then
      val baseEnergy: PLN      = revenue * p.climate.energyCostShares(firm.sector.toInt)
      val etsPrice             = p.climate.etsBasePrice * Math.pow(1.0 + p.climate.etsPriceDrift.toDouble / 12.0, month.toDouble)
      val carbonSurcharge      = p.climate.carbonIntensity(firm.sector.toInt) * (etsPrice / p.climate.etsBasePrice - 1.0)
      val greenDiscount: Ratio = if firm.greenCapital > PLN.Zero then
        val targetGK = p.climate.greenKLRatios(firm.sector.toInt) * workerCount(firm).toDouble
        if targetGK > PLN.Zero then p.climate.greenMaxDiscount * Math.min(1.0, firm.greenCapital / targetGK)
        else Ratio.Zero
      else Ratio.Zero
      baseEnergy * ((1.0 + Math.max(0.0, carbonSurcharge)) * (Ratio.One - greenDiscount).toDouble)
    else PLN.Zero
    val prePsCosts           = labor + otherAfterInv + depnCost + aiMaint + interest + inventoryCost + energyCost
    val grossProfit          = revenue - prePsCosts
    val profitShiftCost: PLN =
      if p.flags.fdi && firm.foreignOwned then grossProfit.max(PLN.Zero) * p.fdi.profitShiftRate
      else PLN.Zero
    val costs                = prePsCosts + profitShiftCost
    val profit               = revenue - costs
    val tax: PLN             = profit.max(PLN.Zero) * p.fiscal.citRate
    PnL(revenue, costs, tax, profit - tax, profitShiftCost, energyCost)

  /** Apply green capital investment — separate cash pool. Firms earmark
    * GreenBudgetShare of cash for green investment; physical capital
    * (applyInvestment) uses the remainder.
    */
  private def applyGreenInvestment(r: Result)(using p: SimParams): Result =
    if !p.flags.energy then return r
    val f           = r.firm
    if !isAlive(f) then return r.copy(firm = f.copy(greenCapital = PLN.Zero))
    val depRate     = p.climate.greenDepRate / 12.0
    val depn: PLN   = f.greenCapital * depRate
    val postDepGK   = f.greenCapital - depn
    val targetGK    = p.climate.greenKLRatios(f.sector.toInt) * workerCount(f).toDouble
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
    val salesValue            = productionValue * Math.min(1.0, sectorDemandMult)
    val unsoldValue           = (productionValue - salesValue).max(PLN.Zero)
    // Spoilage
    val spoilRate             = p.capital.inventorySpoilageRates(f.sector.toInt) / 12.0
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
  private def effectiveShadowShare(sector: SectorIdx, cyclicalAdj: Double)(using p: SimParams): Ratio =
    Ratio(Math.min(1.0, p.informal.sectorShares.map(_.toDouble)(sector.toInt) + cyclicalAdj))

  /** CIT evasion fraction for a sector — shadow share × CIT evasion rate. */
  private def citEvasionFrac(sector: SectorIdx, cyclicalAdj: Double)(using p: SimParams): Ratio =
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
      if p.fiscal.citRate > Rate.Zero && r.taxPaid > PLN.Zero then r.taxPaid * (Rate(1.0) / p.fiscal.citRate - 1.0)
      else PLN.Zero
    val repatriation: PLN   =
      (afterTaxProfit.max(PLN.Zero) * p.fdi.repatriationRate).min(r.firm.cash.max(PLN.Zero))
    if repatriation <= PLN.Zero then return r
    r.copy(firm = r.firm.copy(cash = r.firm.cash - repatriation), fdiRepatriation = repatriation)
