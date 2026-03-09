package sfc.agents

import sfc.config.{RunConfig, SectorDefs, SimParams}
import sfc.engine.World

import sfc.types.*

import scala.util.Random

// ---- Domain types ----

sealed trait BankruptReason
object BankruptReason:
  case object AiDebtTrap          extends BankruptReason
  case object HybridInsolvency    extends BankruptReason
  case object AiImplFailure       extends BankruptReason
  case object HybridImplFailure   extends BankruptReason
  case object LaborCostInsolvency extends BankruptReason
  case class Other(msg: String)   extends BankruptReason

sealed trait TechState

object TechState:
  case class Traditional(workers: Int)                  extends TechState
  case class Hybrid(workers: Int, aiEfficiency: Double) extends TechState
  case class Automated(efficiency: Double)              extends TechState
  case class Bankrupt(reason: BankruptReason)           extends TechState

object Firm:

  // ---- Data types ----

  case class State(
      id: FirmId,
      cash: PLN,
      debt: PLN,
      tech: TechState,
      riskProfile: Ratio,
      innovationCostFactor: Double,
      digitalReadiness: Ratio,
      sector: SectorIdx,             // Index into SectorDefs
      neighbors: Array[FirmId],      // Network adjacency (firm IDs)
      bankId: BankId = BankId(0),    // Multi-bank: index into Banking.State.banks
      equityRaised: PLN = PLN.Zero,  // GPW: cumulative equity raised via IPO/SPO
      initialSize: Int = 10,         // Firm size at creation (v6.0: heterogeneous when FIRM_SIZE_DIST=gus)
      capitalStock: PLN = PLN.Zero,  // Physical capital stock (PLN), #31
      bondDebt: PLN = PLN.Zero,      // Outstanding corporate bond debt (#40)
      foreignOwned: Boolean = false, // FDI (#33)
      inventory: PLN = PLN.Zero,     // Inventory stock (PLN), #43
      greenCapital: PLN = PLN.Zero,  // Green capital stock (PLN), #36
  )

  case class Result(
      firm: State,
      taxPaid: PLN,
      capexSpent: PLN,
      techImports: PLN,
      newLoan: PLN,
      equityIssuance: PLN = PLN.Zero,
      grossInvestment: PLN = PLN.Zero,
      bondIssuance: PLN = PLN.Zero,
      profitShiftCost: PLN = PLN.Zero,
      fdiRepatriation: PLN = PLN.Zero,
      inventoryChange: PLN = PLN.Zero,
      citEvasion: PLN = PLN.Zero,
      energyCost: PLN = PLN.Zero,
      greenInvestment: PLN = PLN.Zero,
  )

  case class PnL(
      revenue: PLN,
      costs: PLN,
      tax: PLN,
      netAfterTax: PLN,
      profitShiftCost: PLN,
      energyCost: PLN,
  )
  object PnL:
    val zero: PnL = PnL(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

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

  def isAlive(f: State): Boolean = f.tech match
    case _: TechState.Bankrupt => false
    case _                     => true

  def workerCount(f: State)(using SimParams): Int = f.tech match
    case TechState.Traditional(w) => w
    case TechState.Hybrid(w, _)   => w
    case _: TechState.Automated   => skeletonCrew(f)
    case _: TechState.Bankrupt    => 0

  /** Skeleton crew for automated firms — scales with firm size. */
  def skeletonCrew(f: State)(using p: SimParams): Int =
    Math.max(p.firm.autoSkeletonCrew, (f.initialSize * 0.02).toInt)

  /** Effective wage multiplier including union wage premium (#44). */
  def effectiveWageMult(sectorIdx: SectorIdx)(using p: SimParams): Double =
    val base = SectorDefs(sectorIdx.toInt).wageMultiplier
    if p.flags.unions then base * (1.0 + p.labor.unionWagePremium.toDouble * p.labor.unionDensity.map(_.toDouble)(sectorIdx.toInt))
    else base

  def computeCapacity(f: State)(using p: SimParams): Double =
    val sec       = SectorDefs(f.sector.toInt)
    val sizeScale = f.initialSize.toDouble / p.pop.workersPerFirm
    val base      = f.tech match
      case TechState.Traditional(w) =>
        p.firm.baseRevenue.toDouble * sizeScale * sec.revenueMultiplier *
          Math.sqrt(w.toDouble / f.initialSize)
      case TechState.Hybrid(w, eff) =>
        p.firm.baseRevenue.toDouble * sizeScale * sec.revenueMultiplier *
          (0.4 * Math.sqrt(w.toDouble / f.initialSize) + 0.6 * eff)
      case TechState.Automated(eff) =>
        p.firm.baseRevenue.toDouble * sizeScale * sec.revenueMultiplier * eff
      case _: TechState.Bankrupt    => 0.0
    if p.flags.physCap && f.capitalStock > PLN.Zero && base > 0 then
      val targetK       = workerCount(f).toDouble * p.capital.klRatios.map(_.toDouble)(f.sector.toInt)
      val kRatio        = if targetK > 0 then f.capitalStock.toDouble / targetK else 1.0
      val capitalFactor = Math.pow(Math.min(2.0, Math.max(0.1, kRatio)), p.capital.prodElast.toDouble)
      base * capitalFactor
    else base

  /** Effective AI CAPEX for sector — sublinear in firm size (exponent 0.6),
    * digital readiness discount.
    */
  def computeAiCapex(f: State)(using p: SimParams): Double =
    val sizeFactor   = Math.pow(f.initialSize.toDouble / p.pop.workersPerFirm, 0.6)
    val digiDiscount = 1.0 - p.firm.digiCapexDiscount.toDouble * f.digitalReadiness.toDouble
    p.firm.aiCapex.toDouble * SectorDefs(
      f.sector.toInt,
    ).aiCapexMultiplier * f.innovationCostFactor * sizeFactor * digiDiscount

  def computeHybridCapex(f: State)(using p: SimParams): Double =
    val sizeFactor   = Math.pow(f.initialSize.toDouble / p.pop.workersPerFirm, 0.6)
    val digiDiscount = 1.0 - p.firm.digiCapexDiscount.toDouble * f.digitalReadiness.toDouble
    p.firm.hybridCapex.toDouble * SectorDefs(
      f.sector.toInt,
    ).hybridCapexMultiplier * f.innovationCostFactor * sizeFactor * digiDiscount

  /** Digital investment cost — sublinear in firm size (exponent 0.5). */
  def computeDigiInvestCost(f: State)(using p: SimParams): Double =
    val sizeFactor = Math.pow(f.initialSize.toDouble / p.pop.workersPerFirm, 0.5)
    p.firm.digiInvestCost.toDouble * sizeFactor

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

  def process(
      firm: State,
      w: World,
      lendRate: Double,
      bankCanLend: Double => Boolean,
      allFirms: Vector[State],
      rc: RunConfig,
  )(using p: SimParams): Result =

    val decision  = decide(firm, w, lendRate, bankCanLend, allFirms)
    val rawResult = execute(firm, decision)

    val sectorDemandMult = w.flows.sectorDemandMult(firm.sector.toInt)
    applyInformalCitEvasion(
      applyFdiFlows(
        applyInventory(applyDigitalDrift(applyInvestment(applyGreenInvestment(rawResult))), sectorDemandMult),
      ),
      w.mechanisms.informalCyclicalAdj,
    )

  // ---- Decide (all match logic + Random rolls) ----

  private def decide(
      firm: State,
      w: World,
      lendRate: Double,
      bankCanLend: Double => Boolean,
      allFirms: Vector[State],
  )(using p: SimParams): Decision =
    firm.tech match
      case _: TechState.Bankrupt         => Decision.StayBankrupt
      case _: TechState.Automated        => decideAutomated(firm, w, lendRate)
      case TechState.Hybrid(wkrs, aiEff) => decideHybrid(firm, w, lendRate, bankCanLend, wkrs, aiEff)
      case TechState.Traditional(wkrs)   => decideTraditional(firm, w, lendRate, bankCanLend, allFirms, wkrs)

  private def decideAutomated(
      firm: State,
      w: World,
      lendRate: Double,
  )(using p: SimParams): Decision =
    val pnl = computePnL(firm, w.hh.marketWage.toDouble, w.flows.sectorDemandMult(firm.sector.toInt), w.priceLevel, lendRate, w.month)
    val nc  = firm.cash + pnl.netAfterTax
    if nc < PLN.Zero then Decision.GoBankrupt(pnl, nc, BankruptReason.AiDebtTrap)
    else Decision.Survive(pnl, nc)

  private def decideHybrid(
      firm: State,
      w: World,
      lendRate: Double,
      bankCanLend: Double => Boolean,
      wkrs: Int,
      aiEff: Double,
  )(using p: SimParams): Decision =
    val pnl    = computePnL(firm, w.hh.marketWage.toDouble, w.flows.sectorDemandMult(firm.sector.toInt), w.priceLevel, lendRate, w.month)
    val ready2 = Ratio(Math.min(1.0, firm.digitalReadiness.toDouble + 0.005))

    val upSizeFactor      = Math.pow(firm.initialSize.toDouble / p.pop.workersPerFirm, 0.6)
    val upDigiDiscount    = 1.0 - p.firm.digiCapexDiscount.toDouble * firm.digitalReadiness.toDouble
    val upCapex           = p.firm.aiCapex.toDouble * SectorDefs(firm.sector.toInt).aiCapexMultiplier *
      firm.innovationCostFactor * 0.6 * upSizeFactor * upDigiDiscount
    val upLoan            = upCapex * 0.85
    val upDown            = upCapex * 0.15
    val wMult             = effectiveWageMult(firm.sector)
    val upOpexSizeFactor  = Math.pow(firm.initialSize.toDouble / p.pop.workersPerFirm, 0.5)
    val upOtherSizeFactor = firm.initialSize.toDouble / p.pop.workersPerFirm
    val upCost            = p.firm.aiOpex.toDouble * (0.60 + 0.40 * w.priceLevel) * upOpexSizeFactor +
      (firm.debt.toDouble + upLoan) * (lendRate / 12.0) +
      skeletonCrew(
        firm,
      ) * w.hh.marketWage.toDouble * wMult + p.firm.otherCosts.toDouble * w.priceLevel * upOtherSizeFactor
    val profitable        = pnl.costs.toDouble > upCost * 1.1
    val canPay            = firm.cash.toDouble > upDown
    val ready             = firm.digitalReadiness.toDouble >= p.firm.fullAiReadinessMin.toDouble
    val bankOk            = bankCanLend(upLoan)

    val prob =
      if profitable && canPay && ready && bankOk then
        ((firm.riskProfile.toDouble * 0.15) + (w.real.automationRatio.toDouble * 0.3)) * firm.digitalReadiness.toDouble
      else 0.0

    if Random.nextDouble() < prob then
      val eff = 1.0 + Random.between(0.2, 0.6) * firm.digitalReadiness.toDouble
      Decision.Upgrade(pnl, TechState.Automated(eff), PLN(upCapex), PLN(upLoan), PLN(upDown), drUpdate = Some(ready2))
    else
      val nc = firm.cash + pnl.netAfterTax
      if nc < PLN.Zero then
        // Forced downsizing for hybrid firms
        if wkrs > 3 then
          val laborPerWorker = w.hh.marketWage.toDouble * effectiveWageMult(firm.sector)
          val maxCut         = Math.max(1, (wkrs * 0.30).toInt)
          val newWkrs        = Math.max(3, wkrs - maxCut)
          val laborSaved     = (wkrs - newWkrs) * laborPerWorker
          val revRatio       = Math.sqrt(newWkrs.toDouble / wkrs.toDouble)
          val revLost        = pnl.revenue.toDouble * (1.0 - revRatio)
          val adjustedNc     = nc.toDouble + laborSaved - revLost
          if adjustedNc >= 0 then Decision.Downsize(pnl, newWkrs, PLN(adjustedNc), TechState.Hybrid(newWkrs, aiEff), drUpdate = Some(ready2))
          else Decision.GoBankrupt(pnl, nc, BankruptReason.HybridInsolvency)
        else Decision.GoBankrupt(pnl, nc, BankruptReason.HybridInsolvency)
      else Decision.Survive(pnl, nc, drUpdate = Some(ready2))

  private def decideTraditional(
      firm: State,
      w: World,
      lendRate: Double,
      bankCanLend: Double => Boolean,
      allFirms: Vector[State],
      wkrs: Int,
  )(using p: SimParams): Decision =
    val pnl = computePnL(firm, w.hh.marketWage.toDouble, w.flows.sectorDemandMult(firm.sector.toInt), w.priceLevel, lendRate, w.month)

    // Full AI
    val sWm              = effectiveWageMult(firm.sector)
    val fCapex           = computeAiCapex(firm)
    val fLoan            = fCapex * 0.85
    val fDown            = fCapex * 0.15
    val fOpexSizeFactor  = Math.pow(firm.initialSize.toDouble / p.pop.workersPerFirm, 0.5)
    val fOtherSizeFactor = firm.initialSize.toDouble / p.pop.workersPerFirm
    val fCost            = p.firm.aiOpex.toDouble * (0.60 + 0.40 * w.priceLevel) * fOpexSizeFactor +
      (firm.debt.toDouble + fLoan) * (lendRate / 12.0) +
      skeletonCrew(
        firm,
      ) * w.hh.marketWage.toDouble * sWm + p.firm.otherCosts.toDouble * w.priceLevel * fOtherSizeFactor
    val fProf            = pnl.costs.toDouble > fCost * (1.1 / sigmaThreshold(w.currentSigmas(firm.sector.toInt)))
    val fPay             = firm.cash.toDouble > fDown
    val fReady           = firm.digitalReadiness.toDouble >= p.firm.fullAiReadinessMin.toDouble
    val fBank            = bankCanLend(fLoan)

    // Hybrid -- sector-specific worker retention
    val hCapex           = computeHybridCapex(firm)
    val hLoan            = hCapex * 0.80
    val hDown            = hCapex * 0.20
    val hWkrs            = Math.max(3, (wkrs * SectorDefs(firm.sector.toInt).hybridRetainFrac.toDouble).toInt)
    val hOpexSizeFactor  = Math.pow(firm.initialSize.toDouble / p.pop.workersPerFirm, 0.5)
    val hOtherSizeFactor = firm.initialSize.toDouble / p.pop.workersPerFirm
    val hCost            =
      hWkrs * w.hh.marketWage.toDouble * sWm + p.firm.hybridOpex.toDouble * (0.60 + 0.40 * w.priceLevel) * hOpexSizeFactor +
        (firm.debt.toDouble + hLoan) * (lendRate / 12.0) + p.firm.otherCosts.toDouble * w.priceLevel * hOtherSizeFactor
    val hProf            = pnl.costs.toDouble > hCost * (1.05 / sigmaThreshold(w.currentSigmas(firm.sector.toInt)))
    val hPay             = firm.cash.toDouble > hDown
    val hReady           = firm.digitalReadiness.toDouble >= p.firm.hybridReadinessMin.toDouble
    val hBank            = bankCanLend(hLoan)

    // Network-aware mimetic pressure: blend local + global with moderate weights
    val localAuto   = computeLocalAutoRatio(firm, allFirms)
    val globalPanic = (w.real.automationRatio.toDouble + w.real.hybridRatio.toDouble * 0.5) * 0.5
    val panic       = localAuto * 0.4 + globalPanic * 0.4 // Balanced local/global
    val desper      = if pnl.netAfterTax < PLN.Zero then 0.2 else 0.0
    val strat       =
      if !fProf && fPay && fReady && fBank then firm.riskProfile.toDouble * 0.005 * firm.digitalReadiness.toDouble
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
      (if fProf && fPay && fReady && fBank then ((firm.riskProfile.toDouble * 0.1) + panic + desper) * firm.digitalReadiness.toDouble
       else strat)

    val pHyb = uncertaintyDiscount *
      (if hProf && hPay && hReady && hBank then ((firm.riskProfile.toDouble * 0.04) + (panic * 0.5) + (desper * 0.5)) * firm.digitalReadiness.toDouble
       else 0.0)

    val canReduce = wkrs > 3 && pnl.netAfterTax < PLN.Zero
    val roll      = Random.nextDouble()

    if roll < pFull then
      val failRate = 0.05 + (1.0 - firm.digitalReadiness.toDouble) * 0.10
      if Random.nextDouble() < failRate then Decision.UpgradeFailed(pnl, BankruptReason.AiImplFailure, PLN(fCapex * 0.5), PLN(fLoan * 0.3), PLN(fDown * 0.5))
      else
        val eff = 1.0 + Random.between(0.05, 0.6) * firm.digitalReadiness.toDouble
        Decision.Upgrade(pnl, TechState.Automated(eff), PLN(fCapex), PLN(fLoan), PLN(fDown))
    else if roll < pFull + pHyb then
      val failRate = 0.03 + (1.0 - firm.digitalReadiness.toDouble) * 0.07
      val ir       = Random.nextDouble()
      if ir < failRate * 0.4 then Decision.UpgradeFailed(pnl, BankruptReason.HybridImplFailure, PLN(hCapex * 0.5), PLN(hLoan * 0.3), PLN(hDown * 0.5))
      else if ir < failRate then
        val badEff = 0.85 + Random.between(0.0, 0.20)
        Decision.Upgrade(pnl, TechState.Hybrid(hWkrs, badEff), PLN(hCapex), PLN(hLoan), PLN(hDown))
      else
        val goodEff = 1.0 + (0.05 + Random.between(0.0, 0.15)) *
          (0.5 + firm.digitalReadiness.toDouble * 0.5)
        Decision.Upgrade(pnl, TechState.Hybrid(hWkrs, goodEff), PLN(hCapex), PLN(hLoan), PLN(hDown))
    else if canReduce && Random.nextDouble() < 0.10 then
      val reductionAmt = Math.max(1, (wkrs * 0.05).toInt)
      val newWkrs      = Math.max(3, wkrs - reductionAmt)
      Decision.Downsize(pnl, newWkrs, firm.cash + pnl.netAfterTax, TechState.Traditional(newWkrs))
    else
      // Digital investment attempt (always-on, #37)
      val digiCost    = computeDigiInvestCost(firm)
      val nc          = firm.cash + pnl.netAfterTax
      val canAfford   = nc.toDouble > digiCost * 2.0
      val competitive = w.real.automationRatio.toDouble + w.real.hybridRatio.toDouble * 0.5
      val diminishing = 1.0 - firm.digitalReadiness.toDouble
      val digiProb    = p.firm.digiInvestBaseProb.toDouble * firm.riskProfile.toDouble *
        diminishing * (0.5 + competitive)
      if canAfford && Random.nextDouble() < digiProb then
        val boost = p.firm.digiInvestBoost.toDouble * diminishing
        val newDR = Ratio(Math.min(1.0, firm.digitalReadiness.toDouble + boost))
        Decision.DigiInvest(pnl, PLN(digiCost), newDR)
      else if nc < PLN.Zero then
        // Forced downsizing before bankruptcy
        if wkrs > 3 then
          val laborPerWorker = w.hh.marketWage.toDouble * effectiveWageMult(firm.sector)
          // Max 30% cut per month, minimum 3 workers retained
          val maxCut         = Math.max(1, (wkrs * 0.30).toInt)
          val newWkrs        = Math.max(3, wkrs - maxCut)
          val laborSaved     = (wkrs - newWkrs) * laborPerWorker
          val revRatio       = Math.sqrt(newWkrs.toDouble / wkrs.toDouble)
          val revLost        = pnl.revenue.toDouble * (1.0 - revRatio)
          val adjustedNc     = nc.toDouble + laborSaved - revLost
          if adjustedNc >= 0 then Decision.Downsize(pnl, newWkrs, PLN(adjustedNc), TechState.Traditional(newWkrs))
          else Decision.GoBankrupt(pnl, nc, BankruptReason.LaborCostInsolvency)
        else Decision.GoBankrupt(pnl, nc, BankruptReason.LaborCostInsolvency)
      else Decision.Survive(pnl, nc)

  // ---- Execute (pure dispatch, zero Random calls) ----

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
        val tImp = capex.toDouble * p.forex.techImportShare.toDouble
        val f    = firm.copy(
          tech = newTech,
          debt = firm.debt + loan,
          cash = firm.cash + pnl.netAfterTax - downPayment,
        )
        buildResult(
          drUpdate.fold(f)(dr => f.copy(digitalReadiness = dr)),
          pnl,
          capex = capex,
          techImports = PLN(tImp),
          newLoan = loan,
        )

      case Decision.UpgradeFailed(pnl, reason, capex, loan, down) =>
        val tImp = capex.toDouble * p.forex.techImportShare.toDouble
        buildResult(
          firm.copy(
            cash = firm.cash + pnl.netAfterTax - down,
            debt = firm.debt + loan,
            tech = TechState.Bankrupt(reason),
          ),
          pnl,
          capex = capex,
          techImports = PLN(tImp),
          newLoan = loan,
        )

      case Decision.Downsize(pnl, _, adjustedCash, newTech, drUpdate) =>
        val f = firm.copy(cash = adjustedCash, tech = newTech)
        buildResult(drUpdate.fold(f)(dr => f.copy(digitalReadiness = dr)), pnl)

      case Decision.DigiInvest(pnl, cost, newDR) =>
        val nc = firm.cash + pnl.netAfterTax
        buildResult(firm.copy(cash = nc - cost, digitalReadiness = newDR), pnl)

  private def buildResult(
      firm: State,
      pnl: PnL,
      capex: PLN = PLN.Zero,
      techImports: PLN = PLN.Zero,
      newLoan: PLN = PLN.Zero,
  ): Result =
    Result(firm, pnl.tax, capex, techImports, newLoan, profitShiftCost = pnl.profitShiftCost, energyCost = pnl.energyCost)

  // ---- Post-processing pipeline ----

  /** Apply natural digital drift to all living firms (always-on). */
  private def applyDigitalDrift(r: Result)(using p: SimParams): Result =
    if p.firm.digiDrift.toDouble <= 0.0 then return r
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
    val depRate    = p.capital.depRates.map(_.toDouble)(f.sector.toInt) / 12.0
    val depn       = (f.capitalStock * depRate).toDouble
    val postDepK   = f.capitalStock.toDouble - depn
    val targetK    = workerCount(f).toDouble * p.capital.klRatios.map(_.toDouble)(f.sector.toInt)
    val gap        = Math.max(0.0, targetK - postDepK)
    val desiredInv = depn + gap * p.capital.adjustSpeed.toDouble
    val actualInv  = Math.min(desiredInv, Math.max(0.0, f.cash.toDouble))
    val newK       = postDepK + actualInv
    r.copy(firm = f.copy(cash = f.cash - PLN(actualInv), capitalStock = PLN(newK)), grossInvestment = PLN(actualInv))

  // ---- PnL computation ----

  private def computePnL(
      firm: State,
      wage: Double,
      sectorDemandMult: Double,
      price: Double,
      lendRate: Double,
      month: Int,
  )(using p: SimParams): PnL =
    val revenue         = computeCapacity(firm) * sectorDemandMult * price
    val labor           = workerCount(firm) * wage * effectiveWageMult(firm.sector)
    val sizeFactor      = firm.initialSize.toDouble / p.pop.workersPerFirm
    val rawOther        = p.firm.otherCosts.toDouble * price * sizeFactor
    val depnCost        =
      if p.flags.physCap then firm.capitalStock.toDouble * p.capital.depRates.map(_.toDouble)(firm.sector.toInt) / 12.0
      else 0.0
    val otherAfterCap   = if p.flags.physCap then rawOther * (1.0 - p.capital.costReplace.toDouble) else rawOther
    // Reduce other costs when energy is explicit (was implicit in OtherCosts)
    val other           = if p.flags.energy then otherAfterCap * (1.0 - p.climate.energyCostReplace.toDouble) else otherAfterCap
    // AI/hybrid opex is partially imported (40%) -- not fully domestic price-sensitive
    // OPEX scales sublinearly with firm size (exponent 0.5)
    val opexSizeFactor  = Math.pow(firm.initialSize.toDouble / p.pop.workersPerFirm, 0.5)
    val aiMaint         = firm.tech match
      case _: TechState.Automated => p.firm.aiOpex.toDouble * (0.60 + 0.40 * price) * opexSizeFactor
      case _: TechState.Hybrid    => p.firm.hybridOpex.toDouble * (0.60 + 0.40 * price) * opexSizeFactor
      case _                      => 0.0
    val interest        = (firm.debt + firm.bondDebt).toDouble * (lendRate / 12.0)
    // Inventory carrying cost: storage, insurance, obsolescence (previously implicit in OtherCosts)
    val inventoryCost   =
      if p.flags.inventory then firm.inventory.toDouble * p.capital.inventoryCarryingCost.toDouble / 12.0 else 0.0
    // Reduce other costs when inventory is explicit (was implicit in OtherCosts)
    val otherAfterInv   = if p.flags.inventory then other * (1.0 - p.capital.inventoryCostReplace.toDouble) else other
    // Energy cost + EU ETS carbon surcharge (#36)
    val energyCost      = if p.flags.energy then
      val baseEnergy      = revenue * p.climate.energyCostShares.map(_.toDouble)(firm.sector.toInt)
      val etsPrice        = p.climate.etsBasePrice * Math.pow(1.0 + p.climate.etsPriceDrift.toDouble / 12.0, month.toDouble)
      val carbonSurcharge = p.climate.carbonIntensity(firm.sector.toInt) * (etsPrice / p.climate.etsBasePrice - 1.0)
      val greenDiscount   = if firm.greenCapital > PLN.Zero then
        val targetGK = workerCount(firm).toDouble * p.climate.greenKLRatios.map(_.toDouble)(firm.sector.toInt)
        if targetGK > 0 then
          Math.min(
            p.climate.greenMaxDiscount.toDouble,
            firm.greenCapital.toDouble / targetGK * p.climate.greenMaxDiscount.toDouble,
          )
        else 0.0
      else 0.0
      baseEnergy * (1.0 + Math.max(0.0, carbonSurcharge)) * (1.0 - greenDiscount)
    else 0.0
    val prePsCosts      = labor + otherAfterInv + depnCost + aiMaint + interest + inventoryCost + energyCost
    val grossProfit     = revenue - prePsCosts
    val profitShiftCost =
      if p.flags.fdi && firm.foreignOwned then Math.max(0.0, grossProfit) * p.fdi.profitShiftRate.toDouble
      else 0.0
    val costs           = prePsCosts + profitShiftCost
    val profit          = revenue - costs
    val tax             = Math.max(0.0, profit) * p.fiscal.citRate.toDouble
    PnL(PLN(revenue), PLN(costs), PLN(tax), PLN(profit - tax), PLN(profitShiftCost), PLN(energyCost))

  /** Apply green capital investment — separate cash pool (#36). Firms earmark
    * GreenBudgetShare of cash for green investment; physical capital
    * (applyInvestment) uses the remainder.
    */
  private def applyGreenInvestment(r: Result)(using p: SimParams): Result =
    if !p.flags.energy then return r
    val f           = r.firm
    if !isAlive(f) then return r.copy(firm = f.copy(greenCapital = PLN.Zero))
    val depRate     = p.climate.greenDepRate.toDouble / 12.0
    val depn        = (f.greenCapital * depRate).toDouble
    val postDepGK   = f.greenCapital.toDouble - depn
    val targetGK    = workerCount(f).toDouble * p.climate.greenKLRatios.map(_.toDouble)(f.sector.toInt)
    val gap         = Math.max(0.0, targetGK - postDepGK)
    val desiredInv  = depn + gap * p.climate.greenAdjustSpeed.toDouble
    val greenBudget = Math.max(0.0, f.cash.toDouble) * p.climate.greenBudgetShare.toDouble
    val actualInv   = Math.min(desiredInv, greenBudget)
    val newGK       = postDepGK + actualInv
    r.copy(firm = f.copy(cash = f.cash - PLN(actualInv), greenCapital = PLN(newGK)), greenInvestment = PLN(actualInv))

  /** Apply inventory accumulation/drawdown after firm decision (#43). */
  private def applyInventory(r: Result, sectorDemandMult: Double)(using p: SimParams): Result =
    if !p.flags.inventory then return r
    val f                     = r.firm
    if !isAlive(f) then return r.copy(firm = f.copy(inventory = PLN.Zero))
    val cap                   = computeCapacity(f).toDouble
    val productionValue       = cap * p.capital.inventoryCostFraction.toDouble
    val salesValue            = productionValue * Math.min(1.0, sectorDemandMult)
    val unsoldValue           = Math.max(0.0, productionValue - salesValue)
    // Spoilage
    val spoilRate             = p.capital.inventorySpoilageRates.map(_.toDouble)(f.sector.toInt) / 12.0
    val postSpoilage          = (f.inventory * (1.0 - spoilRate)).toDouble
    // Target-based adjustment
    val revenue               = cap * sectorDemandMult
    val targetInv             = revenue * p.capital.inventoryTargetRatios.map(_.toDouble)(f.sector.toInt)
    val desired               = (targetInv - postSpoilage) * p.capital.inventoryAdjustSpeed.toDouble
    // Accumulate unsold + adjust toward target
    val rawChange             = unsoldValue + desired
    // Can't draw down more than available
    val invChange             = Math.max(-postSpoilage, rawChange)
    val newInv                = Math.max(0.0, postSpoilage + invChange)
    // Stress liquidation: if cash < 0, sell inventory at discount
    val (finalInv, cashBoost) = if f.cash < PLN.Zero && newInv > 0.0 then
      val liquidate = Math.min(newInv, f.cash.abs.toDouble / p.capital.inventoryLiquidationDisc.toDouble)
      (newInv - liquidate, liquidate * p.capital.inventoryLiquidationDisc.toDouble)
    else (newInv, 0.0)
    val actualChange          = finalInv - f.inventory.toDouble
    r.copy(
      firm = f.copy(inventory = PLN(finalInv), cash = f.cash + PLN(cashBoost)),
      inventoryChange = PLN(actualChange),
    )

  /** Apply informal CIT evasion — firm keeps evaded tax (#45). */
  private def applyInformalCitEvasion(r: Result, cyclicalAdj: Double)(using p: SimParams): Result =
    if !p.flags.informal then return r
    val f               = r.firm
    if !isAlive(f) || r.taxPaid <= PLN.Zero then return r
    val baseShadow      = p.informal.sectorShares.map(_.toDouble)(f.sector.toInt)
    val effectiveShadow = Math.min(1.0, baseShadow + cyclicalAdj)
    val evasionFrac     = effectiveShadow * p.informal.citEvasion.toDouble
    val evaded          = r.taxPaid * evasionFrac
    r.copy(
      firm = f.copy(cash = f.cash + evaded),
      taxPaid = r.taxPaid - evaded,
      citEvasion = evaded,
    )

  /** Apply FDI dividend repatriation for foreign-owned firms (post-tax,
    * cash-constrained).
    */
  private def applyFdiFlows(r: Result)(using p: SimParams): Result =
    if !p.flags.fdi || !r.firm.foreignOwned || !isAlive(r.firm) then return r
    val afterTaxProfit =
      if p.fiscal.citRate.toDouble > 0 && r.taxPaid > PLN.Zero then r.taxPaid.toDouble * (1.0 - p.fiscal.citRate.toDouble) / p.fiscal.citRate.toDouble
      else 0.0
    val repatriation   =
      Math.min(Math.max(0.0, afterTaxProfit) * p.fdi.repatriationRate.toDouble, Math.max(0.0, r.firm.cash.toDouble))
    if repatriation <= 0.0 then return r
    r.copy(firm = r.firm.copy(cash = r.firm.cash - PLN(repatriation)), fdiRepatriation = PLN(repatriation))
