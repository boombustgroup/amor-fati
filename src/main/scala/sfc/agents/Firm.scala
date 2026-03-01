package sfc.agents

import sfc.config.{Config, SECTORS, RunConfig}
import sfc.networks.Network
import sfc.engine.World

import scala.util.Random

// ---- Domain types ----

sealed trait TechState
object TechState:
  case class Traditional(workers: Int) extends TechState
  case class Hybrid(workers: Int, aiEfficiency: Double) extends TechState
  case class Automated(efficiency: Double) extends TechState
  case class Bankrupt(reason: String) extends TechState

case class Firm(
  id: Int,
  cash: Double,
  debt: Double,
  tech: TechState,
  riskProfile: Double,
  innovationCostFactor: Double,
  digitalReadiness: Double,
  sector: Int,              // Index into SECTORS
  neighbors: Array[Int],    // Network adjacency (firm IDs)
  bankId: Int = 0,          // Multi-bank: index into BankingSectorState.banks
  equityRaised: Double = 0.0, // GPW: cumulative equity raised via IPO/SPO
  initialSize: Int = 10     // Firm size at creation (v6.0: heterogeneous when FIRM_SIZE_DIST=gus)
)

object FirmOps:
  def isAlive(f: Firm): Boolean = f.tech match
    case _: TechState.Bankrupt => false
    case _                     => true

  def workers(f: Firm): Int = f.tech match
    case TechState.Traditional(w) => w
    case TechState.Hybrid(w, _)   => w
    case _: TechState.Automated   => skeletonCrew(f)
    case _: TechState.Bankrupt    => 0

  /** Skeleton crew for automated firms — scales with firm size. */
  def skeletonCrew(f: Firm): Int =
    Math.max(Config.AutoSkeletonCrew, (f.initialSize * 0.02).toInt)

  def capacity(f: Firm): Double =
    val sec = SECTORS(f.sector)
    val sizeScale = f.initialSize.toDouble / Config.WorkersPerFirm
    f.tech match
      case TechState.Traditional(w) =>
        Config.BaseRevenue * sizeScale * sec.revenueMultiplier *
          Math.sqrt(w.toDouble / f.initialSize)
      case TechState.Hybrid(w, eff) =>
        Config.BaseRevenue * sizeScale * sec.revenueMultiplier *
          (0.4 * Math.sqrt(w.toDouble / f.initialSize) + 0.6 * eff)
      case TechState.Automated(eff) =>
        Config.BaseRevenue * sizeScale * sec.revenueMultiplier * eff
      case _: TechState.Bankrupt => 0.0

  /** Effective AI CAPEX for sector — sublinear in firm size (exponent 0.6). */
  def aiCapex(f: Firm): Double =
    val sizeFactor = Math.pow(f.initialSize.toDouble / Config.WorkersPerFirm, 0.6)
    Config.AiCapex * SECTORS(f.sector).aiCapexMultiplier * f.innovationCostFactor * sizeFactor
  def hybridCapex(f: Firm): Double =
    val sizeFactor = Math.pow(f.initialSize.toDouble / Config.WorkersPerFirm, 0.6)
    Config.HybridCapex * SECTORS(f.sector).hybridCapexMultiplier * f.innovationCostFactor * sizeFactor

  /** sigma-based threshold modifier: high sigma sectors find automation profitable at lower cost gap.
   *  Only used for profitability threshold, NOT for probability multiplier.
   *  Mapping: sigma=2->0.91, sigma=5->0.95, sigma=10->0.98, sigma=50->1.00
   *  At equilibrium P~1.1: Manufacturing marginal, Healthcare blocked. */
  def sigmaThreshold(sigma: Double): Double =
    Math.min(1.0, 0.88 + 0.075 * Math.log(sigma) / Math.log(10.0))

// ---- Firm step result ----

case class FirmResult(firm: Firm, taxPaid: Double, capexSpent: Double,
  techImports: Double, newLoan: Double, equityIssuance: Double = 0.0)

// ---- Firm decision logic ----

object FirmLogic:
  private def calcPnL(firm: Firm, wage: Double, demandMult: Double,
    price: Double, lendRate: Double): (Double, Double, Double) =
    val revenue = FirmOps.capacity(firm) * demandMult * price
    val labor   = FirmOps.workers(firm) * wage * SECTORS(firm.sector).wageMultiplier
    val sizeFactor = firm.initialSize.toDouble / Config.WorkersPerFirm
    val other   = Config.OtherCosts * price * sizeFactor
    // AI/hybrid opex is partially imported (40%) -- not fully domestic price-sensitive
    // OPEX scales sublinearly with firm size (exponent 0.5)
    val opexSizeFactor = Math.pow(firm.initialSize.toDouble / Config.WorkersPerFirm, 0.5)
    val aiMaint = firm.tech match
      case _: TechState.Automated => Config.AiOpex * (0.60 + 0.40 * price) * opexSizeFactor
      case _: TechState.Hybrid    => Config.HybridOpex * (0.60 + 0.40 * price) * opexSizeFactor
      case _                      => 0.0
    val interest = firm.debt * (lendRate / 12.0)
    val costs    = labor + other + aiMaint + interest
    val profit   = revenue - costs
    val tax      = Math.max(0.0, profit) * Config.CitRate
    (revenue, costs, profit - tax)

  def process(firm: Firm, w: World, lendRate: Double,
    bankCanLend: Double => Boolean,
    allFirms: Array[Firm],
    rc: RunConfig): FirmResult =

    firm.tech match
      case _: TechState.Bankrupt =>
        FirmResult(firm, 0, 0, 0, 0)

      case _: TechState.Automated =>
        val (rev, costs, net) = calcPnL(firm, w.hh.marketWage, w.demandMultiplier, w.priceLevel, lendRate)
        val tax = Math.max(0.0, rev - costs) * Config.CitRate
        val nc  = firm.cash + net
        if nc < 0 then
          FirmResult(firm.copy(cash = nc, tech = TechState.Bankrupt("Pulapka plynnosci (dlug AI)")), tax, 0, 0, 0)
        else
          FirmResult(firm.copy(cash = nc), tax, 0, 0, 0)

      case TechState.Hybrid(wkrs, aiEff) =>
        val (rev, costs, net) = calcPnL(firm, w.hh.marketWage, w.demandMultiplier, w.priceLevel, lendRate)
        val tax = Math.max(0.0, rev - costs) * Config.CitRate
        val ready2 = Math.min(1.0, firm.digitalReadiness + 0.005)

        val upSizeFactor = Math.pow(firm.initialSize.toDouble / Config.WorkersPerFirm, 0.6)
        val upCapex = Config.AiCapex * SECTORS(firm.sector).aiCapexMultiplier *
          firm.innovationCostFactor * 0.6 * upSizeFactor
        val upLoan  = upCapex * 0.85
        val upDown  = upCapex * 0.15
        val wMult   = SECTORS(firm.sector).wageMultiplier
        val upOpexSizeFactor = Math.pow(firm.initialSize.toDouble / Config.WorkersPerFirm, 0.5)
        val upOtherSizeFactor = firm.initialSize.toDouble / Config.WorkersPerFirm
        val upCost  = Config.AiOpex * (0.60 + 0.40 * w.priceLevel) * upOpexSizeFactor +
          (firm.debt + upLoan) * (lendRate / 12.0) +
          FirmOps.skeletonCrew(firm) * w.hh.marketWage * wMult + Config.OtherCosts * w.priceLevel * upOtherSizeFactor
        val profitable = costs > upCost * 1.1
        val canPay     = firm.cash > upDown
        val ready      = firm.digitalReadiness >= Config.FullAiReadinessMin
        val bankOk     = bankCanLend(upLoan)

        val prob = if profitable && canPay && ready && bankOk then
          ((firm.riskProfile * 0.15) + (w.automationRatio * 0.3)) * firm.digitalReadiness
        else 0.0

        if Random.nextDouble() < prob then
          val eff  = 1.0 + Random.between(0.2, 0.6) * firm.digitalReadiness
          val tImp = upCapex * Config.TechImportShare
          FirmResult(
            firm.copy(tech = TechState.Automated(eff), debt = firm.debt + upLoan,
              cash = firm.cash + net - upDown, digitalReadiness = ready2),
            tax, upCapex, tImp, upLoan)
        else
          val nc = firm.cash + net
          if nc < 0 then
            FirmResult(firm.copy(cash = nc, tech = TechState.Bankrupt("Niewyplacalnosc (hybryda)")),
              tax, 0, 0, 0)
          else
            FirmResult(firm.copy(cash = nc, digitalReadiness = ready2), tax, 0, 0, 0)

      case TechState.Traditional(wkrs) =>
        val (rev, costs, net) = calcPnL(firm, w.hh.marketWage, w.demandMultiplier, w.priceLevel, lendRate)
        val tax = Math.max(0.0, rev - costs) * Config.CitRate

        // Full AI
        val sWm    = SECTORS(firm.sector).wageMultiplier
        val fCapex = FirmOps.aiCapex(firm)
        val fLoan  = fCapex * 0.85
        val fDown  = fCapex * 0.15
        val fOpexSizeFactor = Math.pow(firm.initialSize.toDouble / Config.WorkersPerFirm, 0.5)
        val fOtherSizeFactor = firm.initialSize.toDouble / Config.WorkersPerFirm
        val fCost  = Config.AiOpex * (0.60 + 0.40 * w.priceLevel) * fOpexSizeFactor +
          (firm.debt + fLoan) * (lendRate / 12.0) +
          FirmOps.skeletonCrew(firm) * w.hh.marketWage * sWm + Config.OtherCosts * w.priceLevel * fOtherSizeFactor
        val fProf  = costs > fCost * (1.1 / FirmOps.sigmaThreshold(w.currentSigmas(firm.sector)))
        val fPay   = firm.cash > fDown
        val fReady = firm.digitalReadiness >= Config.FullAiReadinessMin
        val fBank  = bankCanLend(fLoan)

        // Hybrid -- sector-specific worker retention
        val hCapex = FirmOps.hybridCapex(firm)
        val hLoan  = hCapex * 0.80
        val hDown  = hCapex * 0.20
        val hWkrs  = Math.max(3, (wkrs * SECTORS(firm.sector).hybridRetainFrac).toInt)
        val hOpexSizeFactor = Math.pow(firm.initialSize.toDouble / Config.WorkersPerFirm, 0.5)
        val hOtherSizeFactor = firm.initialSize.toDouble / Config.WorkersPerFirm
        val hCost  = hWkrs * w.hh.marketWage * sWm + Config.HybridOpex * (0.60 + 0.40 * w.priceLevel) * hOpexSizeFactor +
          (firm.debt + hLoan) * (lendRate / 12.0) + Config.OtherCosts * w.priceLevel * hOtherSizeFactor
        val hProf  = costs > hCost * (1.05 / FirmOps.sigmaThreshold(w.currentSigmas(firm.sector)))
        val hPay   = firm.cash > hDown
        val hReady = firm.digitalReadiness >= Config.HybridReadinessMin
        val hBank  = bankCanLend(hLoan)

        // Network-aware mimetic pressure: blend local + global with moderate weights
        val localAuto = Network.localAutoRatio(firm, allFirms)
        val globalPanic = (w.automationRatio + w.hybridRatio * 0.5) * 0.5
        val panic  = localAuto * 0.4 + globalPanic * 0.4  // Balanced local/global
        val desper = if net < 0 then 0.2 else 0.0
        val strat  = if !fProf && fPay && fReady && fBank then
          firm.riskProfile * 0.005 * firm.digitalReadiness else 0.0

        // Uncertainty discount with network demonstration effect
        val baseDiscount = if rc.isNoBdp then
          // No BDP: gradual natural competitive pressure
          0.15 + 0.15 * (w.month.toDouble / Config.Duration.toDouble)
        else
          if w.month < Config.ShockMonth then 0.15 else 1.0
        // Network demonstration: if many neighbors automated, reduce hesitation
        val demoBoost = if localAuto > Config.DemoEffectThresh then
          Config.DemoEffectBoost * (localAuto - Config.DemoEffectThresh)
        else 0.0
        val uncertaintyDiscount = Math.min(1.0, baseDiscount + demoBoost)

        val pFull = uncertaintyDiscount *
          (if fProf && fPay && fReady && fBank then
            ((firm.riskProfile * 0.1) + panic + desper) * firm.digitalReadiness
          else strat)

        val pHyb = uncertaintyDiscount *
          (if hProf && hPay && hReady && hBank then
            ((firm.riskProfile * 0.04) + (panic * 0.5) + (desper * 0.5)) * firm.digitalReadiness
          else 0.0)

        val canReduce = wkrs > 3 && net < 0
        val roll = Random.nextDouble()

        if roll < pFull then
          val failRate = 0.05 + (1.0 - firm.digitalReadiness) * 0.10
          val tImp = fCapex * Config.TechImportShare
          if Random.nextDouble() < failRate then
            FirmResult(firm.copy(cash = firm.cash + net - fDown * 0.5,
              debt = firm.debt + fLoan * 0.3,
              tech = TechState.Bankrupt("Porazka implementacji AI")),
              tax, fCapex * 0.5, tImp * 0.5, fLoan * 0.3)
          else
            val eff = 1.0 + Random.between(0.05, 0.6) * firm.digitalReadiness
            FirmResult(firm.copy(tech = TechState.Automated(eff),
              debt = firm.debt + fLoan, cash = firm.cash + net - fDown),
              tax, fCapex, tImp, fLoan)

        else if roll < pFull + pHyb then
          val failRate = 0.03 + (1.0 - firm.digitalReadiness) * 0.07
          val tImp = hCapex * Config.TechImportShare
          val ir = Random.nextDouble()
          if ir < failRate * 0.4 then
            FirmResult(firm.copy(cash = firm.cash + net - hDown * 0.5,
              debt = firm.debt + hLoan * 0.3,
              tech = TechState.Bankrupt("Porazka implementacji hybrydy")),
              tax, hCapex * 0.5, tImp * 0.5, hLoan * 0.3)
          else if ir < failRate then
            val badEff = 0.85 + Random.between(0.0, 0.20)
            FirmResult(firm.copy(tech = TechState.Hybrid(hWkrs, badEff),
              debt = firm.debt + hLoan, cash = firm.cash + net - hDown),
              tax, hCapex, tImp, hLoan)
          else
            val goodEff = 1.0 + (0.05 + Random.between(0.0, 0.15)) *
              (0.5 + firm.digitalReadiness * 0.5)
            FirmResult(firm.copy(tech = TechState.Hybrid(hWkrs, goodEff),
              debt = firm.debt + hLoan, cash = firm.cash + net - hDown),
              tax, hCapex, tImp, hLoan)

        else if canReduce && Random.nextDouble() < 0.10 then
          val reductionAmt = Math.max(1, (wkrs * 0.05).toInt)
          FirmResult(firm.copy(tech = TechState.Traditional(Math.max(3, wkrs - reductionAmt)),
            cash = firm.cash + net), tax, 0, 0, 0)

        else
          val nc = firm.cash + net
          if nc < 0 then
            FirmResult(firm.copy(cash = nc,
              tech = TechState.Bankrupt("Niewyplacalnosc (koszty pracy)")), tax, 0, 0, 0)
          else
            FirmResult(firm.copy(cash = nc), tax, 0, 0, 0)
