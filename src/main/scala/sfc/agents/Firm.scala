package sfc.agents

import sfc.config.{Config, SECTORS, RunConfig}
import sfc.networks.Network
import sfc.engine.World
import sfc.types.*

import scala.util.Random

// ---- Domain types ----

sealed trait TechState
object TechState:
  case class Traditional(workers: Int) extends TechState
  case class Hybrid(workers: Int, aiEfficiency: Double) extends TechState
  case class Automated(efficiency: Double) extends TechState
  case class Bankrupt(reason: String) extends TechState

case class Firm(
  id: FirmId,
  cash: Double,
  debt: Double,
  tech: TechState,
  riskProfile: Double,
  innovationCostFactor: Double,
  digitalReadiness: Double,
  sector: SectorIdx,          // Index into SECTORS
  neighbors: Array[Int],      // Network adjacency (firm IDs)
  bankId: BankId = BankId(0), // Multi-bank: index into BankingSectorState.banks
  equityRaised: Double = 0.0, // GPW: cumulative equity raised via IPO/SPO
  initialSize: Int = 10,    // Firm size at creation (v6.0: heterogeneous when FIRM_SIZE_DIST=gus)
  capitalStock: Double = 0.0, // Physical capital stock (PLN), #31
  bondDebt: Double = 0.0,    // Outstanding corporate bond debt (#40)
  foreignOwned: Boolean = false,  // FDI (#33)
  inventory: Double = 0.0,  // Inventory stock (PLN), #43
  greenCapital: Double = 0.0  // Green capital stock (PLN), #36
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

  /** Effective wage multiplier including union wage premium (#44). */
  def effectiveWageMult(sectorIdx: SectorIdx): Double =
    val base = SECTORS(sectorIdx.toInt).wageMultiplier
    if Config.UnionEnabled then base * (1.0 + Config.UnionWagePremium * Config.UnionDensity(sectorIdx.toInt))
    else base

  def capacity(f: Firm): Double =
    val sec = SECTORS(f.sector.toInt)
    val sizeScale = f.initialSize.toDouble / Config.WorkersPerFirm
    val base = f.tech match
      case TechState.Traditional(w) =>
        Config.BaseRevenue * sizeScale * sec.revenueMultiplier *
          Math.sqrt(w.toDouble / f.initialSize)
      case TechState.Hybrid(w, eff) =>
        Config.BaseRevenue * sizeScale * sec.revenueMultiplier *
          (0.4 * Math.sqrt(w.toDouble / f.initialSize) + 0.6 * eff)
      case TechState.Automated(eff) =>
        Config.BaseRevenue * sizeScale * sec.revenueMultiplier * eff
      case _: TechState.Bankrupt => 0.0
    if Config.PhysCapEnabled && f.capitalStock > 0 && base > 0 then
      val targetK = workers(f).toDouble * Config.PhysCapKLRatios(f.sector.toInt)
      val kRatio = if targetK > 0 then f.capitalStock / targetK else 1.0
      val capitalFactor = Math.pow(Math.min(2.0, Math.max(0.1, kRatio)), Config.PhysCapProdElast)
      base * capitalFactor
    else base

  /** Effective AI CAPEX for sector — sublinear in firm size (exponent 0.6), digital readiness discount. */
  def aiCapex(f: Firm): Double =
    val sizeFactor = Math.pow(f.initialSize.toDouble / Config.WorkersPerFirm, 0.6)
    val digiDiscount = 1.0 - Config.DigiCapexDiscount * f.digitalReadiness
    Config.AiCapex * SECTORS(f.sector.toInt).aiCapexMultiplier * f.innovationCostFactor * sizeFactor * digiDiscount
  def hybridCapex(f: Firm): Double =
    val sizeFactor = Math.pow(f.initialSize.toDouble / Config.WorkersPerFirm, 0.6)
    val digiDiscount = 1.0 - Config.DigiCapexDiscount * f.digitalReadiness
    Config.HybridCapex * SECTORS(f.sector.toInt).hybridCapexMultiplier * f.innovationCostFactor * sizeFactor * digiDiscount

  /** Digital investment cost — sublinear in firm size (exponent 0.5). */
  def digiInvestCost(f: Firm): Double =
    val sizeFactor = Math.pow(f.initialSize.toDouble / Config.WorkersPerFirm, 0.5)
    Config.DigiInvestCost * sizeFactor

  /** sigma-based threshold modifier: high sigma sectors find automation profitable at lower cost gap.
   *  Only used for profitability threshold, NOT for probability multiplier.
   *  Mapping: sigma=2->0.91, sigma=5->0.95, sigma=10->0.98, sigma=50->1.00
   *  At equilibrium P~1.1: Manufacturing marginal, Healthcare blocked. */
  def sigmaThreshold(sigma: Double): Double =
    Math.min(1.0, 0.88 + 0.075 * Math.log(sigma) / Math.log(10.0))

// ---- Firm step result ----

case class FirmResult(firm: Firm, taxPaid: Double, capexSpent: Double,
  techImports: Double, newLoan: Double, equityIssuance: Double = 0.0,
  grossInvestment: Double = 0.0, bondIssuance: Double = 0.0,
  profitShiftCost: Double = 0.0, fdiRepatriation: Double = 0.0,
  inventoryChange: Double = 0.0,
  citEvasion: Double = 0.0,
  energyCost: Double = 0.0,
  greenInvestment: Double = 0.0)

// ---- Firm decision logic ----

object FirmLogic:
  /** Apply natural digital drift to all living firms (always-on). */
  private def applyDigitalDrift(r: FirmResult): FirmResult =
    if Config.DigiDrift <= 0.0 then return r
    val f = r.firm
    if !FirmOps.isAlive(f) then return r
    val newDR = Math.min(1.0, f.digitalReadiness + Config.DigiDrift)
    r.copy(firm = f.copy(digitalReadiness = newDR))

  /** Apply physical capital investment after firm decision.
    * Depreciation, replacement + expansion investment, cash-constrained. */
  private def applyInvestment(r: FirmResult): FirmResult =
    if !Config.PhysCapEnabled then return r
    val f = r.firm
    if !FirmOps.isAlive(f) then return r.copy(firm = f.copy(capitalStock = 0.0))
    val depRate = Config.PhysCapDepRates(f.sector.toInt) / 12.0
    val depn = f.capitalStock * depRate
    val postDepK = f.capitalStock - depn
    val targetK = FirmOps.workers(f).toDouble * Config.PhysCapKLRatios(f.sector.toInt)
    val gap = Math.max(0.0, targetK - postDepK)
    val desiredInv = depn + gap * Config.PhysCapAdjustSpeed
    val actualInv = Math.min(desiredInv, Math.max(0.0, f.cash))
    val newK = postDepK + actualInv
    r.copy(firm = f.copy(cash = f.cash - actualInv, capitalStock = newK),
           grossInvestment = actualInv)

  private def calcPnL(firm: Firm, wage: Double, sectorDemandMult: Double,
    price: Double, lendRate: Double, month: Int): (Double, Double, Double, Double, Double) =
    val revenue = FirmOps.capacity(firm) * sectorDemandMult * price
    val labor   = FirmOps.workers(firm) * wage * FirmOps.effectiveWageMult(firm.sector)
    val sizeFactor = firm.initialSize.toDouble / Config.WorkersPerFirm
    val rawOther = Config.OtherCosts * price * sizeFactor
    val depnCost = if Config.PhysCapEnabled then
      firm.capitalStock * Config.PhysCapDepRates(firm.sector.toInt) / 12.0 else 0.0
    val otherAfterCap = if Config.PhysCapEnabled then
      rawOther * (1.0 - Config.PhysCapCostReplace) else rawOther
    // Reduce other costs when energy is explicit (was implicit in OtherCosts)
    val other = if Config.EnergyEnabled then
      otherAfterCap * (1.0 - Config.EnergyCostReplace) else otherAfterCap
    // AI/hybrid opex is partially imported (40%) -- not fully domestic price-sensitive
    // OPEX scales sublinearly with firm size (exponent 0.5)
    val opexSizeFactor = Math.pow(firm.initialSize.toDouble / Config.WorkersPerFirm, 0.5)
    val aiMaint = firm.tech match
      case _: TechState.Automated => Config.AiOpex * (0.60 + 0.40 * price) * opexSizeFactor
      case _: TechState.Hybrid    => Config.HybridOpex * (0.60 + 0.40 * price) * opexSizeFactor
      case _                      => 0.0
    val interest = (firm.debt + firm.bondDebt) * (lendRate / 12.0)
    // Inventory carrying cost: storage, insurance, obsolescence (previously implicit in OtherCosts)
    val inventoryCost = if Config.InventoryEnabled then
      firm.inventory * Config.InventoryCarryingCost / 12.0 else 0.0
    // Reduce other costs when inventory is explicit (was implicit in OtherCosts)
    val otherAfterInv = if Config.InventoryEnabled then
      other * (1.0 - Config.InventoryCostReplace) else other
    // Energy cost + EU ETS carbon surcharge (#36)
    val energyCost = if Config.EnergyEnabled then
      val baseEnergy = revenue * Config.EnergyCostShares(firm.sector.toInt)
      val etsPrice = Config.EtsBasePrice * Math.pow(1.0 + Config.EtsPriceDrift / 12.0, month.toDouble)
      val carbonSurcharge = Config.EnergyCarbonIntensity(firm.sector.toInt) * (etsPrice / Config.EtsBasePrice - 1.0)
      val greenDiscount = if firm.greenCapital > 0 then
        val targetGK = FirmOps.workers(firm).toDouble * Config.GreenKLRatios(firm.sector.toInt)
        if targetGK > 0 then Math.min(Config.GreenMaxDiscount, firm.greenCapital / targetGK * Config.GreenMaxDiscount)
        else 0.0
      else 0.0
      baseEnergy * (1.0 + Math.max(0.0, carbonSurcharge)) * (1.0 - greenDiscount)
    else 0.0
    val prePsCosts = labor + otherAfterInv + depnCost + aiMaint + interest + inventoryCost + energyCost
    val grossProfit = revenue - prePsCosts
    val profitShiftCost = if Config.FdiEnabled && firm.foreignOwned then
      Math.max(0.0, grossProfit) * Config.FdiProfitShiftRate
    else 0.0
    val costs    = prePsCosts + profitShiftCost
    val profit   = revenue - costs
    val tax      = Math.max(0.0, profit) * Config.CitRate
    (revenue, costs, profit - tax, profitShiftCost, energyCost)

  /** Apply green capital investment — separate cash pool (#36).
    * Firms earmark GreenBudgetShare of cash for green investment;
    * physical capital (applyInvestment) uses the remainder. */
  private def applyGreenInvestment(r: FirmResult): FirmResult =
    if !Config.EnergyEnabled then return r
    val f = r.firm
    if !FirmOps.isAlive(f) then return r.copy(firm = f.copy(greenCapital = 0.0))
    val depRate = Config.GreenDepRate / 12.0
    val depn = f.greenCapital * depRate
    val postDepGK = f.greenCapital - depn
    val targetGK = FirmOps.workers(f).toDouble * Config.GreenKLRatios(f.sector.toInt)
    val gap = Math.max(0.0, targetGK - postDepGK)
    val desiredInv = depn + gap * Config.GreenAdjustSpeed
    val greenBudget = Math.max(0.0, f.cash) * Config.GreenBudgetShare
    val actualInv = Math.min(desiredInv, greenBudget)
    val newGK = postDepGK + actualInv
    r.copy(firm = f.copy(cash = f.cash - actualInv, greenCapital = newGK),
           greenInvestment = actualInv)

  /** Apply inventory accumulation/drawdown after firm decision (#43). */
  private def applyInventory(r: FirmResult, sectorDemandMult: Double): FirmResult =
    if !Config.InventoryEnabled then return r
    val f = r.firm
    if !FirmOps.isAlive(f) then return r.copy(firm = f.copy(inventory = 0.0))
    val capacity = FirmOps.capacity(f).toDouble
    val productionValue = capacity * Config.InventoryCostFraction
    val salesValue = productionValue * Math.min(1.0, sectorDemandMult)
    val unsoldValue = Math.max(0.0, productionValue - salesValue)
    // Spoilage
    val spoilRate = Config.InventorySpoilageRates(f.sector.toInt) / 12.0
    val postSpoilage = f.inventory * (1.0 - spoilRate)
    // Target-based adjustment
    val revenue = capacity * sectorDemandMult
    val targetInv = revenue * Config.InventoryTargetRatios(f.sector.toInt)
    val desired = (targetInv - postSpoilage) * Config.InventoryAdjustSpeed
    // Accumulate unsold + adjust toward target
    val rawChange = unsoldValue + desired
    // Can't draw down more than available
    val invChange = Math.max(-postSpoilage, rawChange)
    val newInv = Math.max(0.0, postSpoilage + invChange)
    // Stress liquidation: if cash < 0, sell inventory at discount
    val (finalInv, cashBoost) = if f.cash < 0.0 && newInv > 0.0 then
      val liquidate = Math.min(newInv, Math.abs(f.cash) / Config.InventoryLiquidationDisc)
      (newInv - liquidate, liquidate * Config.InventoryLiquidationDisc)
    else (newInv, 0.0)
    val actualChange = finalInv - f.inventory
    r.copy(firm = f.copy(inventory = finalInv, cash = f.cash + cashBoost),
           inventoryChange = actualChange)

  def process(firm: Firm, w: World, lendRate: Double,
    bankCanLend: Double => Boolean,
    allFirms: Array[Firm],
    rc: RunConfig): FirmResult =

    val rawResult: FirmResult = firm.tech match
      case _: TechState.Bankrupt =>
        FirmResult(firm, 0, 0, 0, 0)

      case _: TechState.Automated =>
        val (rev, costs, net, psCost, eCost) = calcPnL(firm, w.hh.marketWage, w.sectorDemandMult(firm.sector.toInt), w.priceLevel, lendRate, w.month)
        val tax = Math.max(0.0, rev - costs) * Config.CitRate
        val nc  = firm.cash + net
        if nc < 0 then
          FirmResult(firm.copy(cash = nc, tech = TechState.Bankrupt("Pulapka plynnosci (dlug AI)")), tax, 0, 0, 0, profitShiftCost = psCost, energyCost = eCost)
        else
          FirmResult(firm.copy(cash = nc), tax, 0, 0, 0, profitShiftCost = psCost, energyCost = eCost)

      case TechState.Hybrid(wkrs, aiEff) =>
        val (rev, costs, net, psCost, eCost) = calcPnL(firm, w.hh.marketWage, w.sectorDemandMult(firm.sector.toInt), w.priceLevel, lendRate, w.month)
        val tax = Math.max(0.0, rev - costs) * Config.CitRate
        val ready2 = Math.min(1.0, firm.digitalReadiness + 0.005)

        val upSizeFactor = Math.pow(firm.initialSize.toDouble / Config.WorkersPerFirm, 0.6)
        val upDigiDiscount = 1.0 - Config.DigiCapexDiscount * firm.digitalReadiness
        val upCapex = Config.AiCapex * SECTORS(firm.sector.toInt).aiCapexMultiplier *
          firm.innovationCostFactor * 0.6 * upSizeFactor * upDigiDiscount
        val upLoan  = upCapex * 0.85
        val upDown  = upCapex * 0.15
        val wMult   = FirmOps.effectiveWageMult(firm.sector)
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
            tax, upCapex, tImp, upLoan, profitShiftCost = psCost, energyCost = eCost)
        else
          val nc = firm.cash + net
          if nc < 0 then
            // Forced downsizing for hybrid firms
            if wkrs > 3 then
              val laborPerWorker = w.hh.marketWage * FirmOps.effectiveWageMult(firm.sector)
              val maxCut = Math.max(1, (wkrs * 0.30).toInt)
              val newWkrs = Math.max(3, wkrs - maxCut)
              val laborSaved = (wkrs - newWkrs) * laborPerWorker
              val revRatio = Math.sqrt(newWkrs.toDouble / wkrs.toDouble)
              val revLost = rev * (1.0 - revRatio)
              val adjustedNc = nc + laborSaved - revLost
              if adjustedNc >= 0 then
                FirmResult(firm.copy(cash = adjustedNc,
                  tech = TechState.Hybrid(newWkrs, aiEff), digitalReadiness = ready2),
                  tax, 0, 0, 0, profitShiftCost = psCost, energyCost = eCost)
              else
                FirmResult(firm.copy(cash = nc, tech = TechState.Bankrupt("Niewyplacalnosc (hybryda)")),
                  tax, 0, 0, 0, profitShiftCost = psCost, energyCost = eCost)
            else
              FirmResult(firm.copy(cash = nc, tech = TechState.Bankrupt("Niewyplacalnosc (hybryda)")),
                tax, 0, 0, 0, profitShiftCost = psCost, energyCost = eCost)
          else
            FirmResult(firm.copy(cash = nc, digitalReadiness = ready2), tax, 0, 0, 0, profitShiftCost = psCost, energyCost = eCost)

      case TechState.Traditional(wkrs) =>
        val (rev, costs, net, psCost, eCost) = calcPnL(firm, w.hh.marketWage, w.sectorDemandMult(firm.sector.toInt), w.priceLevel, lendRate, w.month)
        val tax = Math.max(0.0, rev - costs) * Config.CitRate

        // Full AI
        val sWm    = FirmOps.effectiveWageMult(firm.sector)
        val fCapex = FirmOps.aiCapex(firm)
        val fLoan  = fCapex * 0.85
        val fDown  = fCapex * 0.15
        val fOpexSizeFactor = Math.pow(firm.initialSize.toDouble / Config.WorkersPerFirm, 0.5)
        val fOtherSizeFactor = firm.initialSize.toDouble / Config.WorkersPerFirm
        val fCost  = Config.AiOpex * (0.60 + 0.40 * w.priceLevel) * fOpexSizeFactor +
          (firm.debt + fLoan) * (lendRate / 12.0) +
          FirmOps.skeletonCrew(firm) * w.hh.marketWage * sWm + Config.OtherCosts * w.priceLevel * fOtherSizeFactor
        val fProf  = costs > fCost * (1.1 / FirmOps.sigmaThreshold(w.currentSigmas(firm.sector.toInt)))
        val fPay   = firm.cash > fDown
        val fReady = firm.digitalReadiness >= Config.FullAiReadinessMin
        val fBank  = bankCanLend(fLoan)

        // Hybrid -- sector-specific worker retention
        val hCapex = FirmOps.hybridCapex(firm)
        val hLoan  = hCapex * 0.80
        val hDown  = hCapex * 0.20
        val hWkrs  = Math.max(3, (wkrs * SECTORS(firm.sector.toInt).hybridRetainFrac).toInt)
        val hOpexSizeFactor = Math.pow(firm.initialSize.toDouble / Config.WorkersPerFirm, 0.5)
        val hOtherSizeFactor = firm.initialSize.toDouble / Config.WorkersPerFirm
        val hCost  = hWkrs * w.hh.marketWage * sWm + Config.HybridOpex * (0.60 + 0.40 * w.priceLevel) * hOpexSizeFactor +
          (firm.debt + hLoan) * (lendRate / 12.0) + Config.OtherCosts * w.priceLevel * hOtherSizeFactor
        val hProf  = costs > hCost * (1.05 / FirmOps.sigmaThreshold(w.currentSigmas(firm.sector.toInt)))
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
              tax, fCapex * 0.5, tImp * 0.5, fLoan * 0.3, profitShiftCost = psCost, energyCost = eCost)
          else
            val eff = 1.0 + Random.between(0.05, 0.6) * firm.digitalReadiness
            FirmResult(firm.copy(tech = TechState.Automated(eff),
              debt = firm.debt + fLoan, cash = firm.cash + net - fDown),
              tax, fCapex, tImp, fLoan, profitShiftCost = psCost, energyCost = eCost)

        else if roll < pFull + pHyb then
          val failRate = 0.03 + (1.0 - firm.digitalReadiness) * 0.07
          val tImp = hCapex * Config.TechImportShare
          val ir = Random.nextDouble()
          if ir < failRate * 0.4 then
            FirmResult(firm.copy(cash = firm.cash + net - hDown * 0.5,
              debt = firm.debt + hLoan * 0.3,
              tech = TechState.Bankrupt("Porazka implementacji hybrydy")),
              tax, hCapex * 0.5, tImp * 0.5, hLoan * 0.3, profitShiftCost = psCost, energyCost = eCost)
          else if ir < failRate then
            val badEff = 0.85 + Random.between(0.0, 0.20)
            FirmResult(firm.copy(tech = TechState.Hybrid(hWkrs, badEff),
              debt = firm.debt + hLoan, cash = firm.cash + net - hDown),
              tax, hCapex, tImp, hLoan, profitShiftCost = psCost, energyCost = eCost)
          else
            val goodEff = 1.0 + (0.05 + Random.between(0.0, 0.15)) *
              (0.5 + firm.digitalReadiness * 0.5)
            FirmResult(firm.copy(tech = TechState.Hybrid(hWkrs, goodEff),
              debt = firm.debt + hLoan, cash = firm.cash + net - hDown),
              tax, hCapex, tImp, hLoan, profitShiftCost = psCost, energyCost = eCost)

        else if canReduce && Random.nextDouble() < 0.10 then
          val reductionAmt = Math.max(1, (wkrs * 0.05).toInt)
          FirmResult(firm.copy(tech = TechState.Traditional(Math.max(3, wkrs - reductionAmt)),
            cash = firm.cash + net), tax, 0, 0, 0, profitShiftCost = psCost, energyCost = eCost)

        else
          // Digital investment attempt (always-on, #37)
          val digiCost = FirmOps.digiInvestCost(firm)
          val nc = firm.cash + net
          val canAfford = nc > digiCost * 2.0
          val competitive = w.automationRatio + w.hybridRatio * 0.5
          val diminishing = 1.0 - firm.digitalReadiness
          val digiProb = Config.DigiInvestBaseProb * firm.riskProfile *
            diminishing * (0.5 + competitive)
          if canAfford && Random.nextDouble() < digiProb then
            val boost = Config.DigiInvestBoost * diminishing
            val newDR = Math.min(1.0, firm.digitalReadiness + boost)
            FirmResult(firm.copy(cash = nc - digiCost, digitalReadiness = newDR),
              tax, 0, 0, 0, profitShiftCost = psCost, energyCost = eCost)
          else if nc < 0 then
            // Forced downsizing before bankruptcy (zwolnienia grupowe)
            // Firms shed workers to survive — realistic Polish labor code adjustment
            if wkrs > 3 then
              val laborPerWorker = w.hh.marketWage * FirmOps.effectiveWageMult(firm.sector)
              // Max 30% cut per month, minimum 3 workers retained
              val maxCut = Math.max(1, (wkrs * 0.30).toInt)
              val newWkrs = Math.max(3, wkrs - maxCut)
              val laborSaved = (wkrs - newWkrs) * laborPerWorker
              val revRatio = Math.sqrt(newWkrs.toDouble / wkrs.toDouble)
              val revLost = rev * (1.0 - revRatio)
              val adjustedNc = nc + laborSaved - revLost
              if adjustedNc >= 0 then
                FirmResult(firm.copy(cash = adjustedNc,
                  tech = TechState.Traditional(newWkrs)),
                  tax, 0, 0, 0, profitShiftCost = psCost, energyCost = eCost)
              else
                FirmResult(firm.copy(cash = nc,
                  tech = TechState.Bankrupt("Niewyplacalnosc (koszty pracy)")),
                  tax, 0, 0, 0, profitShiftCost = psCost, energyCost = eCost)
            else
              FirmResult(firm.copy(cash = nc,
                tech = TechState.Bankrupt("Niewyplacalnosc (koszty pracy)")),
                tax, 0, 0, 0, profitShiftCost = psCost, energyCost = eCost)
          else
            FirmResult(firm.copy(cash = nc), tax, 0, 0, 0, profitShiftCost = psCost, energyCost = eCost)

    val sectorDemandMult = w.sectorDemandMult(firm.sector.toInt)
    applyInformalCitEvasion(
      applyFdiFlows(applyInventory(applyDigitalDrift(applyInvestment(applyGreenInvestment(rawResult))), sectorDemandMult)),
      w.informalCyclicalAdj)

  /** Apply informal CIT evasion — firm keeps evaded tax (#45). */
  private def applyInformalCitEvasion(r: FirmResult, cyclicalAdj: Double): FirmResult =
    if !Config.InformalEnabled then return r
    val f = r.firm
    if !FirmOps.isAlive(f) || r.taxPaid <= 0.0 then return r
    val baseShadow = Config.InformalSectorShares(f.sector.toInt)
    val effectiveShadow = Math.min(1.0, baseShadow + cyclicalAdj)
    val evasionFrac = effectiveShadow * Config.InformalCitEvasion
    val evaded = r.taxPaid * evasionFrac
    r.copy(
      firm = f.copy(cash = f.cash + evaded),
      taxPaid = r.taxPaid - evaded,
      citEvasion = evaded
    )

  /** Apply FDI dividend repatriation for foreign-owned firms (post-tax, cash-constrained). */
  private def applyFdiFlows(r: FirmResult): FirmResult =
    if !Config.FdiEnabled || !r.firm.foreignOwned || !FirmOps.isAlive(r.firm) then return r
    val afterTaxProfit = if Config.CitRate > 0 && r.taxPaid > 0 then
      r.taxPaid * (1.0 - Config.CitRate) / Config.CitRate
    else 0.0
    val repatriation = Math.min(
      Math.max(0.0, afterTaxProfit) * Config.FdiRepatriationRate,
      Math.max(0.0, r.firm.cash))
    if repatriation <= 0.0 then return r
    r.copy(firm = r.firm.copy(cash = r.firm.cash - repatriation),
           fdiRepatriation = repatriation)
