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

object Firm:

  case class State(
    id: FirmId,
    cash: PLN,
    debt: PLN,
    tech: TechState,
    riskProfile: Ratio,
    innovationCostFactor: Double,
    digitalReadiness: Ratio,
    sector: SectorIdx,          // Index into SECTORS
    neighbors: Array[Int],      // Network adjacency (firm IDs)
    bankId: BankId = BankId(0), // Multi-bank: index into Banking.State.banks
    equityRaised: PLN = PLN.Zero, // GPW: cumulative equity raised via IPO/SPO
    initialSize: Int = 10,    // Firm size at creation (v6.0: heterogeneous when FIRM_SIZE_DIST=gus)
    capitalStock: PLN = PLN.Zero, // Physical capital stock (PLN), #31
    bondDebt: PLN = PLN.Zero,    // Outstanding corporate bond debt (#40)
    foreignOwned: Boolean = false,  // FDI (#33)
    inventory: PLN = PLN.Zero,  // Inventory stock (PLN), #43
    greenCapital: PLN = PLN.Zero  // Green capital stock (PLN), #36
  )

  // ---- Firm step result ----

  case class Result(firm: State, taxPaid: PLN, capexSpent: PLN,
    techImports: PLN, newLoan: PLN, equityIssuance: PLN = PLN.Zero,
    grossInvestment: PLN = PLN.Zero, bondIssuance: PLN = PLN.Zero,
    profitShiftCost: PLN = PLN.Zero, fdiRepatriation: PLN = PLN.Zero,
    inventoryChange: PLN = PLN.Zero,
    citEvasion: PLN = PLN.Zero,
    energyCost: PLN = PLN.Zero,
    greenInvestment: PLN = PLN.Zero)

  // ---- Ops ----

  def isAlive(f: State): Boolean = f.tech match
    case _: TechState.Bankrupt => false
    case _                     => true

  def workers(f: State): Int = f.tech match
    case TechState.Traditional(w) => w
    case TechState.Hybrid(w, _)   => w
    case _: TechState.Automated   => skeletonCrew(f)
    case _: TechState.Bankrupt    => 0

  /** Skeleton crew for automated firms — scales with firm size. */
  def skeletonCrew(f: State): Int =
    Math.max(Config.AutoSkeletonCrew, (f.initialSize * 0.02).toInt)

  /** Effective wage multiplier including union wage premium (#44). */
  def effectiveWageMult(sectorIdx: SectorIdx): Double =
    val base = SECTORS(sectorIdx.toInt).wageMultiplier
    if Config.UnionEnabled then base * (1.0 + Config.UnionWagePremium * Config.UnionDensity(sectorIdx.toInt))
    else base

  def capacity(f: State): Double =
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
    if Config.PhysCapEnabled && f.capitalStock > PLN.Zero && base > 0 then
      val targetK = workers(f).toDouble * Config.PhysCapKLRatios(f.sector.toInt)
      val kRatio = if targetK > 0 then f.capitalStock.toDouble / targetK else 1.0
      val capitalFactor = Math.pow(Math.min(2.0, Math.max(0.1, kRatio)), Config.PhysCapProdElast)
      base * capitalFactor
    else base

  /** Effective AI CAPEX for sector — sublinear in firm size (exponent 0.6), digital readiness discount. */
  def aiCapex(f: State): Double =
    val sizeFactor = Math.pow(f.initialSize.toDouble / Config.WorkersPerFirm, 0.6)
    val digiDiscount = 1.0 - Config.DigiCapexDiscount * f.digitalReadiness.toDouble
    Config.AiCapex * SECTORS(f.sector.toInt).aiCapexMultiplier * f.innovationCostFactor * sizeFactor * digiDiscount
  def hybridCapex(f: State): Double =
    val sizeFactor = Math.pow(f.initialSize.toDouble / Config.WorkersPerFirm, 0.6)
    val digiDiscount = 1.0 - Config.DigiCapexDiscount * f.digitalReadiness.toDouble
    Config.HybridCapex * SECTORS(f.sector.toInt).hybridCapexMultiplier * f.innovationCostFactor * sizeFactor * digiDiscount

  /** Digital investment cost — sublinear in firm size (exponent 0.5). */
  def digiInvestCost(f: State): Double =
    val sizeFactor = Math.pow(f.initialSize.toDouble / Config.WorkersPerFirm, 0.5)
    Config.DigiInvestCost * sizeFactor

  /** sigma-based threshold modifier: high sigma sectors find automation profitable at lower cost gap.
   *  Only used for profitability threshold, NOT for probability multiplier.
   *  Mapping: sigma=2->0.91, sigma=5->0.95, sigma=10->0.98, sigma=50->1.00
   *  At equilibrium P~1.1: Manufacturing marginal, Healthcare blocked. */
  def sigmaThreshold(sigma: Double): Double =
    Math.min(1.0, 0.88 + 0.075 * Math.log(sigma) / Math.log(10.0))

  // ---- Firm decision logic ----

  /** Apply natural digital drift to all living firms (always-on). */
  private def applyDigitalDrift(r: Result): Result =
    if Config.DigiDrift <= 0.0 then return r
    val f = r.firm
    if !isAlive(f) then return r
    val newDR = Ratio(Math.min(1.0, f.digitalReadiness.toDouble + Config.DigiDrift))
    r.copy(firm = f.copy(digitalReadiness = newDR))

  /** Apply physical capital investment after firm decision.
    * Depreciation, replacement + expansion investment, cash-constrained. */
  private def applyInvestment(r: Result): Result =
    if !Config.PhysCapEnabled then return r
    val f = r.firm
    if !isAlive(f) then return r.copy(firm = f.copy(capitalStock = PLN.Zero))
    val depRate = Config.PhysCapDepRates(f.sector.toInt) / 12.0
    val depn = (f.capitalStock * depRate).toDouble
    val postDepK = f.capitalStock.toDouble - depn
    val targetK = workers(f).toDouble * Config.PhysCapKLRatios(f.sector.toInt)
    val gap = Math.max(0.0, targetK - postDepK)
    val desiredInv = depn + gap * Config.PhysCapAdjustSpeed
    val actualInv = Math.min(desiredInv, Math.max(0.0, f.cash.toDouble))
    val newK = postDepK + actualInv
    r.copy(firm = f.copy(cash = f.cash - PLN(actualInv), capitalStock = PLN(newK)),
           grossInvestment = PLN(actualInv))

  private def calcPnL(firm: State, wage: Double, sectorDemandMult: Double,
    price: Double, lendRate: Double, month: Int): (Double, Double, Double, Double, Double) =
    val revenue = capacity(firm) * sectorDemandMult * price
    val labor   = workers(firm) * wage * effectiveWageMult(firm.sector)
    val sizeFactor = firm.initialSize.toDouble / Config.WorkersPerFirm
    val rawOther = Config.OtherCosts * price * sizeFactor
    val depnCost = if Config.PhysCapEnabled then
      firm.capitalStock.toDouble * Config.PhysCapDepRates(firm.sector.toInt) / 12.0 else 0.0
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
    val interest = (firm.debt + firm.bondDebt).toDouble * (lendRate / 12.0)
    // Inventory carrying cost: storage, insurance, obsolescence (previously implicit in OtherCosts)
    val inventoryCost = if Config.InventoryEnabled then
      firm.inventory.toDouble * Config.InventoryCarryingCost / 12.0 else 0.0
    // Reduce other costs when inventory is explicit (was implicit in OtherCosts)
    val otherAfterInv = if Config.InventoryEnabled then
      other * (1.0 - Config.InventoryCostReplace) else other
    // Energy cost + EU ETS carbon surcharge (#36)
    val energyCost = if Config.EnergyEnabled then
      val baseEnergy = revenue * Config.EnergyCostShares(firm.sector.toInt)
      val etsPrice = Config.EtsBasePrice * Math.pow(1.0 + Config.EtsPriceDrift / 12.0, month.toDouble)
      val carbonSurcharge = Config.EnergyCarbonIntensity(firm.sector.toInt) * (etsPrice / Config.EtsBasePrice - 1.0)
      val greenDiscount = if firm.greenCapital > PLN.Zero then
        val targetGK = workers(firm).toDouble * Config.GreenKLRatios(firm.sector.toInt)
        if targetGK > 0 then Math.min(Config.GreenMaxDiscount, firm.greenCapital.toDouble / targetGK * Config.GreenMaxDiscount)
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
  private def applyGreenInvestment(r: Result): Result =
    if !Config.EnergyEnabled then return r
    val f = r.firm
    if !isAlive(f) then return r.copy(firm = f.copy(greenCapital = PLN.Zero))
    val depRate = Config.GreenDepRate / 12.0
    val depn = (f.greenCapital * depRate).toDouble
    val postDepGK = f.greenCapital.toDouble - depn
    val targetGK = workers(f).toDouble * Config.GreenKLRatios(f.sector.toInt)
    val gap = Math.max(0.0, targetGK - postDepGK)
    val desiredInv = depn + gap * Config.GreenAdjustSpeed
    val greenBudget = Math.max(0.0, f.cash.toDouble) * Config.GreenBudgetShare
    val actualInv = Math.min(desiredInv, greenBudget)
    val newGK = postDepGK + actualInv
    r.copy(firm = f.copy(cash = f.cash - PLN(actualInv), greenCapital = PLN(newGK)),
           greenInvestment = PLN(actualInv))

  /** Apply inventory accumulation/drawdown after firm decision (#43). */
  private def applyInventory(r: Result, sectorDemandMult: Double): Result =
    if !Config.InventoryEnabled then return r
    val f = r.firm
    if !isAlive(f) then return r.copy(firm = f.copy(inventory = PLN.Zero))
    val cap = capacity(f).toDouble
    val productionValue = cap * Config.InventoryCostFraction
    val salesValue = productionValue * Math.min(1.0, sectorDemandMult)
    val unsoldValue = Math.max(0.0, productionValue - salesValue)
    // Spoilage
    val spoilRate = Config.InventorySpoilageRates(f.sector.toInt) / 12.0
    val postSpoilage = (f.inventory * (1.0 - spoilRate)).toDouble
    // Target-based adjustment
    val revenue = cap * sectorDemandMult
    val targetInv = revenue * Config.InventoryTargetRatios(f.sector.toInt)
    val desired = (targetInv - postSpoilage) * Config.InventoryAdjustSpeed
    // Accumulate unsold + adjust toward target
    val rawChange = unsoldValue + desired
    // Can't draw down more than available
    val invChange = Math.max(-postSpoilage, rawChange)
    val newInv = Math.max(0.0, postSpoilage + invChange)
    // Stress liquidation: if cash < 0, sell inventory at discount
    val (finalInv, cashBoost) = if f.cash < PLN.Zero && newInv > 0.0 then
      val liquidate = Math.min(newInv, f.cash.abs.toDouble / Config.InventoryLiquidationDisc)
      (newInv - liquidate, liquidate * Config.InventoryLiquidationDisc)
    else (newInv, 0.0)
    val actualChange = finalInv - f.inventory.toDouble
    r.copy(firm = f.copy(inventory = PLN(finalInv), cash = f.cash + PLN(cashBoost)),
           inventoryChange = PLN(actualChange))

  def process(firm: State, w: World, lendRate: Double,
    bankCanLend: Double => Boolean,
    allFirms: Array[State],
    rc: RunConfig): Result =

    val rawResult: Result = firm.tech match
      case _: TechState.Bankrupt =>
        Result(firm, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

      case _: TechState.Automated =>
        val (rev, costs, net, psCost, eCost) = calcPnL(firm, w.hh.marketWage.toDouble, w.sectorDemandMult(firm.sector.toInt), w.priceLevel, lendRate, w.month)
        val tax = Math.max(0.0, rev - costs) * Config.CitRate
        val nc  = firm.cash + PLN(net)
        if nc < PLN.Zero then
          Result(firm.copy(cash = nc, tech = TechState.Bankrupt("Pulapka plynnosci (dlug AI)")), PLN(tax), PLN.Zero, PLN.Zero, PLN.Zero, profitShiftCost = PLN(psCost), energyCost = PLN(eCost))
        else
          Result(firm.copy(cash = nc), PLN(tax), PLN.Zero, PLN.Zero, PLN.Zero, profitShiftCost = PLN(psCost), energyCost = PLN(eCost))

      case TechState.Hybrid(wkrs, aiEff) =>
        val (rev, costs, net, psCost, eCost) = calcPnL(firm, w.hh.marketWage.toDouble, w.sectorDemandMult(firm.sector.toInt), w.priceLevel, lendRate, w.month)
        val tax = Math.max(0.0, rev - costs) * Config.CitRate
        val ready2 = Ratio(Math.min(1.0, firm.digitalReadiness.toDouble + 0.005))

        val upSizeFactor = Math.pow(firm.initialSize.toDouble / Config.WorkersPerFirm, 0.6)
        val upDigiDiscount = 1.0 - Config.DigiCapexDiscount * firm.digitalReadiness.toDouble
        val upCapex = Config.AiCapex * SECTORS(firm.sector.toInt).aiCapexMultiplier *
          firm.innovationCostFactor * 0.6 * upSizeFactor * upDigiDiscount
        val upLoan  = upCapex * 0.85
        val upDown  = upCapex * 0.15
        val wMult   = effectiveWageMult(firm.sector)
        val upOpexSizeFactor = Math.pow(firm.initialSize.toDouble / Config.WorkersPerFirm, 0.5)
        val upOtherSizeFactor = firm.initialSize.toDouble / Config.WorkersPerFirm
        val upCost  = Config.AiOpex * (0.60 + 0.40 * w.priceLevel) * upOpexSizeFactor +
          (firm.debt.toDouble + upLoan) * (lendRate / 12.0) +
          skeletonCrew(firm) * w.hh.marketWage.toDouble * wMult + Config.OtherCosts * w.priceLevel * upOtherSizeFactor
        val profitable = costs > upCost * 1.1
        val canPay     = firm.cash.toDouble > upDown
        val ready      = firm.digitalReadiness.toDouble >= Config.FullAiReadinessMin
        val bankOk     = bankCanLend(upLoan)

        val prob = if profitable && canPay && ready && bankOk then
          ((firm.riskProfile.toDouble * 0.15) + (w.automationRatio.toDouble * 0.3)) * firm.digitalReadiness.toDouble
        else 0.0

        if Random.nextDouble() < prob then
          val eff  = 1.0 + Random.between(0.2, 0.6) * firm.digitalReadiness.toDouble
          val tImp = upCapex * Config.TechImportShare
          Result(
            firm.copy(tech = TechState.Automated(eff), debt = firm.debt + PLN(upLoan),
              cash = firm.cash + PLN(net) - PLN(upDown), digitalReadiness = ready2),
            PLN(tax), PLN(upCapex), PLN(tImp), PLN(upLoan), profitShiftCost = PLN(psCost), energyCost = PLN(eCost))
        else
          val nc = firm.cash + PLN(net)
          if nc < PLN.Zero then
            // Forced downsizing for hybrid firms
            if wkrs > 3 then
              val laborPerWorker = w.hh.marketWage.toDouble * effectiveWageMult(firm.sector)
              val maxCut = Math.max(1, (wkrs * 0.30).toInt)
              val newWkrs = Math.max(3, wkrs - maxCut)
              val laborSaved = (wkrs - newWkrs) * laborPerWorker
              val revRatio = Math.sqrt(newWkrs.toDouble / wkrs.toDouble)
              val revLost = rev * (1.0 - revRatio)
              val adjustedNc = nc.toDouble + laborSaved - revLost
              if adjustedNc >= 0 then
                Result(firm.copy(cash = PLN(adjustedNc),
                  tech = TechState.Hybrid(newWkrs, aiEff), digitalReadiness = ready2),
                  PLN(tax), PLN.Zero, PLN.Zero, PLN.Zero, profitShiftCost = PLN(psCost), energyCost = PLN(eCost))
              else
                Result(firm.copy(cash = nc, tech = TechState.Bankrupt("Niewyplacalnosc (hybryda)")),
                  PLN(tax), PLN.Zero, PLN.Zero, PLN.Zero, profitShiftCost = PLN(psCost), energyCost = PLN(eCost))
            else
              Result(firm.copy(cash = nc, tech = TechState.Bankrupt("Niewyplacalnosc (hybryda)")),
                PLN(tax), PLN.Zero, PLN.Zero, PLN.Zero, profitShiftCost = PLN(psCost), energyCost = PLN(eCost))
          else
            Result(firm.copy(cash = nc, digitalReadiness = ready2), PLN(tax), PLN.Zero, PLN.Zero, PLN.Zero, profitShiftCost = PLN(psCost), energyCost = PLN(eCost))

      case TechState.Traditional(wkrs) =>
        val (rev, costs, net, psCost, eCost) = calcPnL(firm, w.hh.marketWage.toDouble, w.sectorDemandMult(firm.sector.toInt), w.priceLevel, lendRate, w.month)
        val tax = Math.max(0.0, rev - costs) * Config.CitRate

        // Full AI
        val sWm    = effectiveWageMult(firm.sector)
        val fCapex = aiCapex(firm)
        val fLoan  = fCapex * 0.85
        val fDown  = fCapex * 0.15
        val fOpexSizeFactor = Math.pow(firm.initialSize.toDouble / Config.WorkersPerFirm, 0.5)
        val fOtherSizeFactor = firm.initialSize.toDouble / Config.WorkersPerFirm
        val fCost  = Config.AiOpex * (0.60 + 0.40 * w.priceLevel) * fOpexSizeFactor +
          (firm.debt.toDouble + fLoan) * (lendRate / 12.0) +
          skeletonCrew(firm) * w.hh.marketWage.toDouble * sWm + Config.OtherCosts * w.priceLevel * fOtherSizeFactor
        val fProf  = costs > fCost * (1.1 / sigmaThreshold(w.currentSigmas(firm.sector.toInt)))
        val fPay   = firm.cash.toDouble > fDown
        val fReady = firm.digitalReadiness.toDouble >= Config.FullAiReadinessMin
        val fBank  = bankCanLend(fLoan)

        // Hybrid -- sector-specific worker retention
        val hCapex = hybridCapex(firm)
        val hLoan  = hCapex * 0.80
        val hDown  = hCapex * 0.20
        val hWkrs  = Math.max(3, (wkrs * SECTORS(firm.sector.toInt).hybridRetainFrac.toDouble).toInt)
        val hOpexSizeFactor = Math.pow(firm.initialSize.toDouble / Config.WorkersPerFirm, 0.5)
        val hOtherSizeFactor = firm.initialSize.toDouble / Config.WorkersPerFirm
        val hCost  = hWkrs * w.hh.marketWage.toDouble * sWm + Config.HybridOpex * (0.60 + 0.40 * w.priceLevel) * hOpexSizeFactor +
          (firm.debt.toDouble + hLoan) * (lendRate / 12.0) + Config.OtherCosts * w.priceLevel * hOtherSizeFactor
        val hProf  = costs > hCost * (1.05 / sigmaThreshold(w.currentSigmas(firm.sector.toInt)))
        val hPay   = firm.cash.toDouble > hDown
        val hReady = firm.digitalReadiness.toDouble >= Config.HybridReadinessMin
        val hBank  = bankCanLend(hLoan)

        // Network-aware mimetic pressure: blend local + global with moderate weights
        val localAuto = Network.localAutoRatio(firm, allFirms)
        val globalPanic = (w.automationRatio.toDouble + w.hybridRatio.toDouble * 0.5) * 0.5
        val panic  = localAuto * 0.4 + globalPanic * 0.4  // Balanced local/global
        val desper = if net < 0 then 0.2 else 0.0
        val strat  = if !fProf && fPay && fReady && fBank then
          firm.riskProfile.toDouble * 0.005 * firm.digitalReadiness.toDouble else 0.0

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
            ((firm.riskProfile.toDouble * 0.1) + panic + desper) * firm.digitalReadiness.toDouble
          else strat)

        val pHyb = uncertaintyDiscount *
          (if hProf && hPay && hReady && hBank then
            ((firm.riskProfile.toDouble * 0.04) + (panic * 0.5) + (desper * 0.5)) * firm.digitalReadiness.toDouble
          else 0.0)

        val canReduce = wkrs > 3 && net < 0
        val roll = Random.nextDouble()

        if roll < pFull then
          val failRate = 0.05 + (1.0 - firm.digitalReadiness.toDouble) * 0.10
          val tImp = fCapex * Config.TechImportShare
          if Random.nextDouble() < failRate then
            Result(firm.copy(cash = firm.cash + PLN(net) - PLN(fDown * 0.5),
              debt = firm.debt + PLN(fLoan * 0.3),
              tech = TechState.Bankrupt("Porazka implementacji AI")),
              PLN(tax), PLN(fCapex * 0.5), PLN(tImp * 0.5), PLN(fLoan * 0.3), profitShiftCost = PLN(psCost), energyCost = PLN(eCost))
          else
            val eff = 1.0 + Random.between(0.05, 0.6) * firm.digitalReadiness.toDouble
            Result(firm.copy(tech = TechState.Automated(eff),
              debt = firm.debt + PLN(fLoan), cash = firm.cash + PLN(net) - PLN(fDown)),
              PLN(tax), PLN(fCapex), PLN(tImp), PLN(fLoan), profitShiftCost = PLN(psCost), energyCost = PLN(eCost))

        else if roll < pFull + pHyb then
          val failRate = 0.03 + (1.0 - firm.digitalReadiness.toDouble) * 0.07
          val tImp = hCapex * Config.TechImportShare
          val ir = Random.nextDouble()
          if ir < failRate * 0.4 then
            Result(firm.copy(cash = firm.cash + PLN(net) - PLN(hDown * 0.5),
              debt = firm.debt + PLN(hLoan * 0.3),
              tech = TechState.Bankrupt("Porazka implementacji hybrydy")),
              PLN(tax), PLN(hCapex * 0.5), PLN(tImp * 0.5), PLN(hLoan * 0.3), profitShiftCost = PLN(psCost), energyCost = PLN(eCost))
          else if ir < failRate then
            val badEff = 0.85 + Random.between(0.0, 0.20)
            Result(firm.copy(tech = TechState.Hybrid(hWkrs, badEff),
              debt = firm.debt + PLN(hLoan), cash = firm.cash + PLN(net) - PLN(hDown)),
              PLN(tax), PLN(hCapex), PLN(tImp), PLN(hLoan), profitShiftCost = PLN(psCost), energyCost = PLN(eCost))
          else
            val goodEff = 1.0 + (0.05 + Random.between(0.0, 0.15)) *
              (0.5 + firm.digitalReadiness.toDouble * 0.5)
            Result(firm.copy(tech = TechState.Hybrid(hWkrs, goodEff),
              debt = firm.debt + PLN(hLoan), cash = firm.cash + PLN(net) - PLN(hDown)),
              PLN(tax), PLN(hCapex), PLN(tImp), PLN(hLoan), profitShiftCost = PLN(psCost), energyCost = PLN(eCost))

        else if canReduce && Random.nextDouble() < 0.10 then
          val reductionAmt = Math.max(1, (wkrs * 0.05).toInt)
          Result(firm.copy(tech = TechState.Traditional(Math.max(3, wkrs - reductionAmt)),
            cash = firm.cash + PLN(net)), PLN(tax), PLN.Zero, PLN.Zero, PLN.Zero, profitShiftCost = PLN(psCost), energyCost = PLN(eCost))

        else
          // Digital investment attempt (always-on, #37)
          val digiCost = digiInvestCost(firm)
          val nc = firm.cash + PLN(net)
          val canAfford = nc.toDouble > digiCost * 2.0
          val competitive = w.automationRatio.toDouble + w.hybridRatio.toDouble * 0.5
          val diminishing = 1.0 - firm.digitalReadiness.toDouble
          val digiProb = Config.DigiInvestBaseProb * firm.riskProfile.toDouble *
            diminishing * (0.5 + competitive)
          if canAfford && Random.nextDouble() < digiProb then
            val boost = Config.DigiInvestBoost * diminishing
            val newDR = Ratio(Math.min(1.0, firm.digitalReadiness.toDouble + boost))
            Result(firm.copy(cash = nc - PLN(digiCost), digitalReadiness = newDR),
              PLN(tax), PLN.Zero, PLN.Zero, PLN.Zero, profitShiftCost = PLN(psCost), energyCost = PLN(eCost))
          else if nc < PLN.Zero then
            // Forced downsizing before bankruptcy (zwolnienia grupowe)
            // Firms shed workers to survive — realistic Polish labor code adjustment
            if wkrs > 3 then
              val laborPerWorker = w.hh.marketWage.toDouble * effectiveWageMult(firm.sector)
              // Max 30% cut per month, minimum 3 workers retained
              val maxCut = Math.max(1, (wkrs * 0.30).toInt)
              val newWkrs = Math.max(3, wkrs - maxCut)
              val laborSaved = (wkrs - newWkrs) * laborPerWorker
              val revRatio = Math.sqrt(newWkrs.toDouble / wkrs.toDouble)
              val revLost = rev * (1.0 - revRatio)
              val adjustedNc = nc.toDouble + laborSaved - revLost
              if adjustedNc >= 0 then
                Result(firm.copy(cash = PLN(adjustedNc),
                  tech = TechState.Traditional(newWkrs)),
                  PLN(tax), PLN.Zero, PLN.Zero, PLN.Zero, profitShiftCost = PLN(psCost), energyCost = PLN(eCost))
              else
                Result(firm.copy(cash = nc,
                  tech = TechState.Bankrupt("Niewyplacalnosc (koszty pracy)")),
                  PLN(tax), PLN.Zero, PLN.Zero, PLN.Zero, profitShiftCost = PLN(psCost), energyCost = PLN(eCost))
            else
              Result(firm.copy(cash = nc,
                tech = TechState.Bankrupt("Niewyplacalnosc (koszty pracy)")),
                PLN(tax), PLN.Zero, PLN.Zero, PLN.Zero, profitShiftCost = PLN(psCost), energyCost = PLN(eCost))
          else
            Result(firm.copy(cash = nc), PLN(tax), PLN.Zero, PLN.Zero, PLN.Zero, profitShiftCost = PLN(psCost), energyCost = PLN(eCost))

    val sectorDemandMult = w.sectorDemandMult(firm.sector.toInt)
    applyInformalCitEvasion(
      applyFdiFlows(applyInventory(applyDigitalDrift(applyInvestment(applyGreenInvestment(rawResult))), sectorDemandMult)),
      w.informalCyclicalAdj)

  /** Apply informal CIT evasion — firm keeps evaded tax (#45). */
  private def applyInformalCitEvasion(r: Result, cyclicalAdj: Double): Result =
    if !Config.InformalEnabled then return r
    val f = r.firm
    if !isAlive(f) || r.taxPaid <= PLN.Zero then return r
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
  private def applyFdiFlows(r: Result): Result =
    if !Config.FdiEnabled || !r.firm.foreignOwned || !isAlive(r.firm) then return r
    val afterTaxProfit = if Config.CitRate > 0 && r.taxPaid > PLN.Zero then
      r.taxPaid.toDouble * (1.0 - Config.CitRate) / Config.CitRate
    else 0.0
    val repatriation = Math.min(
      Math.max(0.0, afterTaxProfit) * Config.FdiRepatriationRate,
      Math.max(0.0, r.firm.cash.toDouble))
    if repatriation <= 0.0 then return r
    r.copy(firm = r.firm.copy(cash = r.firm.cash - PLN(repatriation)),
           fdiRepatriation = PLN(repatriation))
