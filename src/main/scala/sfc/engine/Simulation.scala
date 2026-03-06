package sfc.engine

import sfc.accounting.{ForexState, GovState, MonetaryAggregates, SfcCheck}
import sfc.config.{Config, HH_MODE, HhMode, RunConfig, SECTORS}
import sfc.agents.*
import sfc.agents.Immigration
import sfc.accounting.*
import sfc.networks.Network
import sfc.dynamics.{DynamicNetwork, SigmaDynamics}
import sfc.engine.steps
import sfc.types.*
import sfc.util.KahanSum.*

import scala.util.Random

object Sectors:
  private def laborSupplyRatio(wage: Double, resWage: Double): Double =
    val x = Config.LaborSupplySteepness * (wage / resWage - 1.0)
    1.0 / (1.0 + Math.exp(-x))

  def updateLaborMarket(prevWage: Double, resWage: Double, laborDemand: Int): (Double, Int) =
    val supplyAtPrev = (Config.TotalPopulation * laborSupplyRatio(prevWage, resWage)).toInt
    val excessDemand = (laborDemand - supplyAtPrev).toDouble / Config.TotalPopulation
    val wageGrowth   = excessDemand * Config.WageAdjSpeed
    val newWage      = Math.max(resWage, prevWage * (1.0 + wageGrowth))
    val newSupply    = (Config.TotalPopulation * laborSupplyRatio(newWage, resWage)).toInt
    val employed     = Math.min(laborDemand, newSupply)
    (newWage, employed)

  def updateInflation(prevInflation: Double, prevPrice: Double, demandMult: Double,
    wageGrowth: Double, exRateDeviation: Double,
    autoRatio: Double, hybridRatio: Double, rc: RunConfig): (Double, Double) =
    val demandPull    = (demandMult - 1.0) * 0.15
    val costPush      = wageGrowth * 0.25
    // EUR: no exchange rate pass-through (single currency area)
    val rawImportPush = if rc.isEurozone then 0.0
                        else Math.max(0.0, exRateDeviation) * Config.ImportPropensity * 0.25
    val importPush    = if Config.OeEnabled then Math.min(rawImportPush, Config.OeImportPushCap)
                        else rawImportPush
    val techDeflation = autoRatio * 0.060 + hybridRatio * 0.018
    // Soft floor: beyond -1.5%/month, deflation passes through at 30% rate
    // (models downward price stickiness -- Bewley 1999, Schmitt-Grohe & Uribe 2016)
    val rawMonthly    = demandPull + costPush + importPush - techDeflation
    val monthly       = if rawMonthly >= -0.015 then rawMonthly
                        else -0.015 + (rawMonthly + 0.015) * 0.3
    val annualized    = monthly * 12.0
    val smoothed      = prevInflation * 0.7 + annualized * 0.3
    val newPrice      = Math.max(0.30, prevPrice * (1.0 + monthly))
    (smoothed, newPrice)

  def updateCbRate(prevRate: Double, inflation: Double, exRateChange: Double,
    employed: Int, rc: RunConfig): Double =
    if rc.isEurozone then
      // ECB Taylor rule reacting to Eurozone-wide inflation (exogenous to Poland)
      val infGap = Config.EuroInflation - Config.EcbTargetInfl
      val taylor = Config.EcbNeutralRate +
        Config.EcbAlpha * Math.max(0.0, infGap)
      val smoothed = prevRate * Config.EcbInertia + taylor * (1.0 - Config.EcbInertia)
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, smoothed))
    else if Config.NbpSymmetric then
      // v2.0: Symmetric Taylor + output gap (dual mandate)
      val infGap = inflation - Config.NbpTargetInfl
      val unempRate = 1.0 - (employed.toDouble / Config.TotalPopulation)
      val rawOutputGap = (unempRate - Config.NbpNairu) / Config.NbpNairu
      // Cap output gap at ±0.30 — prevents extreme Taylor responses from initial tight labor
      // market while preserving dual-mandate stabilization (Svensson 2003)
      val outputGap = Math.max(-0.30, Math.min(0.30, rawOutputGap))
      val taylor = Config.NbpNeutralRate +
        Config.TaylorAlpha * infGap -
        Config.TaylorDelta * outputGap +
        Config.TaylorBeta  * exRateChange
      val smoothed = prevRate * Config.TaylorInertia + taylor * (1.0 - Config.TaylorInertia)
      val effective = if Config.NbpMaxRateChange > 0 then
        prevRate + Math.max(-Config.NbpMaxRateChange, Math.min(Config.NbpMaxRateChange, smoothed - prevRate))
      else smoothed
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, effective))
    else
      // v1.0 legacy: asymmetric Taylor (inflation-only)
      val infGap  = inflation - Config.NbpTargetInfl
      val taylor  = Config.NbpNeutralRate +
        Config.TaylorAlpha * Math.max(0.0, infGap) +
        Config.TaylorBeta  * Math.max(0.0, exRateChange)
      val smoothed = prevRate * Config.TaylorInertia + taylor * (1.0 - Config.TaylorInertia)
      val effective = if Config.NbpMaxRateChange > 0 then
        prevRate + Math.max(-Config.NbpMaxRateChange, Math.min(Config.NbpMaxRateChange, smoothed - prevRate))
      else smoothed
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, effective))

  def updateForeign(prev: ForexState, importConsumption: Double, techImports: Double,
    autoRatio: Double, domesticRate: Double, gdp: Double, rc: RunConfig): ForexState =
    val techComp = 1.0 + autoRatio * Config.ExportAutoBoost
    val totalImp = importConsumption + techImports
    if rc.isEurozone then
      // EUR: fixed exchange rate, exports depend on relative price competitiveness
      // No capital account arbitrage (single monetary zone)
      val exports  = Config.ExportBase * techComp
      val tradeBal = exports - totalImp
      ForexState(Config.BaseExRate, PLN(totalImp), PLN(exports), PLN(tradeBal), PLN(techImports))
    else
      // PLN: floating exchange rate, BoP-driven adjustment
      val exComp   = prev.exchangeRate / Config.BaseExRate
      val exports  = Config.ExportBase * exComp * techComp
      val tradeBal = exports - totalImp
      val rateDiff = domesticRate - Config.ForeignRate
      val capAcct  = rateDiff * Config.IrpSensitivity * gdp
      val bop      = tradeBal + capAcct
      val bopRatio = if gdp > 0 then bop / gdp else 0.0
      val exRateChg = -Config.ExRateAdjSpeed * bopRatio
      val newRate  = Math.max(3.0, Math.min(8.0, prev.exchangeRate * (1.0 + exRateChg)))
      ForexState(newRate, PLN(totalImp), PLN(exports), PLN(tradeBal), PLN(techImports))

  def updateGov(prev: GovState, citPaid: Double, vat: Double,
    bdpActive: Boolean, bdpAmount: Double, priceLevel: Double,
    unempBenefitSpend: Double,
    debtService: Double = 0.0,
    nbpRemittance: Double = 0.0,
    zusGovSubvention: Double = 0.0,
    socialTransferSpend: Double = 0.0,
    euCofinancing: Double = 0.0,
    euProjectCapital: Double = 0.0,
    exciseRevenue: Double = 0.0,
    customsDutyRevenue: Double = 0.0,
    govPurchasesActual: Double = 0.0): GovState =
    val bdpSpend   = if bdpActive then Config.TotalPopulation.toDouble * bdpAmount else 0.0
    val govBaseRaw = if govPurchasesActual > 0 then govPurchasesActual
                     else Config.GovBaseSpending * priceLevel
    val (govCurrent, govCapital) = if Config.GovInvestEnabled then
      (govBaseRaw * (1.0 - Config.GovInvestShare), govBaseRaw * Config.GovInvestShare)
    else (govBaseRaw, 0.0)
    val totalSpend = bdpSpend + unempBenefitSpend + socialTransferSpend + govCurrent + govCapital + debtService + zusGovSubvention + euCofinancing
    val totalRev   = citPaid + vat + nbpRemittance + exciseRevenue + customsDutyRevenue
    val deficit    = totalSpend - totalRev
    val newBondsOutstanding = if Config.GovBondMarket then Math.max(0.0, prev.bondsOutstanding.toDouble + deficit)
                              else prev.bondsOutstanding.toDouble
    val newCapitalStock = if Config.GovInvestEnabled then
      prev.publicCapitalStock.toDouble * (1.0 - Config.GovDepreciationRate / 12.0) + govCapital + euProjectCapital
    else 0.0
    GovState(bdpActive, PLN(totalRev), PLN(bdpSpend), PLN(deficit), PLN(prev.cumulativeDebt.toDouble + deficit),
      PLN(unempBenefitSpend), PLN(newBondsOutstanding), prev.bondYield, PLN(debtService), PLN(socialTransferSpend),
      PLN(newCapitalStock), PLN(govCurrent), PLN(govCapital + euProjectCapital), PLN(euCofinancing),
      PLN(exciseRevenue), PLN(customsDutyRevenue))

object Simulation:
  /** Step with optional individual households.
    * When households = None (aggregate mode), behavior is identical to Papers 1–5.
    * When households = Some(...) (individual mode), uses HouseholdLogic + LaborMarket. */
  def step(w: World, firms: Array[Firm], rc: RunConfig,
           households: Option[Vector[Household]] = None): (World, Array[Firm], Option[Vector[Household]]) =
    // ---- Step 1: Fiscal constraint ----
    val s1 = steps.FiscalConstraintStep.run(steps.FiscalConstraintStep.Input(
      month = w.month, gdpProxy = w.gdpProxy, gov = w.gov, priceLevel = w.priceLevel,
      minWageLevel = w.hh.minWageLevel, minWagePriceLevel = w.hh.minWagePriceLevel,
      marketWage = w.hh.marketWage, bankingSector = w.bankingSector,
      nbpReferenceRate = w.nbp.referenceRate.toDouble,
      expectedRate = w.expectations.expectedRate.toDouble,
      bdpAmount = rc.bdpAmount, isEurozone = rc.isEurozone))
    val m = s1.m; val bdpActive = s1.bdpActive; val bdp = s1.bdp
    val baseMinWage = s1.baseMinWage; val updatedMinWagePriceLevel = s1.updatedMinWagePriceLevel
    val resWage = s1.resWage; val lendingBaseRate = s1.lendingBaseRate

    // ---- Step 2: Labor market, demographics, immigration, ZUS/PPK ----
    val s2 = steps.LaborDemographicsStep.run(steps.LaborDemographicsStep.Input(
      marketWage = w.hh.marketWage, firms = firms, demographics = w.demographics,
      immigration = w.immigration, zusBalance = w.zus.fusBalance.toDouble,
      ppkBondHoldings = w.ppk.bondHoldings.toDouble, households = households,
      resWage = resWage, expectedInflation = w.expectations.expectedInflation.toDouble,
      m = m, rc = rc))
    val newWage = s2.newWage; val employed = s2.employed; val wageGrowth = s2.wageGrowth
    val newImmig = s2.newImmig; val netMigration = s2.netMigration
    val newDemographics = s2.newDemographics
    val newZus = s2.newZus; val newPpk = s2.newPpk; val rawPpkBondPurchase = s2.rawPpkBondPurchase
    val living = s2.living

    // ---- Step 3: Household income / consumption ----
    val s3 = steps.HouseholdIncomeStep.run(steps.HouseholdIncomeStep.Input(
      employed = employed, newWage = newWage, bdp = bdp, bdpActive = bdpActive,
      resWage = resWage, w = w, households = households, firms = firms,
      lendingBaseRate = lendingBaseRate, newZus = newZus, rc = rc))
    val totalIncome = s3.totalIncome; val consumption = s3.consumption
    val importCons = s3.importCons; val domesticCons = s3.domesticCons
    val updatedHouseholds = s3.updatedHouseholds
    val hhAgg = s3.hhAgg; val perBankHhFlowsOpt = s3.perBankHhFlowsOpt
    val pitRevenue = s3.pitRevenue; val importAdj = s3.importAdj
    val aggUnempBenefit = s3.aggUnempBenefit

    // ---- Step 4: Flow-of-funds demand multipliers ----
    val s4 = steps.DemandStep.run(steps.DemandStep.Input(
      employed = employed, domesticCons = domesticCons, living = living,
      priceLevel = w.priceLevel, zusContributions = w.zus.contributions.toDouble,
      zusPensionPayments = w.zus.pensionPayments.toDouble,
      govTaxRevenue = w.gov.taxRevenue.toDouble,
      govCurrentSpend = w.gov.govCurrentSpend.toDouble,
      govCapitalSpend = w.gov.govCapitalSpend.toDouble,
      nbpReferenceRate = w.nbp.referenceRate.toDouble,
      expectedInflation = w.expectations.expectedInflation.toDouble,
      forexExports = w.forex.exports.toDouble,
      gvcEnabled = Config.GvcEnabled, gvcSectorExports = w.gvc.sectorExports,
      grossInvestment = w.grossInvestment.toDouble,
      aggGreenInvestment = w.aggGreenInvestment.toDouble))
    val govPurchases = s4.govPurchases; val sectorMults = s4.sectorMults
    val avgDemandMult = s4.avgDemandMult; val sectorCap = s4.sectorCap
    val laggedInvestDemand = s4.laggedInvestDemand

    // ---- Step 5: Firm processing ----
    val s5 = steps.FirmProcessingStep.run(steps.FirmProcessingStep.Input(
      w = w, firms = firms, households = households,
      updatedHouseholds = updatedHouseholds, newWage = newWage, resWage = resWage,
      m = m, sectorMults = sectorMults, lendingBaseRate = lendingBaseRate,
      newImmig = newImmig, rc = rc))
    val ioFirms = s5.ioFirms; val finalHouseholds = s5.finalHouseholds
    val sumTax = s5.sumTax; val sumCapex = s5.sumCapex; val sumTechImp = s5.sumTechImp
    val sumNewLoans = s5.sumNewLoans; val sumEquityIssuance = s5.sumEquityIssuance
    val sumGrossInvestment = s5.sumGrossInvestment; val sumBondIssuance = s5.sumBondIssuance
    val sumProfitShifting = s5.sumProfitShifting; val sumFdiRepatriation = s5.sumFdiRepatriation
    val sumInventoryChange = s5.sumInventoryChange; val sumCitEvasion = s5.sumCitEvasion
    val sumEnergyCost = s5.sumEnergyCost; val sumGreenInvestment = s5.sumGreenInvestment
    val totalIoPaid = s5.totalIoPaid; val nplNew = s5.nplNew; val nplLoss = s5.nplLoss
    val totalBondDefault = s5.totalBondDefault; val firmDeaths = s5.firmDeaths
    val intIncome = s5.intIncome; val corpBondAbsorption = s5.corpBondAbsorption
    val actualBondIssuance = s5.actualBondIssuance
    val perBankNewLoans = s5.perBankNewLoans; val perBankNplDebt = s5.perBankNplDebt
    val perBankIntIncome = s5.perBankIntIncome; val perBankWorkers = s5.perBankWorkers
    val lendingRates = s5.lendingRates
    val postFirmCrossSectorHires = s5.postFirmCrossSectorHires

    // Side effect: update TotalPopulation with net migration (aggregate mode)
    if Config.ImmigEnabled && households.isEmpty then
      Config.setTotalPopulation(Config.TotalPopulation + netMigration)

    // ---- Step 6: Household financial flows ----
    val s6 = steps.HouseholdFinancialStep.run(steps.HouseholdFinancialStep.Input(
      hhAgg = hhAgg,
      newImmigRemittanceOutflow = newImmig.remittanceOutflow,
      employed = employed,
      m = m,
      lendingBaseRate = lendingBaseRate,
      domesticCons = domesticCons,
      forexExchangeRate = w.forex.exchangeRate,
      gdpProxy = w.gdpProxy,
      demographicsWorkingAgePop = w.demographics.workingAgePop,
      bankConsumerLoans = w.bank.consumerLoans.toDouble
    ))
    val hhDebtService = s6.hhDebtService; val depositInterestPaid = s6.depositInterestPaid
    val remittanceOutflow = s6.remittanceOutflow; val diasporaInflow = s6.diasporaInflow
    val tourismExport = s6.tourismExport; val tourismImport = s6.tourismImport
    val consumerDebtService = s6.consumerDebtService; val consumerOrigination = s6.consumerOrigination
    val consumerDefaultAmt = s6.consumerDefaultAmt; val consumerNplLoss = s6.consumerNplLoss
    val consumerPrincipal = s6.consumerPrincipal

    // ---- Step 7: Price, equity, GDP, macropru ----
    val s7 = steps.PriceEquityStep.run(steps.PriceEquityStep.Input(
      ioFirms = ioFirms,
      w = w,
      sectorMults = sectorMults,
      newWage = newWage,
      wageGrowth = wageGrowth,
      avgDemandMult = avgDemandMult,
      m = m,
      sumGrossInvestment = sumGrossInvestment,
      sumGreenInvestment = sumGreenInvestment,
      sumEquityIssuance = sumEquityIssuance,
      sumInventoryChange = sumInventoryChange,
      domesticCons = domesticCons,
      lendingRates = lendingRates,
      rc = rc
    ))
    val autoR = s7.autoR; val hybR = s7.hybR
    val aggInventoryStock = s7.aggInventoryStock; val aggGreenCapital = s7.aggGreenCapital
    val euMonthly = s7.euMonthly; val euCofin = s7.euCofin; val euProjectCapital = s7.euProjectCapital
    val gdp = s7.gdp; val newMacropru = s7.newMacropru; val newSigmas = s7.newSigmas
    val rewiredFirms = s7.rewiredFirms; val newInfl = s7.newInfl; val newPrice = s7.newPrice
    val equityAfterIssuance = s7.equityAfterIssuance
    val netDomesticDividends = s7.netDomesticDividends
    val foreignDividendOutflow = s7.foreignDividendOutflow; val dividendTax = s7.dividendTax
    val firmProfits = s7.firmProfits; val domesticGFCF = s7.domesticGFCF
    val investmentImports = s7.investmentImports; val aggInventoryChange = s7.aggInventoryChange

    // ---- Step 8: Open economy, monetary, bonds, insurance, NBFI ----
    val s8 = steps.OpenEconomyStep.run(steps.OpenEconomyStep.Input(
      w = w,
      ioFirms = ioFirms,
      sectorMults = sectorMults,
      importCons = importCons,
      sumTechImp = sumTechImp,
      investmentImports = investmentImports,
      autoR = autoR,
      gdp = gdp,
      newInfl = newInfl,
      newPrice = newPrice,
      employed = employed,
      sumProfitShifting = sumProfitShifting,
      sumFdiRepatriation = sumFdiRepatriation,
      remittanceOutflow = remittanceOutflow,
      euMonthly = euMonthly,
      diasporaInflow = diasporaInflow,
      tourismExport = tourismExport,
      tourismImport = tourismImport,
      equityAfterIssuance = equityAfterIssuance,
      foreignDividendOutflow = foreignDividendOutflow,
      totalBondDefault = totalBondDefault,
      actualBondIssuance = actualBondIssuance,
      corpBondAbsorption = corpBondAbsorption,
      firmProfits = firmProfits,
      newWage = newWage,
      domesticCons = domesticCons,
      m = m,
      rc = rc
    ))
    val newForex = s8.newForex; val newBop = s8.newBop; val newGvc = s8.newGvc
    val newRefRate = s8.newRefRate; val newExp = s8.newExp
    val totalReserveInterest = s8.totalReserveInterest
    val totalStandingFacilityIncome = s8.totalStandingFacilityIncome
    val totalInterbankInterest = s8.totalInterbankInterest
    val newBondYield = s8.newBondYield; val monthlyDebtService = s8.monthlyDebtService
    val bankBondIncome = s8.bankBondIncome; val nbpRemittance = s8.nbpRemittance
    val postFxNbp = s8.postFxNbp; val qePurchaseAmount = s8.qePurchaseAmount
    val newCorpBonds = s8.newCorpBonds
    val corpBondBankCoupon = s8.corpBondBankCoupon
    val corpBondBankDefaultLoss = s8.corpBondBankDefaultLoss
    val corpBondAmort = s8.corpBondAmort
    val newInsurance = s8.newInsurance; val insNetDepositChange = s8.insNetDepositChange
    val newNbfi = s8.newNbfi; val nbfiDepositDrain = s8.nbfiDepositDrain
    val oeValuationEffect = s8.oeValuationEffect; val fdiCitLoss = s8.fdiCitLoss

    // ---- Step 9: Bank update (tax, gov, housing, bank, bond allocation) ----
    val s9 = steps.BankUpdateStep.run(steps.BankUpdateStep.Input(
      w = w, consumption = consumption, totalIncome = totalIncome, domesticCons = domesticCons,
      employed = employed, newWage = newWage, resWage = resWage,
      bdpActive = bdpActive, bdp = bdp, lendingBaseRate = lendingBaseRate,
      newZus = newZus, newPpk = newPpk, rawPpkBondPurchase = rawPpkBondPurchase, wageGrowth = wageGrowth,
      pitRevenue = pitRevenue, importAdj = importAdj, aggUnempBenefit = aggUnempBenefit,
      perBankHhFlowsOpt = perBankHhFlowsOpt, hhAgg = hhAgg,
      govPurchases = govPurchases, laggedInvestDemand = laggedInvestDemand,
      ioFirms = ioFirms, rewiredFirms = rewiredFirms, finalHouseholds = finalHouseholds,
      sumTax = sumTax, sumNewLoans = sumNewLoans, nplNew = nplNew, nplLoss = nplLoss,
      intIncome = intIncome, sumGrossInvestment = sumGrossInvestment, sumGreenInvestment = sumGreenInvestment,
      perBankNewLoans = perBankNewLoans, perBankNplDebt = perBankNplDebt,
      perBankIntIncome = perBankIntIncome, perBankWorkers = perBankWorkers,
      hhDebtService = hhDebtService, depositInterestPaid = depositInterestPaid,
      consumerDebtService = consumerDebtService, consumerOrigination = consumerOrigination,
      consumerDefaultAmt = consumerDefaultAmt, consumerNplLoss = consumerNplLoss,
      consumerPrincipal = consumerPrincipal, remittanceOutflow = remittanceOutflow,
      diasporaInflow = diasporaInflow, tourismExport = tourismExport, tourismImport = tourismImport,
      euCofin = euCofin, euProjectCapital = euProjectCapital, gdp = gdp,
      newMacropru = newMacropru, newInfl = newInfl, newPrice = newPrice,
      netDomesticDividends = netDomesticDividends, foreignDividendOutflow = foreignDividendOutflow,
      dividendTax = dividendTax, newBop = newBop,
      totalReserveInterest = totalReserveInterest, totalStandingFacilityIncome = totalStandingFacilityIncome,
      totalInterbankInterest = totalInterbankInterest, newBondYield = newBondYield,
      monthlyDebtService = monthlyDebtService, bankBondIncome = bankBondIncome, nbpRemittance = nbpRemittance,
      postFxNbp = postFxNbp, qePurchaseAmount = qePurchaseAmount, newCorpBonds = newCorpBonds,
      corpBondBankCoupon = corpBondBankCoupon, corpBondBankDefaultLoss = corpBondBankDefaultLoss,
      newInsurance = newInsurance, insNetDepositChange = insNetDepositChange,
      newNbfi = newNbfi, nbfiDepositDrain = nbfiDepositDrain,
      m = m, rc = rc
    ))

    // ---- Step 10: World assembly, SFC check, FDI M&A, firm entry ----
    val s10 = steps.WorldAssemblyStep.run(steps.WorldAssemblyStep.Input(
      w = w, firms = firms, households = households, living = living, sectorCap = sectorCap,
      baseMinWage = baseMinWage, updatedMinWagePriceLevel = updatedMinWagePriceLevel,
      newWage = newWage, resWage = resWage, employed = employed, newImmig = newImmig,
      newDemographics = newDemographics, newZus = newZus,
      totalIncome = totalIncome, consumption = consumption, domesticCons = domesticCons,
      importCons = importCons, importAdj = importAdj, pitRevenue = pitRevenue, hhAgg = hhAgg,
      sectorMults = sectorMults, govPurchases = govPurchases,
      ioFirms = ioFirms, sumTax = sumTax, sumNewLoans = sumNewLoans, nplNew = nplNew,
      nplLoss = nplLoss, intIncome = intIncome, sumGrossInvestment = sumGrossInvestment,
      sumGreenInvestment = sumGreenInvestment, sumProfitShifting = sumProfitShifting,
      sumFdiRepatriation = sumFdiRepatriation, sumInventoryChange = sumInventoryChange,
      sumCitEvasion = sumCitEvasion, sumEnergyCost = sumEnergyCost, totalIoPaid = totalIoPaid,
      firmDeaths = firmDeaths, postFirmCrossSectorHires = postFirmCrossSectorHires,
      hhDebtService = hhDebtService, depositInterestPaid = depositInterestPaid,
      consumerDebtService = consumerDebtService, consumerOrigination = consumerOrigination,
      consumerDefaultAmt = consumerDefaultAmt, consumerNplLoss = consumerNplLoss,
      consumerPrincipal = consumerPrincipal, remittanceOutflow = remittanceOutflow,
      diasporaInflow = diasporaInflow, tourismExport = tourismExport, tourismImport = tourismImport,
      autoR = autoR, hybR = hybR, aggInventoryStock = aggInventoryStock,
      aggGreenCapital = aggGreenCapital, euCofin = euCofin, gdp = gdp,
      newMacropru = newMacropru, newSigmas = newSigmas, newInfl = newInfl, newPrice = newPrice,
      equityAfterIssuance = equityAfterIssuance, netDomesticDividends = netDomesticDividends,
      foreignDividendOutflow = foreignDividendOutflow, dividendTax = dividendTax,
      aggInventoryChange = aggInventoryChange,
      newForex = newForex, newBop = newBop, newGvc = newGvc, newExp = newExp,
      totalReserveInterest = totalReserveInterest, totalStandingFacilityIncome = totalStandingFacilityIncome,
      totalInterbankInterest = totalInterbankInterest, newBondYield = newBondYield,
      monthlyDebtService = monthlyDebtService, bankBondIncome = bankBondIncome,
      nbpRemittance = nbpRemittance, postFxNbp = postFxNbp, qePurchaseAmount = qePurchaseAmount,
      newCorpBonds = newCorpBonds, corpBondBankCoupon = corpBondBankCoupon,
      corpBondBankDefaultLoss = corpBondBankDefaultLoss, corpBondAmort = corpBondAmort,
      insNetDepositChange = insNetDepositChange, nbfiDepositDrain = nbfiDepositDrain,
      oeValuationEffect = oeValuationEffect, fdiCitLoss = fdiCitLoss,
      totalBondDefault = totalBondDefault, actualBondIssuance = actualBondIssuance,
      resolvedBank = s9.resolvedBank, finalBankingSector = s9.finalBankingSector,
      reassignedFirms = s9.reassignedFirms, reassignedHouseholds = s9.reassignedHouseholds,
      finalPpk = s9.finalPpk, finalInsurance = s9.finalInsurance, finalNbfi = s9.finalNbfi,
      newGovWithYield = s9.newGovWithYield, newJst = s9.newJst, housingAfterFlows = s9.housingAfterFlows,
      bfgLevy = s9.bfgLevy, bailInLoss = s9.bailInLoss, multiCapDestruction = s9.multiCapDestruction,
      monAgg = s9.monAgg, finalHhAgg = s9.finalHhAgg,
      vat = s9.vat, vatAfterEvasion = s9.vatAfterEvasion, pitAfterEvasion = s9.pitAfterEvasion,
      exciseRevenue = s9.exciseRevenue, exciseAfterEvasion = s9.exciseAfterEvasion,
      customsDutyRevenue = s9.customsDutyRevenue, effectiveShadowShare = s9.effectiveShadowShare,
      mortgageInterestIncome = s9.mortgageInterestIncome, mortgagePrincipal = s9.mortgagePrincipal,
      mortgageDefaultLoss = s9.mortgageDefaultLoss, mortgageDefaultAmount = s9.mortgageDefaultAmount,
      jstDepositChange = s9.jstDepositChange, investNetDepositFlow = s9.investNetDepositFlow,
      actualBondChange = s9.actualBondChange,
      m = m, rc = rc
    ))
    (s10.newWorld, s10.finalFirms, s10.reassignedHouseholds)
