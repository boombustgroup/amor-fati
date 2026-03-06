package sfc.engine.steps

import sfc.accounting.{BopState, ForexState}
import sfc.agents.*
import sfc.config.{Config, RunConfig, SECTORS}
import sfc.engine.{CorporateBondMarket, EquityMarket, Expectations,
  ExternalSector, GvcState, OpenEconomy, Sectors, World, YieldCurve}
import sfc.types.*
import sfc.util.KahanSum.*

object OpenEconomyStep:

  case class Input(
    w: World,
    ioFirms: Array[Firm],
    sectorMults: Vector[Double],
    importCons: Double,
    sumTechImp: Double,
    investmentImports: Double,
    autoR: Double,
    gdp: Double,
    newInfl: Double,
    newPrice: Double,
    employed: Int,
    sumProfitShifting: Double,
    sumFdiRepatriation: Double,
    remittanceOutflow: Double,
    euMonthly: Double,
    diasporaInflow: Double,
    tourismExport: Double,
    tourismImport: Double,
    equityAfterIssuance: EquityMarket.State,
    foreignDividendOutflow: Double,
    totalBondDefault: Double,
    actualBondIssuance: Double,
    corpBondAbsorption: Double,
    firmProfits: Double,
    newWage: Double,
    domesticCons: Double,
    m: Int,
    rc: RunConfig
  )

  case class Output(
    newForex: ForexState,
    newBop: BopState,
    newGvc: GvcState,
    newRefRate: Double,
    newExp: Expectations.State,
    totalReserveInterest: Double,
    totalStandingFacilityIncome: Double,
    totalInterbankInterest: Double,
    newBondYield: Double,
    monthlyDebtService: Double,
    bankBondIncome: Double,
    nbpRemittance: Double,
    postFxNbp: NbpState,
    qePurchaseAmount: Double,
    newCorpBonds: CorporateBondMarket.State,
    corpBondBankCoupon: Double,
    corpBondBankDefaultLoss: Double,
    corpBondAmort: Double,
    newInsurance: InsuranceSectorState,
    insNetDepositChange: Double,
    newNbfi: NbfiState,
    nbfiDepositDrain: Double,
    oeValuationEffect: Double,
    fdiCitLoss: Double
  )

  def run(in: Input): Output =
    val living2 = in.ioFirms.filter(FirmOps.isAlive)

    // Sector outputs for open economy
    val sectorOutputs = (0 until SECTORS.length).map { s =>
      living2.filter(_.sector.toInt == s).kahanSumBy(f =>
        FirmOps.capacity(f) * in.sectorMults(f.sector.toInt) * in.w.priceLevel)
    }.toVector

    // GVC / Deep External Sector (v5.0)
    val newGvc = if Config.GvcEnabled && Config.OeEnabled then
      ExternalSector.step(in.w.gvc, sectorOutputs, in.w.priceLevel,
        in.w.forex.exchangeRate, in.autoR, in.m, in.rc)
    else in.w.gvc

    val (gvcExp, gvcImp) = if Config.GvcEnabled && Config.OeEnabled then
      (Some(newGvc.totalExports.toDouble), Some(newGvc.sectorImports.map(_.toDouble)))
    else (None, None)

    val totalTechAndInvImports = in.sumTechImp + in.investmentImports
    val (newForex, newBop0, oeValuationEffect, fxResult) = if Config.OeEnabled then
      val oeResult = OpenEconomy.step(
        in.w.bop, in.w.forex, in.importCons, totalTechAndInvImports,
        in.autoR, in.w.nbp.referenceRate.toDouble, in.gdp, in.w.priceLevel,
        sectorOutputs, in.m, in.rc,
        nbpFxReserves = in.w.nbp.fxReserves.toDouble,
        gvcExports = gvcExp,
        gvcIntermImports = gvcImp,
        remittanceOutflow = in.remittanceOutflow,
        euFundsMonthly = in.euMonthly,
        diasporaInflow = in.diasporaInflow,
        tourismExport = in.tourismExport,
        tourismImport = in.tourismImport)
      (oeResult.forex, oeResult.bop, oeResult.valuationEffect, oeResult.fxIntervention)
    else
      val fx = Sectors.updateForeign(in.w.forex, in.importCons, totalTechAndInvImports,
        in.autoR, in.w.nbp.referenceRate.toDouble, in.gdp, in.rc)
      (fx, in.w.bop, 0.0, CentralBankLogic.FxInterventionResult(0.0, 0.0, in.w.nbp.fxReserves.toDouble))

    // Adjust BOP for foreign dividend outflow (primary income component) + EU funds tracking
    val newBop1 = if in.foreignDividendOutflow > 0 && Config.OeEnabled then
      newBop0.copy(
        currentAccount = newBop0.currentAccount - PLN(in.foreignDividendOutflow),
        nfa = newBop0.nfa - PLN(in.foreignDividendOutflow)
      )
    else newBop0
    // FDI composition (#33): profit shifting (service import) + repatriation (primary income debit)
    val fdiTotalBopDebit = in.sumProfitShifting + in.sumFdiRepatriation
    val newBop2 = if fdiTotalBopDebit > 0 && Config.FdiEnabled && Config.OeEnabled then
      newBop1.copy(
        currentAccount = newBop1.currentAccount - PLN(fdiTotalBopDebit),
        nfa = newBop1.nfa - PLN(fdiTotalBopDebit),
        tradeBalance = newBop1.tradeBalance - PLN(in.sumProfitShifting),
        totalImports = newBop1.totalImports + PLN(in.sumProfitShifting))
    else newBop1
    val fdiCitLoss = in.sumProfitShifting * Config.CitRate
    val newBop = newBop2.copy(
      euFundsMonthly = PLN(in.euMonthly),
      euCumulativeAbsorption = in.w.bop.euCumulativeAbsorption + PLN(in.euMonthly)
    )

    val exRateChg = if in.rc.isEurozone then 0.0
                    else (newForex.exchangeRate / in.w.forex.exchangeRate) - 1.0
    val newRefRate = Sectors.updateCbRate(in.w.nbp.referenceRate.toDouble, in.newInfl, exRateChg, in.employed, in.rc)

    // Expectations step: update after inflation + rate computed
    val unempRateForExp = 1.0 - in.employed.toDouble / Config.TotalPopulation
    val newExp = if Config.ExpEnabled then
      Expectations.step(in.w.expectations, in.newInfl, newRefRate, unempRateForExp, in.rc)
    else in.w.expectations

    // Reserve interest, standing facilities, interbank interest
    val (totalReserveInterest, totalStandingFacilityIncome, totalInterbankInterest) =
      in.w.bankingSector match
        case Some(bs) =>
          val (_, resInt) = BankingSector.computeReserveInterest(bs.banks, in.w.nbp.referenceRate.toDouble)
          val (_, sfInc) = BankingSector.computeStandingFacilities(bs.banks, in.w.nbp.referenceRate.toDouble)
          val (_, ibInt) = BankingSector.interbankInterestFlows(bs.banks, bs.interbankRate.toDouble)
          (resInt, sfInc, ibInt)
        case None => (0.0, 0.0, 0.0)

    // --- Bond market + QE ---
    val annualGdpForBonds = in.w.gdpProxy * 12.0
    val debtToGdp = if annualGdpForBonds > 0 then in.w.gov.cumulativeDebt.toDouble / annualGdpForBonds else 0.0
    val nbpBondGdpShare = if annualGdpForBonds > 0 then in.w.nbp.qeCumulative.toDouble / annualGdpForBonds else 0.0
    // Channel 3: De-anchored expectations → higher bond yields
    val credPremium = if Config.ExpEnabled then
      val target = if in.rc.isEurozone then Config.EcbTargetInfl else Config.NbpTargetInfl
      (1.0 - in.w.expectations.credibility.toDouble) *
        Math.abs(in.w.expectations.expectedInflation.toDouble - target) *
        Config.ExpBondSensitivity
    else 0.0
    val newBondYield = CentralBankLogic.bondYield(newRefRate, debtToGdp, nbpBondGdpShare, in.w.bop.nfa.toDouble, credPremium)

    // Debt service: use LAGGED bond stock
    val rawDebtService = in.w.gov.bondsOutstanding.toDouble * newBondYield / 12.0
    val monthlyDebtService = Math.min(rawDebtService, in.w.gdpProxy * 0.50)
    val bankBondIncome = in.w.bank.govBondHoldings.toDouble * newBondYield / 12.0
    val nbpBondIncome = in.w.nbp.govBondHoldings.toDouble * newBondYield / 12.0
    val nbpRemittance = nbpBondIncome - totalReserveInterest - totalStandingFacilityIncome

    // QE logic
    val qeActivate = CentralBankLogic.shouldActivateQe(newRefRate, in.newInfl)
    val qeTaper = CentralBankLogic.shouldTaperQe(in.newInfl)
    val qeActive = if qeActivate then true
                   else if qeTaper then false
                   else in.w.nbp.qeActive
    val preQeNbp = NbpState(Rate(newRefRate), in.w.nbp.govBondHoldings, qeActive, in.w.nbp.qeCumulative)
    val (postQeNbp, qePurchaseAmount) = CentralBankLogic.executeQe(
      preQeNbp, in.w.bank.govBondHoldings.toDouble, annualGdpForBonds)
    val postFxNbp = postQeNbp.copy(
      fxReserves = PLN(fxResult.newReserves),
      lastFxTraded = PLN(fxResult.eurTraded)
    )

    // --- Corporate bond market step (#40) ---
    val corpBondAmort = CorporateBondMarket.amortization(in.w.corporateBonds)
    val newCorpBonds = CorporateBondMarket.step(in.w.corporateBonds, newBondYield,
      in.w.bank.nplRatio, in.totalBondDefault, in.actualBondIssuance)
      .copy(lastAbsorptionRate = Ratio(in.corpBondAbsorption))
    val (_, corpBondBankCoupon, _) = CorporateBondMarket.computeCoupon(in.w.corporateBonds)
    val (_, _, corpBondBankDefaultLoss, _) = CorporateBondMarket.processDefaults(
      in.w.corporateBonds, in.totalBondDefault)

    // --- Insurance sector step (#41) ---
    val insUnempRate = 1.0 - in.employed.toDouble / Config.TotalPopulation
    val newInsurance = if Config.InsEnabled then
      InsuranceSector.step(in.w.insurance, in.employed, in.newWage, in.w.priceLevel, insUnempRate,
        newBondYield, in.w.corporateBonds.corpBondYield.toDouble, in.w.equity.monthlyReturn.toDouble)
    else in.w.insurance
    val insNetDepositChange = newInsurance.lastNetDepositChange.toDouble

    // --- Shadow Banking / NBFI step (#42) ---
    val nbfiDepositRate = Math.max(0.0, postFxNbp.referenceRate.toDouble - 0.02)
    val nbfiUnempRate = 1.0 - in.employed.toDouble / Config.TotalPopulation
    val newNbfi = if Config.NbfiEnabled then
      ShadowBanking.step(in.w.nbfi, in.employed, in.newWage, in.w.priceLevel,
        nbfiUnempRate, in.w.bank.nplRatio, newBondYield,
        in.w.corporateBonds.corpBondYield.toDouble, in.w.equity.monthlyReturn.toDouble,
        nbfiDepositRate, in.domesticCons)
    else in.w.nbfi
    val nbfiDepositDrain = newNbfi.lastDepositDrain.toDouble

    Output(newForex, newBop, newGvc, newRefRate, newExp,
      totalReserveInterest, totalStandingFacilityIncome, totalInterbankInterest,
      newBondYield, monthlyDebtService, bankBondIncome, nbpRemittance, postFxNbp,
      qePurchaseAmount, newCorpBonds, corpBondBankCoupon, corpBondBankDefaultLoss,
      corpBondAmort, newInsurance, insNetDepositChange, newNbfi, nbfiDepositDrain,
      oeValuationEffect, fdiCitLoss)
