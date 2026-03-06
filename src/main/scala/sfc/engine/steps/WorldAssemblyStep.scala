package sfc.engine.steps

import sfc.accounting.{BankState, BopState, ForexState, GovState, MonetaryAggregates, SfcCheck}
import sfc.agents.*
import sfc.config.{Config, RunConfig, SECTORS}
import sfc.engine.{CorporateBondMarket, EquityMarket, Expectations, GvcState,
  HousingMarket, Macroprudential, SectoralMobility, World}
import sfc.types.*
import sfc.util.KahanSum.*

import scala.util.Random

object WorldAssemblyStep:

  case class Input(
    w: World,
    firms: Array[Firm],
    households: Option[Vector[Household]],
    living: Array[Firm],
    sectorCap: Vector[Double],
    // Step 1
    baseMinWage: Double,
    updatedMinWagePriceLevel: Double,
    // Step 2
    newWage: Double,
    resWage: Double,
    employed: Int,
    newImmig: ImmigrationState,
    newDemographics: DemographicsState,
    newZus: ZusState,
    // Step 3
    totalIncome: Double,
    consumption: Double,
    domesticCons: Double,
    importCons: Double,
    importAdj: Double,
    pitRevenue: Double,
    hhAgg: Option[HhAggregates],
    // Step 4
    sectorMults: Vector[Double],
    govPurchases: Double,
    // Step 5
    ioFirms: Array[Firm],
    sumTax: Double,
    sumNewLoans: Double,
    nplNew: Double,
    nplLoss: Double,
    intIncome: Double,
    sumGrossInvestment: Double,
    sumGreenInvestment: Double,
    sumProfitShifting: Double,
    sumFdiRepatriation: Double,
    sumInventoryChange: Double,
    sumCitEvasion: Double,
    sumEnergyCost: Double,
    totalIoPaid: Double,
    firmDeaths: Int,
    postFirmCrossSectorHires: Int,
    // Step 6
    hhDebtService: Double,
    depositInterestPaid: Double,
    consumerDebtService: Double,
    consumerOrigination: Double,
    consumerDefaultAmt: Double,
    consumerNplLoss: Double,
    consumerPrincipal: Double,
    remittanceOutflow: Double,
    diasporaInflow: Double,
    tourismExport: Double,
    tourismImport: Double,
    // Step 7
    autoR: Double,
    hybR: Double,
    aggInventoryStock: Double,
    aggGreenCapital: Double,
    euCofin: Double,
    gdp: Double,
    newMacropru: Macroprudential.State,
    newSigmas: Vector[Double],
    newInfl: Double,
    newPrice: Double,
    equityAfterIssuance: EquityMarket.State,
    netDomesticDividends: Double,
    foreignDividendOutflow: Double,
    dividendTax: Double,
    aggInventoryChange: Double,
    // Step 8
    newForex: ForexState,
    newBop: BopState,
    newGvc: GvcState,
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
    insNetDepositChange: Double,
    nbfiDepositDrain: Double,
    oeValuationEffect: Double,
    fdiCitLoss: Double,
    totalBondDefault: Double,
    actualBondIssuance: Double,
    // Step 9
    resolvedBank: BankState,
    finalBankingSector: Option[BankingSectorState],
    reassignedFirms: Array[Firm],
    reassignedHouseholds: Option[Vector[Household]],
    finalPpk: PpkState,
    finalInsurance: Insurance.State,
    finalNbfi: Nbfi.State,
    newGovWithYield: GovState,
    newJst: Jst.State,
    housingAfterFlows: HousingMarket.State,
    bfgLevy: Double,
    bailInLoss: Double,
    multiCapDestruction: Double,
    monAgg: Option[MonetaryAggregates],
    finalHhAgg: Option[HhAggregates],
    vat: Double,
    vatAfterEvasion: Double,
    pitAfterEvasion: Double,
    exciseRevenue: Double,
    exciseAfterEvasion: Double,
    customsDutyRevenue: Double,
    effectiveShadowShare: Double,
    mortgageInterestIncome: Double,
    mortgagePrincipal: Double,
    mortgageDefaultLoss: Double,
    mortgageDefaultAmount: Double,
    jstDepositChange: Double,
    investNetDepositFlow: Double,
    actualBondChange: Double,
    m: Int,
    rc: RunConfig
  )

  case class Output(
    newWorld: World,
    finalFirms: Array[Firm],
    reassignedHouseholds: Option[Vector[Household]]
  )

  def run(in: Input): Output =
    // GPW: finalize equity state with HH equity wealth
    val (totalHhEquityWealth, totalWealthEffectAgg) = in.reassignedHouseholds match
      case Some(hhs) =>
        (hhs.kahanSumBy(_.equityWealth.toDouble), 0.0)
      case None =>
        if Config.GpwHhEquity && Config.GpwEnabled then
          val prevHhEq = in.w.equity.hhEquityWealth.toDouble
          val newHhEq = prevHhEq * (1.0 + in.equityAfterIssuance.monthlyReturn.toDouble)
          val wEffect = if in.equityAfterIssuance.monthlyReturn > Rate.Zero then
            (newHhEq - prevHhEq) * Config.GpwWealthEffectMpc
          else 0.0
          (newHhEq, wEffect)
        else (0.0, 0.0)

    val equityAfterStep = in.equityAfterIssuance.copy(
      hhEquityWealth = PLN(totalHhEquityWealth),
      lastWealthEffect = PLN(totalWealthEffectAgg),
      lastDomesticDividends = PLN(in.netDomesticDividends),
      lastForeignDividends = PLN(in.foreignDividendOutflow),
      lastDividendTax = PLN(in.dividendTax)
    )

    // Flow-of-funds residual
    val fofResidual = {
      val totalFirmRev = (0 until SECTORS.length).map { s =>
        in.living.filter(_.sector.toInt == s).kahanSumBy(f =>
          FirmOps.capacity(f).toDouble * in.sectorMults(s) * in.w.priceLevel)
      }.kahanSum
      val adjustedDemand = in.sectorMults.indices.map { s =>
        in.sectorCap(s) * in.sectorMults(s) * in.w.priceLevel
      }.kahanSum
      totalFirmRev - adjustedDemand
    }

    // Informal economy: aggregate metrics and next-month cyclical adjustment (#45)
    val taxEvasionLoss = if Config.InformalEnabled then
      in.sumCitEvasion + (in.vat - in.vatAfterEvasion) + (in.pitRevenue - in.pitAfterEvasion) + (in.exciseRevenue - in.exciseAfterEvasion)
    else 0.0
    val informalEmployed = if Config.InformalEnabled then
      in.employed.toDouble * in.effectiveShadowShare else 0.0
    val newInformalCyclicalAdj = if Config.InformalEnabled then
      val unemp = 1.0 - in.employed.toDouble / Config.TotalPopulation
      val target = Math.max(0.0, unemp - Config.InformalUnempThreshold) * Config.InformalCyclicalSens
      in.w.informalCyclicalAdj * Config.InformalSmoothing + target * (1.0 - Config.InformalSmoothing)
    else 0.0

    val newW = World(in.m, Rate(in.newInfl), in.newPrice, in.newGovWithYield, in.postFxNbp,
      in.resolvedBank, in.newForex,
      HhState(in.employed, PLN(in.newWage), PLN(in.resWage), PLN(in.totalIncome), PLN(in.consumption), PLN(in.domesticCons), PLN(in.importCons),
        minWageLevel = PLN(in.baseMinWage), minWagePriceLevel = in.updatedMinWagePriceLevel),
      Ratio(in.autoR), Ratio(in.hybR), in.gdp, in.newSigmas,
      ioFlows = PLN(in.totalIoPaid),
      bop = in.newBop,
      hhAgg = in.finalHhAgg,
      households = in.reassignedHouseholds,
      bankingSector = in.finalBankingSector,
      monetaryAgg = in.monAgg,
      jst = in.newJst,
      zus = in.newZus,
      ppk = in.finalPpk,
      demographics = in.newDemographics,
      macropru = in.newMacropru,
      equity = equityAfterStep,
      housing = in.housingAfterFlows,
      sectoralMobility = SectoralMobility.State(
        crossSectorHires = in.postFirmCrossSectorHires + in.hhAgg.map(_.crossSectorHires).getOrElse(0),
        voluntaryQuits = in.hhAgg.map(_.voluntaryQuits).getOrElse(0),
        sectorMobilityRate = in.finalHhAgg.map(_.sectorMobilityRate.toDouble).getOrElse(0.0)
      ),
      gvc = in.newGvc,
      expectations = in.newExp,
      immigration = in.newImmig,
      corporateBonds = in.newCorpBonds,
      insurance = in.finalInsurance,
      nbfi = in.finalNbfi,
      sectorDemandMult = in.sectorMults,
      fofResidual = fofResidual,
      grossInvestment = PLN(in.sumGrossInvestment),
      fdiProfitShifting = PLN(in.sumProfitShifting),
      fdiRepatriation = PLN(in.sumFdiRepatriation),
      fdiCitLoss = PLN(in.fdiCitLoss),
      aggInventoryStock = PLN(in.aggInventoryStock),
      aggInventoryChange = PLN(in.aggInventoryChange),
      informalCyclicalAdj = newInformalCyclicalAdj,
      taxEvasionLoss = PLN(taxEvasionLoss),
      informalEmployed = PLN(informalEmployed),
      aggEnergyCost = PLN(in.sumEnergyCost),
      aggGreenCapital = PLN(in.aggGreenCapital),
      aggGreenInvestment = PLN(in.sumGreenInvestment),
      diasporaRemittanceInflow = PLN(in.diasporaInflow),
      tourismExport = PLN(in.tourismExport),
      tourismImport = PLN(in.tourismImport),
      bfgFundBalance = PLN(in.w.bfgFundBalance.toDouble + in.bfgLevy),
      bailInLoss = PLN(in.bailInLoss))

    // SFC accounting check
    val prevSnap = SfcCheck.snapshot(in.w, in.firms, in.households)
    val currSnap = SfcCheck.snapshot(newW, in.reassignedFirms, in.reassignedHouseholds)
    val sfcFlows = SfcCheck.MonthlyFlows(
      govSpending = PLN(in.newGovWithYield.bdpSpending.toDouble + in.newGovWithYield.unempBenefitSpend.toDouble
        + in.newGovWithYield.socialTransferSpend.toDouble
        + in.govPurchases + in.monthlyDebtService + in.newZus.govSubvention.toDouble
        + in.euCofin),
      govRevenue = PLN(in.sumTax + in.dividendTax + in.pitAfterEvasion + in.vatAfterEvasion + in.nbpRemittance + in.exciseAfterEvasion + in.customsDutyRevenue),
      nplLoss = PLN(in.nplLoss),
      interestIncome = PLN(in.intIncome),
      hhDebtService = PLN(in.hhDebtService),
      totalIncome = PLN(in.totalIncome),
      totalConsumption = PLN(in.consumption),
      newLoans = PLN(in.sumNewLoans),
      nplRecovery = PLN(in.nplNew * Config.LoanRecovery),
      currentAccount = in.newBop.currentAccount,
      valuationEffect = PLN(in.oeValuationEffect),
      bankBondIncome = PLN(in.bankBondIncome),
      qePurchase = PLN(in.qePurchaseAmount),
      newBondIssuance = PLN(if Config.GovBondMarket then in.actualBondChange else 0.0),
      depositInterestPaid = PLN(in.depositInterestPaid),
      reserveInterest = PLN(in.totalReserveInterest),
      standingFacilityIncome = PLN(in.totalStandingFacilityIncome),
      interbankInterest = PLN(in.totalInterbankInterest),
      jstDepositChange = PLN(in.jstDepositChange),
      jstSpending = in.newJst.spending,
      jstRevenue = in.newJst.revenue,
      zusContributions = in.newZus.contributions,
      zusPensionPayments = in.newZus.pensionPayments,
      zusGovSubvention = in.newZus.govSubvention,
      dividendIncome = PLN(in.netDomesticDividends),
      foreignDividendOutflow = PLN(in.foreignDividendOutflow),
      dividendTax = PLN(in.dividendTax),
      mortgageInterestIncome = PLN(in.mortgageInterestIncome),
      mortgageNplLoss = PLN(in.mortgageDefaultLoss),
      mortgageOrigination = in.housingAfterFlows.lastOrigination,
      mortgagePrincipalRepaid = PLN(in.mortgagePrincipal),
      mortgageDefaultAmount = PLN(in.mortgageDefaultAmount),
      remittanceOutflow = PLN(in.remittanceOutflow),
      fofResidual = PLN(fofResidual),
      consumerDebtService = PLN(in.consumerDebtService),
      consumerNplLoss = PLN(in.consumerNplLoss),
      consumerOrigination = PLN(in.consumerOrigination),
      consumerPrincipalRepaid = PLN(in.consumerPrincipal),
      consumerDefaultAmount = PLN(in.consumerDefaultAmt),
      corpBondCouponIncome = PLN(in.corpBondBankCoupon),
      corpBondDefaultLoss = PLN(in.corpBondBankDefaultLoss),
      corpBondIssuance = PLN(in.actualBondIssuance),
      corpBondAmortization = PLN(in.corpBondAmort),
      corpBondDefaultAmount = PLN(in.totalBondDefault),
      insNetDepositChange = PLN(in.insNetDepositChange),
      nbfiDepositDrain = PLN(in.nbfiDepositDrain),
      nbfiOrigination = in.finalNbfi.lastNbfiOrigination,
      nbfiRepayment = in.finalNbfi.lastNbfiRepayment,
      nbfiDefaultAmount = in.finalNbfi.lastNbfiDefaultAmount,
      fdiProfitShifting = PLN(in.sumProfitShifting),
      fdiRepatriation = PLN(in.sumFdiRepatriation),
      diasporaInflow = PLN(in.diasporaInflow),
      tourismExport = PLN(in.tourismExport),
      tourismImport = PLN(in.tourismImport),
      bfgLevy = PLN(in.bfgLevy),
      bailInLoss = PLN(in.bailInLoss),
      bankCapitalDestruction = PLN(in.multiCapDestruction),
      investNetDepositFlow = PLN(in.investNetDepositFlow),
      exports = in.newBop.exports,
      totalImports = in.newBop.totalImports,
      grossInvestment = PLN(in.sumGrossInvestment),
      greenInvestment = PLN(in.sumGreenInvestment),
      inventoryChange = PLN(in.aggInventoryChange)
    )
    val sfcResult = SfcCheck.validate(in.m, prevSnap, currSnap, sfcFlows)
    if !sfcResult.passed then
      System.err.println(
        f"[SFC] Month ${in.m} FAIL:" +
        f" bankCap=${sfcResult.bankCapitalError}%.2f" +
        f" bankDep=${sfcResult.bankDepositsError}%.2f" +
        f" govDebt=${sfcResult.govDebtError}%.2f" +
        f" nfa=${sfcResult.nfaError}%.2f" +
        f" bondClr=${sfcResult.bondClearingError}%.2f" +
        f" ibNet=${sfcResult.interbankNettingError}%.2f" +
        f" jstDebt=${sfcResult.jstDebtError}%.2f" +
        f" fusBal=${sfcResult.fusBalanceError}%.2f" +
        f" mortgage=${sfcResult.mortgageStockError}%.2f" +
        f" fof=${sfcResult.fofError}%.2f" +
        f" ccStock=${sfcResult.consumerCreditError}%.2f" +
        f" corpBond=${sfcResult.corpBondStockError}%.2f" +
        f" nbfiCredit=${sfcResult.nbfiCreditError}%.2f" +
        f" secBal=${sfcResult.sectoralBalancesError}%.2f")

    // FDI M&A: monthly domestic → foreign conversion (#33)
    val postFdiFirms = if Config.FdiEnabled && Config.FdiMaProb > 0 then
      in.reassignedFirms.map { f =>
        if FirmOps.isAlive(f) && !f.foreignOwned &&
           f.initialSize >= Config.FdiMaSizeMin &&
           Random.nextDouble() < Config.FdiMaProb then
          f.copy(foreignOwned = true)
        else f
      }
    else in.reassignedFirms

    // Endogenous Firm Entry (#35): recycle bankrupt slots
    val (finalFirms, firmBirths) = if Config.FirmEntryEnabled then
      val postLiving = postFdiFirms.filter(FirmOps.isAlive)
      val sectorCashSum = Array.fill(SECTORS.length)(0.0)
      val sectorCashCnt = Array.fill(SECTORS.length)(0)
      for f <- postLiving do
        sectorCashSum(f.sector.toInt) += f.cash.toDouble
        sectorCashCnt(f.sector.toInt) += 1
      val sectorAvgCash = SECTORS.indices.map(s =>
        if sectorCashCnt(s) > 0 then sectorCashSum(s) / sectorCashCnt(s) else 0.0).toArray
      val globalAvgCash = if postLiving.nonEmpty then
        postLiving.map(_.cash.toDouble).sum / postLiving.length else 1.0
      val profitSignals = sectorAvgCash.map { c =>
        Math.max(-1.0, Math.min(2.0,
          (c - globalAvgCash) / Math.max(1.0, Math.abs(globalAvgCash))))
      }

      val sectorWeights = SECTORS.indices.map { s =>
        Math.max(0.01, (1.0 + profitSignals(s) * Config.FirmEntryProfitSens) *
          Config.FirmEntrySectorBarriers(s))
      }.toArray
      val totalWeight = sectorWeights.sum

      val totalAdoption = newW.automationRatio.toDouble + newW.hybridRatio.toDouble
      val livingIds = postLiving.map(_.id.toInt)
      var births = 0

      val result = postFdiFirms.map { f =>
        if !FirmOps.isAlive(f) then
          val slotSector = f.sector.toInt
          val entryProb = Config.FirmEntryRate * Config.FirmEntrySectorBarriers(slotSector) *
            Math.max(0.0, 1.0 + profitSignals(slotSector) * Config.FirmEntryProfitSens)
          if Random.nextDouble() < entryProb then
            births += 1
            val roll = Random.nextDouble() * totalWeight
            var cumul = 0.0
            var newSector = 0
            var found = false
            for s <- SECTORS.indices if !found do
              cumul += sectorWeights(s)
              if roll < cumul then { newSector = s; found = true }

            val firmSize = Math.max(1, Random.between(1, 10))
            val sizeMult = firmSize.toDouble / Config.WorkersPerFirm

            val isAiNative = totalAdoption > Config.FirmEntryAiThreshold &&
              Random.nextDouble() < Config.FirmEntryAiProb
            val dr = if isAiNative then Random.between(0.50, 0.90)
                     else Math.max(0.02, Math.min(0.30,
                       SECTORS(newSector).baseDigitalReadiness.toDouble + Random.nextGaussian() * 0.10))
            val startWorkers = if in.households.isDefined then 0 else firmSize
            val tech = if isAiNative then
              val hw = Math.max(1, (startWorkers * 0.6).toInt)
              TechState.Hybrid(hw, 0.5 + Random.nextDouble() * 0.3)
            else TechState.Traditional(startWorkers)

            val nNeighbors = Math.min(6, livingIds.length)
            val newNeighbors = if nNeighbors > 0 then
              Random.shuffle(livingIds.toList).take(nNeighbors).toArray
            else Array.empty[Int]

            val newBankId = if Config.BankMulti then
              BankingSector.assignBank(SectorIdx(newSector), BankingSector.DefaultConfigs, Random)
            else BankId(0)

            val foreignOwned = Config.FdiEnabled &&
              Random.nextDouble() < Config.FdiForeignShares(newSector)

            val capitalStock = if Config.PhysCapEnabled then
              firmSize.toDouble * Config.PhysCapKLRatios(newSector)
            else 0.0

            val initInventory = if Config.InventoryEnabled then
              val cap = Config.BaseRevenue * (firmSize.toDouble / Config.WorkersPerFirm) *
                SECTORS(newSector).revenueMultiplier
              cap * Config.InventoryTargetRatios(newSector) * Config.InventoryInitRatio
            else 0.0

            val initGreenK = if Config.EnergyEnabled then
              firmSize.toDouble * Config.GreenKLRatios(newSector) * Config.GreenInitRatio
            else 0.0

            Firm(
              id = f.id,
              cash = PLN(Config.FirmEntryStartupCash * sizeMult),
              debt = PLN.Zero,
              tech = tech,
              riskProfile = Ratio(Random.between(0.1, 0.9)),
              innovationCostFactor = Random.between(0.8, 1.5),
              digitalReadiness = Ratio(dr),
              sector = SectorIdx(newSector),
              neighbors = newNeighbors,
              bankId = newBankId,
              initialSize = firmSize,
              capitalStock = PLN(capitalStock),
              foreignOwned = foreignOwned,
              inventory = PLN(initInventory),
              greenCapital = PLN(initGreenK)
            )
          else f
        else f
      }
      (result, births)
    else (postFdiFirms, 0)

    val finalW = newW.copy(firmBirths = firmBirths, firmDeaths = in.firmDeaths)
    Output(finalW, finalFirms, in.reassignedHouseholds)
