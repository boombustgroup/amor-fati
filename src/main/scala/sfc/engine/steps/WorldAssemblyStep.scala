package sfc.engine.steps

import sfc.accounting.*
import sfc.agents.*
import sfc.config.{Config, RunConfig, SECTORS}
import sfc.engine.*
import sfc.types.*
import sfc.util.KahanSum.*

import scala.util.Random

object WorldAssemblyStep:

  case class Input(
    w: World,
    rc: RunConfig,
    firms: Vector[Firm.State],
    households: Vector[Household.State],
    s1: FiscalConstraintStep.Output,
    s2: LaborDemographicsStep.Output,
    s3: HouseholdIncomeStep.Output,
    s4: DemandStep.Output,
    s5: FirmProcessingStep.Output,
    s6: HouseholdFinancialStep.Output,
    s7: PriceEquityStep.Output,
    s8: OpenEconomyStep.Output,
    s9: BankUpdateStep.Output,
  )

  case class Output(
    newWorld: World,
    finalFirms: Vector[Firm.State],
    reassignedHouseholds: Vector[Household.State],
    sfcResult: Sfc.SfcResult,
  )

  def run(in: Input): Output =
    // GPW: finalize equity state with HH equity wealth
    val totalHhEquityWealth = in.s9.reassignedHouseholds.kahanSumBy(_.equityWealth.toDouble)

    val equityAfterStep = in.s7.equityAfterIssuance.copy(
      hhEquityWealth = PLN(totalHhEquityWealth),
      lastWealthEffect = PLN.Zero,
      lastDomesticDividends = PLN(in.s7.netDomesticDividends),
      lastForeignDividends = PLN(in.s7.foreignDividendOutflow),
      lastDividendTax = PLN(in.s7.dividendTax),
    )

    // Flow-of-funds residual
    val fofResidual = {
      val totalFirmRev = (0 until SECTORS.length).map { s =>
        in.s2.living
          .filter(_.sector.toInt == s)
          .kahanSumBy(f => Firm.capacity(f).toDouble * in.s4.sectorMults(s) * in.w.priceLevel)
      }.kahanSum
      val adjustedDemand = in.s4.sectorMults.indices.map { s =>
        in.s4.sectorCap(s) * in.s4.sectorMults(s) * in.w.priceLevel
      }.kahanSum
      totalFirmRev - adjustedDemand
    }

    // Informal economy: aggregate metrics and next-month cyclical adjustment (#45)
    val taxEvasionLoss =
      if Config.InformalEnabled then
        in.s5.sumCitEvasion + (in.s9.vat - in.s9.vatAfterEvasion) + (in.s3.pitRevenue - in.s9.pitAfterEvasion) + (in.s9.exciseRevenue - in.s9.exciseAfterEvasion)
      else 0.0
    val informalEmployed = if Config.InformalEnabled then in.s2.employed.toDouble * in.s9.effectiveShadowShare else 0.0
    val newInformalCyclicalAdj = if Config.InformalEnabled then
      val unemp = 1.0 - in.s2.employed.toDouble / Config.TotalPopulation
      val target = Math.max(0.0, unemp - Config.InformalUnempThreshold) * Config.InformalCyclicalSens
      in.w.informalCyclicalAdj * Config.InformalSmoothing + target * (1.0 - Config.InformalSmoothing)
    else 0.0

    // Pre-compute values surfaced on World for SimOutput
    val aliveBanksForObs = in.s9.finalBankingSector.banks.filterNot(_.failed)
    val depositFacilityUsage = aliveBanksForObs
      .filter(_.reservesAtNbp > PLN.Zero)
      .kahanSumBy(_.reservesAtNbp.toDouble)
    val etsPrice =
      if Config.EnergyEnabled then Config.EtsBasePrice * Math.pow(1.0 + Config.EtsPriceDrift / 12.0, in.s1.m.toDouble)
      else 0.0
    val monthInYear = ((in.s1.m - 1) % 12) + 1
    val tourismSeasonalFactor =
      1.0 + Config.TourismSeasonality * Math.cos(2 * Math.PI * (monthInYear - Config.TourismPeakMonth) / 12.0)

    val newW = World(
      in.s1.m,
      Rate(in.s7.newInfl),
      in.s7.newPrice,
      in.s9.newGovWithYield,
      in.s8.postFxNbp,
      in.s9.resolvedBank,
      in.s8.newForex,
      Household.SectorState(
        in.s2.employed,
        PLN(in.s2.newWage),
        PLN(in.s1.resWage),
        PLN(in.s3.totalIncome),
        PLN(in.s3.consumption),
        PLN(in.s3.domesticCons),
        PLN(in.s3.importCons),
        minWageLevel = PLN(in.s1.baseMinWage),
        minWagePriceLevel = in.s1.updatedMinWagePriceLevel,
      ),
      Ratio(in.s7.autoR),
      Ratio(in.s7.hybR),
      in.s7.gdp,
      in.s7.newSigmas,
      ioFlows = PLN(in.s5.totalIoPaid),
      bop = in.s8.newBop,
      hhAgg = Some(in.s9.finalHhAgg),
      households = in.s9.reassignedHouseholds,
      bankingSector = in.s9.finalBankingSector,
      monetaryAgg = in.s9.monAgg,
      jst = in.s9.newJst,
      zus = in.s2.newZus,
      ppk = in.s9.finalPpk,
      demographics = in.s2.newDemographics,
      macropru = in.s7.newMacropru,
      equity = equityAfterStep,
      housing = in.s9.housingAfterFlows,
      sectoralMobility = SectoralMobility.State(
        crossSectorHires = in.s5.postFirmCrossSectorHires + in.s3.hhAgg.crossSectorHires,
        voluntaryQuits = in.s3.hhAgg.voluntaryQuits,
        sectorMobilityRate = in.s9.finalHhAgg.sectorMobilityRate.toDouble,
      ),
      gvc = in.s8.newGvc,
      expectations = in.s8.newExp,
      immigration = in.s2.newImmig,
      corporateBonds = in.s8.newCorpBonds,
      insurance = in.s9.finalInsurance,
      nbfi = in.s9.finalNbfi,
      sectorDemandMult = in.s4.sectorMults,
      fofResidual = fofResidual,
      grossInvestment = PLN(in.s5.sumGrossInvestment),
      fdiProfitShifting = PLN(in.s5.sumProfitShifting),
      fdiRepatriation = PLN(in.s5.sumFdiRepatriation),
      fdiCitLoss = PLN(in.s8.fdiCitLoss),
      aggInventoryStock = PLN(in.s7.aggInventoryStock),
      aggInventoryChange = PLN(in.s7.aggInventoryChange),
      informalCyclicalAdj = newInformalCyclicalAdj,
      taxEvasionLoss = PLN(taxEvasionLoss),
      informalEmployed = PLN(informalEmployed),
      aggEnergyCost = PLN(in.s5.sumEnergyCost),
      aggGreenCapital = PLN(in.s7.aggGreenCapital),
      aggGreenInvestment = PLN(in.s5.sumGreenInvestment),
      diasporaRemittanceInflow = PLN(in.s6.diasporaInflow),
      tourismExport = PLN(in.s6.tourismExport),
      tourismImport = PLN(in.s6.tourismImport),
      bfgFundBalance = PLN(in.w.bfgFundBalance.toDouble + in.s9.bfgLevy),
      bailInLoss = PLN(in.s9.bailInLoss),
      effectiveShadowShare =
        if Config.InformalEnabled then
          Config.FofConsWeights
            .zip(Config.InformalSectorShares)
            .map((cw, ss) => cw * Math.min(1.0, ss + newInformalCyclicalAdj))
            .sum
        else 0.0,
      bfgLevyTotal = in.s9.bfgLevy,
      reserveInterestTotal = in.s8.totalReserveInterest,
      standingFacilityNet = in.s8.totalStandingFacilityIncome,
      interbankInterestNet = in.s8.totalInterbankInterest,
      depositFacilityUsage = depositFacilityUsage,
      etsPrice = etsPrice,
      tourismSeasonalFactor = tourismSeasonalFactor,
    )

    // SFC accounting check
    val prevSnap = Sfc.snapshot(in.w, in.firms, in.w.households)
    val currSnap = Sfc.snapshot(newW, in.s9.reassignedFirms, in.s9.reassignedHouseholds)
    val sfcFlows = Sfc.MonthlyFlows(
      govSpending = PLN(
        in.s9.newGovWithYield.bdpSpending.toDouble + in.s9.newGovWithYield.unempBenefitSpend.toDouble
          + in.s9.newGovWithYield.socialTransferSpend.toDouble
          + in.s4.govPurchases + in.s8.monthlyDebtService + in.s2.newZus.govSubvention.toDouble
          + in.s7.euCofin,
      ),
      govRevenue = PLN(
        in.s5.sumTax + in.s7.dividendTax + in.s9.pitAfterEvasion + in.s9.vatAfterEvasion + in.s8.nbpRemittance + in.s9.exciseAfterEvasion + in.s9.customsDutyRevenue,
      ),
      nplLoss = PLN(in.s5.nplLoss),
      interestIncome = PLN(in.s5.intIncome),
      hhDebtService = PLN(in.s6.hhDebtService),
      totalIncome = PLN(in.s3.totalIncome),
      totalConsumption = PLN(in.s3.consumption),
      newLoans = PLN(in.s5.sumNewLoans),
      nplRecovery = PLN(in.s5.nplNew * Config.LoanRecovery),
      currentAccount = in.s8.newBop.currentAccount,
      valuationEffect = PLN(in.s8.oeValuationEffect),
      bankBondIncome = PLN(in.s8.bankBondIncome),
      qePurchase = PLN(in.s8.qePurchaseAmount),
      newBondIssuance = PLN(if Config.GovBondMarket then in.s9.actualBondChange else 0.0),
      depositInterestPaid = PLN(in.s6.depositInterestPaid),
      reserveInterest = PLN(in.s8.totalReserveInterest),
      standingFacilityIncome = PLN(in.s8.totalStandingFacilityIncome),
      interbankInterest = PLN(in.s8.totalInterbankInterest),
      jstDepositChange = PLN(in.s9.jstDepositChange),
      jstSpending = in.s9.newJst.spending,
      jstRevenue = in.s9.newJst.revenue,
      zusContributions = in.s2.newZus.contributions,
      zusPensionPayments = in.s2.newZus.pensionPayments,
      zusGovSubvention = in.s2.newZus.govSubvention,
      dividendIncome = PLN(in.s7.netDomesticDividends),
      foreignDividendOutflow = PLN(in.s7.foreignDividendOutflow),
      dividendTax = PLN(in.s7.dividendTax),
      mortgageInterestIncome = PLN(in.s9.mortgageInterestIncome),
      mortgageNplLoss = PLN(in.s9.mortgageDefaultLoss),
      mortgageOrigination = in.s9.housingAfterFlows.lastOrigination,
      mortgagePrincipalRepaid = PLN(in.s9.mortgagePrincipal),
      mortgageDefaultAmount = PLN(in.s9.mortgageDefaultAmount),
      remittanceOutflow = PLN(in.s6.remittanceOutflow),
      fofResidual = PLN(fofResidual),
      consumerDebtService = PLN(in.s6.consumerDebtService),
      consumerNplLoss = PLN(in.s6.consumerNplLoss),
      consumerOrigination = PLN(in.s6.consumerOrigination),
      consumerPrincipalRepaid = PLN(in.s6.consumerPrincipal),
      consumerDefaultAmount = PLN(in.s6.consumerDefaultAmt),
      corpBondCouponIncome = PLN(in.s8.corpBondBankCoupon),
      corpBondDefaultLoss = PLN(in.s8.corpBondBankDefaultLoss),
      corpBondIssuance = PLN(in.s5.actualBondIssuance),
      corpBondAmortization = PLN(in.s8.corpBondAmort),
      corpBondDefaultAmount = PLN(in.s5.totalBondDefault),
      insNetDepositChange = PLN(in.s8.insNetDepositChange),
      nbfiDepositDrain = PLN(in.s8.nbfiDepositDrain),
      nbfiOrigination = in.s9.finalNbfi.lastNbfiOrigination,
      nbfiRepayment = in.s9.finalNbfi.lastNbfiRepayment,
      nbfiDefaultAmount = in.s9.finalNbfi.lastNbfiDefaultAmount,
      fdiProfitShifting = PLN(in.s5.sumProfitShifting),
      fdiRepatriation = PLN(in.s5.sumFdiRepatriation),
      diasporaInflow = PLN(in.s6.diasporaInflow),
      tourismExport = PLN(in.s6.tourismExport),
      tourismImport = PLN(in.s6.tourismImport),
      bfgLevy = PLN(in.s9.bfgLevy),
      bailInLoss = PLN(in.s9.bailInLoss),
      bankCapitalDestruction = PLN(in.s9.multiCapDestruction),
      investNetDepositFlow = PLN(in.s9.investNetDepositFlow),
    )
    val sfcResult = Sfc.validate(prevSnap, currSnap, sfcFlows)

    // FDI M&A: monthly domestic → foreign conversion (#33)
    val postFdiFirms =
      if Config.FdiEnabled && Config.FdiMaProb > 0 then
        in.s9.reassignedFirms.map { f =>
          if Firm.isAlive(f) && !f.foreignOwned &&
            f.initialSize >= Config.FdiMaSizeMin &&
            Random.nextDouble() < Config.FdiMaProb
          then f.copy(foreignOwned = true)
          else f
        }
      else in.s9.reassignedFirms

    // Endogenous Firm Entry (#35): recycle bankrupt slots
    val (finalFirms, firmBirths) = if Config.FirmEntryEnabled then
      val postLiving = postFdiFirms.filter(Firm.isAlive)
      val sectorCashSum = Array.fill(SECTORS.length)(0.0)
      val sectorCashCnt = Array.fill(SECTORS.length)(0)
      for f <- postLiving do
        sectorCashSum(f.sector.toInt) += f.cash.toDouble
        sectorCashCnt(f.sector.toInt) += 1
      val sectorAvgCash =
        SECTORS.indices.map(s => if sectorCashCnt(s) > 0 then sectorCashSum(s) / sectorCashCnt(s) else 0.0).toArray
      val globalAvgCash = if postLiving.nonEmpty then postLiving.map(_.cash.toDouble).sum / postLiving.length else 1.0
      val profitSignals = sectorAvgCash.map { c =>
        Math.max(-1.0, Math.min(2.0, (c - globalAvgCash) / Math.max(1.0, Math.abs(globalAvgCash))))
      }

      val sectorWeights = SECTORS.indices.map { s =>
        Math.max(
          0.01,
          (1.0 + profitSignals(s) * Config.FirmEntryProfitSens) *
            Config.FirmEntrySectorBarriers(s),
        )
      }.toArray
      val totalWeight = sectorWeights.sum

      val totalAdoption = newW.automationRatio.toDouble + newW.hybridRatio.toDouble
      val livingIds = postLiving.map(_.id.toInt)
      var births = 0

      val result = postFdiFirms.map { f =>
        if !Firm.isAlive(f) then
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
            val dr =
              if isAiNative then Random.between(0.50, 0.90)
              else
                Math.max(
                  0.02,
                  Math.min(0.30, SECTORS(newSector).baseDigitalReadiness.toDouble + Random.nextGaussian() * 0.10),
                )
            val startWorkers = 0 // workers hired via labor market
            val tech = if isAiNative then
              val hw = Math.max(1, (startWorkers * 0.6).toInt)
              TechState.Hybrid(hw, 0.5 + Random.nextDouble() * 0.3)
            else TechState.Traditional(startWorkers)

            val nNeighbors = Math.min(6, livingIds.length)
            val newNeighbors =
              if nNeighbors > 0 then Random.shuffle(livingIds.toList).take(nNeighbors).map(FirmId(_)).toArray
              else Array.empty[FirmId]

            val newBankId = Banking.assignBank(SectorIdx(newSector), Banking.DefaultConfigs, Random)

            val foreignOwned = Config.FdiEnabled &&
              Random.nextDouble() < Config.FdiForeignShares(newSector)

            val capitalStock =
              if Config.PhysCapEnabled then firmSize.toDouble * Config.PhysCapKLRatios(newSector)
              else 0.0

            val initInventory = if Config.InventoryEnabled then
              val cap = Config.BaseRevenue * (firmSize.toDouble / Config.WorkersPerFirm) *
                SECTORS(newSector).revenueMultiplier
              cap * Config.InventoryTargetRatios(newSector) * Config.InventoryInitRatio
            else 0.0

            val initGreenK =
              if Config.EnergyEnabled then firmSize.toDouble * Config.GreenKLRatios(newSector) * Config.GreenInitRatio
              else 0.0

            Firm.State(
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
              greenCapital = PLN(initGreenK),
            )
          else f
        else f
      }
      (result, births)
    else (postFdiFirms, 0)

    val finalW = newW.copy(firmBirths = firmBirths, firmDeaths = in.s5.firmDeaths)
    Output(finalW, finalFirms, in.s9.reassignedHouseholds, sfcResult)
