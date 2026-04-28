package com.boombustgroup.amorfati.diagnostics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.{MonthRandomness, SignalExtraction}
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.economics.*
import com.boombustgroup.amorfati.engine.markets.{PriceLevel, RegionalClearing}
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*

object InflationProbe:

  private def laborSupplyCount(wage: PLN, resWage: PLN, totalPopulation: Int)(using p: SimParams): Int =
    if resWage <= PLN.Zero then if wage > PLN.Zero then totalPopulation else 0
    else
      val wageGap     = wage.ratioTo(resWage).toCoefficient - Coefficient.One
      val slope       = p.household.laborSupplySteepness * wageGap
      val denominator = Multiplier.One + (-slope).exp
      Multiplier.One.ratioTo(denominator).toShare.applyTo(totalPopulation)

  private def pct(value: Scalar, digits: Int = 2): String =
    (value * Scalar(100)).format(digits) + "%"

  private def topPressures(pressures: Vector[Multiplier])(using p: SimParams): String =
    p.sectorDefs
      .zip(pressures)
      .sortBy { case (_, v) => -(v / Multiplier.One) }
      .take(3)
      .map { case (sec, v) =>
        s"${sec.name}=${(v / Multiplier.One).format(2)}"
      }
      .mkString(", ")

  private case class GovPurchasesBreakdown(
      zusNetSurplus: PLN,
      unempGap: Share,
      base: PLN,
      recycleCounterfactual: PLN,
      stimulus: PLN,
      rawTarget: PLN,
  )

  private def govPurchasesBreakdown(world: World, employed: Int)(using p: SimParams): GovPurchasesBreakdown =
    val zusNetSurplus = (world.social.zus.contributions - world.social.zus.pensionPayments).max(PLN.Zero)
    val unempRate     = Share.One - Share.fraction(employed, world.derivedTotalPopulation)
    val unempGap      = (unempRate - p.monetary.nairu).max(Share.Zero)
    val base          = p.fiscal.govBaseSpending * world.priceLevel.toMultiplier.max(Multiplier.One)
    val recycling     = (world.gov.taxRevenue + zusNetSurplus) * p.fiscal.govFiscalRecyclingRate
    val stimulus      = p.fiscal.govBaseSpending * unempGap * p.fiscal.govAutoStabMult
    GovPurchasesBreakdown(zusNetSurplus, unempGap, base, recycling, stimulus, base + stimulus)

  @main def runInflationProbe(seed: Long = 1L, months: Int = 12): Unit =
    given SimParams = SimParams.defaults

    val init                 = WorldInit.initialize(InitRandomness.Contract.fromSeed(seed))
    var world                = init.world
    var firms                = init.firms
    var hhs                  = init.households
    var banks                = init.banks
    var ledgerFinancialState = init.ledgerFinancialState

    println(s"seed=$seed months=$months")

    (1 to months).foreach: month =>
      val population        = world.derivedTotalPopulation.max(1)
      val contract          = MonthRandomness.Contract.fromSeed(seed * 1000 + month)
      val s1                = FiscalConstraintEconomics.compute(world, banks, ledgerFinancialState, ExecutionMonth(month))
      val s2Pre             = LaborEconomics.compute(world, firms, hhs, s1)
      val prevWage          = world.householdMarket.marketWage
      val rawLaborWage      = RegionalClearing.clear(world.regionalWages, s1.resWage, s2Pre.laborDemand, population).nationalWage
      val expWagePressure   =
        (summon[SimParams].labor.expWagePassthrough * (world.mechanisms.expectations.expectedInflation - summon[SimParams].monetary.targetInfl)
          .max(Rate.Zero)
          .toCoefficient) / 12
      val wageAfterExp      = s1.resWage.max(rawLaborWage * expWagePressure.growthMultiplier)
      val aggUnionDensity   =
        summon[SimParams].sectorDefs.zipWithIndex
          .map((s, i) => s.share * summon[SimParams].labor.unionDensity(i))
          .foldLeft(Share.Zero)(_ + _)
      val unionAdjustedWage =
        if wageAfterExp < prevWage then
          val decline = prevWage - wageAfterExp
          s1.resWage.max(wageAfterExp + decline * summon[SimParams].labor.unionRigidity * aggUnionDensity)
        else wageAfterExp
      val supplyAtPrev      = laborSupplyCount(world.householdMarket.marketWage, s1.resWage, population)
      val newSupply         = laborSupplyCount(unionAdjustedWage, s1.resWage, population)
      val excessDemand      = (s2Pre.laborDemand - supplyAtPrev).ratioTo(population)
      val phillipsGrowth    = if prevWage > PLN.Zero then rawLaborWage / prevWage - Scalar.One else Scalar.Zero
      val expGrowth         = if rawLaborWage > PLN.Zero then wageAfterExp / rawLaborWage - Scalar.One else Scalar.Zero
      val unionGrowth       = if wageAfterExp > PLN.Zero then unionAdjustedWage / wageAfterExp - Scalar.One else Scalar.Zero
      val s3                =
        HouseholdIncomeEconomics.compute(
          world,
          firms,
          hhs,
          banks,
          ledgerFinancialState,
          s1.lendingBaseRate,
          s1.resWage,
          s2Pre.newWage,
          contract.stages.householdIncomeEconomics.newStream(),
        )
      val s4                = DemandEconomics.compute(world, s2Pre.employed, s2Pre.living, s3.domesticCons)
      val s5                = FirmEconomics.runStep(world, firms, hhs, banks, ledgerFinancialState, s1, s2Pre, s3, s4, contract.stages.firmEconomics.newStream())
      val living            = s5.ioFirms.filter(Firm.isAlive)
      val s2                = LaborEconomics.reconcilePostFirmStep(world, s1, s2Pre, living, s5.households)
      val s6                =
        HouseholdFinancialEconomics.compute(world, s1.m, s2.employed, s3.hhAgg, contract.stages.householdFinancialEconomics.newStream())
      val s7                = PriceEquityEconomics.compute(
        w = world,
        month = s1.m,
        wageGrowth = s2.wageGrowth,
        avgDemandMult = s4.avgDemandMult,
        sectorMults = s4.sectorMults,
        totalSystemLoans = ledgerFinancialState.banks.map(_.firmLoan).sumPln,
        firmStep = s5,
      )
      val s8                =
        OpenEconEconomics.runStep(
          OpenEconEconomics.StepInput(
            world,
            ledgerFinancialState,
            s1,
            s2,
            s3,
            s4,
            s5,
            s6,
            s7,
            banks,
            contract.stages.openEconEconomics.newStream(),
          ),
        )
      val s9                =
        BankingEconomics.runStep(
          BankingEconomics.StepInput(
            world,
            ledgerFinancialState,
            s1,
            s2,
            s3,
            s4,
            s5,
            s6,
            s7,
            s8,
            banks,
            contract.stages.bankingEconomics.newStream(),
          ),
        )

      val exRateDeviation = world.forex.exchangeRate.deviationFrom(summon[SimParams].forex.baseExRate)
      val priceUpd        = PriceLevel.update(
        // PriceLevel reads the current pre-policy expectation; s8.newExp is next month's post-policy anchor.
        expectedInflation = world.mechanisms.expectations.expectedInflation,
        prevPrice = world.priceLevel,
        demandMult = s4.avgDemandMult,
        wageGrowth = s2.wageGrowth,
        exRateDeviation = exRateDeviation,
      )
      val unemp           = Share.One - Share.fraction(s2.employed, population)
      val realRate        = s8.monetary.newRefRate - s8.monetary.newExp.expectedInflation
      val govBreakdown    = govPurchasesBreakdown(world, s2Pre.employed)
      val debtToGdp       =
        if s7.gdp > PLN.Zero then s9.newGovWithYield.cumulativeDebt / (s7.gdp * 12) else Scalar.Zero
      val deficitToGdp    =
        if s7.gdp > PLN.Zero then s9.newGovWithYield.deficit / s7.gdp else Scalar.Zero

      println(
        s"m=$month u=${pct(unemp.toScalar)} pi=${pct(s7.newInfl.toScalar)} wage=${s2.newWage.format(0)} wg=${pct(s2.wageGrowth.toScalar)} demand=${(s4.avgDemandMult / Multiplier.One).format(3)} markup=${pct(s5.markupInflation.toScalar)}",
      )
      println(
        s"  channels monthly: demand=${pct(priceUpd.demandPull.toScalar)}pp cost=${pct(priceUpd.costPush.toScalar)}pp import=${pct(priceUpd.importPush.toScalar)}pp raw=${pct(priceUpd.rawMonthly.toScalar)}pp floor=${pct(priceUpd.flooredMonthly.toScalar)}pp",
      )
      println(
        s"  annualized: base=${pct(priceUpd.inflation.toScalar)} markup=${pct(s5.markupInflation.toScalar)} total=${pct(s7.newInfl.toScalar)} exDev=${pct(exRateDeviation.toCoefficient.toScalar)} importCost=${world.external.gvc.importCostIndex.format(3)} commodity=${world.external.gvc.commodityPriceIndex.format(3)}",
      )
      println(
        s"  policy: ref=${pct(s8.monetary.newRefRate.toScalar)} expPi=${pct(s8.monetary.newExp.expectedInflation.toScalar)} real=${pct(realRate.toScalar)} cred=${pct(s8.monetary.newExp.credibility.toScalar)} fg=${pct(s8.monetary.newExp.forwardGuidanceRate.toScalar)}",
      )
      println(
        s"  wages: phillips=${pct(phillipsGrowth)} exp=${pct(expGrowth)} union=${pct(unionGrowth)} raw=${rawLaborWage.format(0)} afterExp=${wageAfterExp.format(0)} final=${unionAdjustedWage.format(0)}",
      )
      println(
        s"  labor: demand=${s2Pre.laborDemand} supplyPrev=${supplyAtPrev} supplyNew=${newSupply} excess=${pct(excessDemand)} employedPre=${s2Pre.employed} employedPost=${s2.employed}",
      )
      println(
        s"  fiscal: govPurch=${s4.govPurchases.format(0)} govCur=${s9.newGovWithYield.govCurrentSpend.format(0)} govCapDom=${s9.newGovWithYield.govCapitalSpend.format(0)} euProjCap=${s9.newGovWithYield.euProjectCapital.format(0)} euCofinDom=${s9.newGovWithYield.euCofinancing.format(0)} def=${s9.newGovWithYield.deficit.format(0)} def/gdp=${pct(deficitToGdp)} debt/gdp=${pct(debtToGdp)} rule=${s4.fiscalRuleStatus.bindingRule} cut=${pct(s4.fiscalRuleStatus.spendingCutRatio.toScalar)}",
      )
      println(
        s"  gov raw target: base=${govBreakdown.base.format(0)} recycleCf=${govBreakdown.recycleCounterfactual.format(0)} stimulus=${govBreakdown.stimulus.format(0)} raw=${govBreakdown.rawTarget.format(0)} zusSurplus=${govBreakdown.zusNetSurplus.format(0)} unempGap=${pct(govBreakdown.unempGap.toScalar)}",
      )
      println(s"  top pressure: ${topPressures(s4.sectorDemandPressure)}")

      val assemblyInput = WorldAssemblyEconomics.StepInput(
        world,
        s1,
        s2,
        s3,
        s4,
        s5,
        s6,
        s7,
        s8,
        s9,
      )
      val assembled     = WorldAssemblyEconomics.computePostMonth(assemblyInput, contract.assembly.newStreams())
      val seedOut       = SignalExtraction
        .fromPostMonth(
          world = assembled.world,
          households = assembled.households,
          operationalHiringSlack = assemblyInput.s2.operationalHiringSlack,
          startupAbsorptionRate = assembled.startupAbsorptionRate,
          demand = SignalExtraction.DemandOutcomes(
            sectorDemandMult = assemblyInput.s4.sectorMults,
            sectorDemandPressure = assemblyInput.s4.sectorDemandPressure,
            sectorHiringSignal = assemblyInput.s4.sectorHiringSignal,
          ),
        )
        .seedOut

      world = assembled.world.copy(pipeline = assembled.world.pipeline.withDecisionSignals(seedOut))
      firms = assembled.firms
      hhs = assembled.households
      banks = assembled.banks
      ledgerFinancialState = assembled.ledgerFinancialState
