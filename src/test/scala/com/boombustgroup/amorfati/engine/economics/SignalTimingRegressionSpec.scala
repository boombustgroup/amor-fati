package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.{DecisionSignals, MonthRandomness, MonthTraceStage, OperationalSignals, SignalExtraction, World}
import com.boombustgroup.amorfati.engine.ledger.LedgerFinancialState
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SignalTimingRegressionSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private case class PipelineFixture(
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      ledgerFinancialState: LedgerFinancialState,
      s1: FiscalConstraintEconomics.Output,
      s2Pre: LaborEconomics.Output,
      s2: LaborEconomics.Output,
      s3: HouseholdIncomeEconomics.Output,
      s4: DemandEconomics.Output,
      s5: FirmEconomics.StepOutput,
      s6: HouseholdFinancialEconomics.Output,
      s7: PriceEquityEconomics.Output,
      s8: OpenEconEconomics.StepOutput,
      s9: BankingEconomics.StepOutput,
  )

  private val baseline = buildFixture(42L)

  private def buildFixture(seed: Long): PipelineFixture =
    val init                 = WorldInit.initialize(InitRandomness.Contract.fromSeed(seed))
    val world                = init.world
    val contract             = MonthRandomness.Contract.fromSeed(seed)
    val ledgerFinancialState = init.ledgerFinancialState

    val fiscal = FiscalConstraintEconomics.compute(world, init.banks, ExecutionMonth.First)
    val s1     = FiscalConstraintEconomics.toOutput(fiscal)
    val labor  = LaborEconomics.compute(world, init.firms, init.households, s1)
    val s2Pre  = LaborEconomics.Output(
      labor.wage,
      labor.employed,
      labor.laborDemand,
      labor.wageGrowth,
      labor.operationalHiringSlack,
      labor.immigration,
      labor.netMigration,
      labor.demographics,
      SocialSecurity.ZusState.zero,
      SocialSecurity.NfzState.zero,
      SocialSecurity.PpkState.zero,
      PLN.Zero,
      EarmarkedFunds.State.zero,
      labor.living,
      labor.regionalWages,
    )
    val s3     =
      HouseholdIncomeEconomics.compute(
        world,
        init.firms,
        init.households,
        init.banks,
        s1.lendingBaseRate,
        s1.resWage,
        s2Pre.newWage,
        contract.stages.householdIncomeEconomics.newStream(),
      )
    val s4     = DemandEconomics.compute(DemandEconomics.Input(world, s2Pre.employed, s2Pre.living, s3.domesticCons))
    val s5     =
      FirmEconomics.runStep(world, init.firms, init.households, init.banks, ledgerFinancialState, s1, s2Pre, s3, s4, contract.stages.firmEconomics.newStream())
    val living = s5.ioFirms.filter(Firm.isAlive)
    val s2     = LaborEconomics.reconcilePostFirmStep(world, s1, s2Pre, living, s5.households)
    val s6     = HouseholdFinancialEconomics.compute(world, s1.m, s2.employed, s3.hhAgg, contract.stages.householdFinancialEconomics.newStream())
    val s7     = PriceEquityEconomics.compute(
      PriceEquityEconomics.Input(
        world,
        s1.m,
        s2.newWage,
        s2.employed,
        s2.wageGrowth,
        s3.domesticCons,
        s4.govPurchases,
        s4.avgDemandMult,
        s4.sectorMults,
        init.banks,
        s5,
      ),
      contract.stages.priceEquityEconomics.newStream(),
    )
    val s8     =
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
          init.banks,
          contract.stages.openEconEconomics.newStream(),
        ),
      )
    val s9     =
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
          init.banks,
          contract.stages.bankingEconomics.newStream(),
        ),
      )

    PipelineFixture(world, init.firms, init.households, init.banks, ledgerFinancialState, s1, s2Pre, s2, s3, s4, s5, s6, s7, s8, s9)

  private def baseStepInput: WorldAssemblyEconomics.StepInput =
    WorldAssemblyEconomics.StepInput(
      baseline.world,
      baseline.firms,
      baseline.households,
      baseline.banks,
      baseline.ledgerFinancialState,
      baseline.s1,
      baseline.s2,
      baseline.s3,
      baseline.s4,
      baseline.s5,
      baseline.s6,
      baseline.s7,
      baseline.s8,
      baseline.s9,
    )

  private def multiplierVector(value: Double): Vector[Multiplier] =
    Vector.fill(p.sectorDefs.length)(Multiplier(value))

  private def baseOperationalSignals: OperationalSignals =
    OperationalSignals(
      sectorDemandMult = baseline.s4.sectorMults,
      sectorDemandPressure = baseline.s4.sectorDemandPressure,
      sectorHiringSignal = baseline.s4.sectorHiringSignal,
      operationalHiringSlack = baseline.s2.operationalHiringSlack,
    )

  private def baseFirmComputeInput(world: World, operationalSignals: OperationalSignals, seed: Long): FirmEconomics.Input =
    FirmEconomics.Input(
      w = world,
      firms = baseline.firms,
      households = baseline.households,
      banks = baseline.banks,
      ledgerFinancialState = baseline.ledgerFinancialState,
      month = baseline.s1.m,
      lendingBaseRate = baseline.s1.lendingBaseRate,
      resWage = baseline.s1.resWage,
      baseMinWage = baseline.s1.baseMinWage,
      minWagePriceLevel = baseline.s1.updatedMinWagePriceLevel,
      newWage = baseline.s2Pre.newWage,
      employed = baseline.s2Pre.employed,
      laborDemand = baseline.s2Pre.laborDemand,
      wageGrowth = baseline.s2Pre.wageGrowth,
      immigration = baseline.s2Pre.newImmig,
      netMigration = baseline.s2Pre.netMigration,
      demographics = baseline.s2Pre.newDemographics,
      zusState = baseline.s2Pre.newZus,
      nfzState = baseline.s2Pre.newNfz,
      ppkState = baseline.s2Pre.newPpk,
      rawPpkBondPurchase = baseline.s2Pre.rawPpkBondPurchase,
      earmarked = baseline.s2Pre.newEarmarked,
      living = baseline.s2Pre.living,
      regionalWages = baseline.s2Pre.regionalWages,
      hhOutput = baseline.s3,
      operationalSignals = operationalSignals,
      avgDemandMult = baseline.s4.avgDemandMult,
      sectorCapReal = baseline.s4.sectorCapReal,
      govPurchases = baseline.s4.govPurchases,
      laggedInvestDemand = baseline.s4.laggedInvestDemand,
      fiscalRuleStatus = baseline.s4.fiscalRuleStatus,
      rng = RandomStream.seeded(seed),
    )

  private def baseBankingComputeInput(world: World, operationalSignals: OperationalSignals, seed: Long): BankingEconomics.Input =
    BankingEconomics.Input(
      w = world,
      ledgerFinancialState = baseline.ledgerFinancialState,
      month = baseline.s1.m,
      lendingBaseRate = baseline.s1.lendingBaseRate,
      resWage = baseline.s1.resWage,
      baseMinWage = baseline.s1.baseMinWage,
      minWagePriceLevel = baseline.s1.updatedMinWagePriceLevel,
      employed = baseline.s2.employed,
      newWage = baseline.s2.newWage,
      laborDemand = baseline.s2.laborDemand,
      wageGrowth = baseline.s2.wageGrowth,
      govPurchases = baseline.s4.govPurchases,
      avgDemandMult = baseline.s4.avgDemandMult,
      sectorCapReal = baseline.s4.sectorCapReal,
      laggedInvestDemand = baseline.s4.laggedInvestDemand,
      fiscalRuleStatus = baseline.s4.fiscalRuleStatus,
      laborOutput = baseline.s2,
      operationalSignals = operationalSignals,
      hhOutput = baseline.s3,
      firmOutput = baseline.s5,
      hhFinancialOutput = baseline.s6,
      priceEquityOutput = baseline.s7,
      openEconOutput = baseline.s8,
      banks = baseline.banks,
      depositRng = RandomStream.seeded(seed),
    )

  private def baseComputeInput(world: World): WorldAssemblyEconomics.Input =
    WorldAssemblyEconomics.Input(
      w = world,
      firms = baseline.firms,
      households = baseline.households,
      banks = baseline.banks,
      ledgerFinancialState = baseline.ledgerFinancialState,
      month = baseline.s1.m,
      lendingBaseRate = baseline.s1.lendingBaseRate,
      resWage = baseline.s1.resWage,
      baseMinWage = baseline.s1.baseMinWage,
      minWagePriceLevel = baseline.s1.updatedMinWagePriceLevel,
      govPurchases = baseline.s4.govPurchases,
      sectorMults = baseline.s4.sectorMults,
      sectorDemandPressure = baseline.s4.sectorDemandPressure,
      sectorHiringSignal = baseline.s4.sectorHiringSignal,
      avgDemandMult = baseline.s4.avgDemandMult,
      sectorCapReal = baseline.s4.sectorCapReal,
      laggedInvestDemand = baseline.s4.laggedInvestDemand,
      fiscalRuleStatus = baseline.s4.fiscalRuleStatus,
      laborOutput = baseline.s2,
      hhOutput = baseline.s3,
      firmOutput = baseline.s5,
      hhFinancialOutput = baseline.s6,
      priceEquityOutput = baseline.s7,
      openEconOutput = baseline.s8,
      bankOutput = baseline.s9,
      randomness = MonthRandomness.Contract.fromSeed(42L).assembly.newStreams(),
    )

  private def assemblyRandomness(seed: Long): MonthRandomness.AssemblyStreams =
    MonthRandomness.Contract.fromSeed(seed).assembly.newStreams()

  private def allEmployed(
      households: Vector[Household.State],
      firms: Vector[Firm.State],
      wage: PLN,
  ): Vector[Household.State] =
    val employer = firms.find(Firm.isAlive).getOrElse(fail("expected at least one living firm"))
    households.map(_.copy(status = HhStatus.Employed(employer.id, employer.sector, wage)))

  private def withUnemploymentShare(
      households: Vector[Household.State],
      firms: Vector[Firm.State],
      wage: PLN,
      unemploymentShare: Double,
  ): Vector[Household.State] =
    val employedBase    = allEmployed(households, firms, wage)
    val boundedShare    = Math.max(0.0, Math.min(1.0, unemploymentShare))
    val unemployedCount = Math.round(employedBase.length * boundedShare).toInt
    employedBase.zipWithIndex.map: (hh, idx) =>
      if idx < unemployedCount then hh.copy(status = HhStatus.Unemployed(1))
      else hh

  private def withSeedSignals(world: World, f: DecisionSignals => DecisionSignals): World =
    world.copy(pipeline = world.pipeline.withDecisionSignals(f(world.seedIn)))

  private def entrySensitiveInput: WorldAssemblyEconomics.StepInput =
    val base = baseStepInput
    base.copy(
      w = withSeedSignals(
        base.w,
        _.copy(
          unemploymentRate = Share(0.15),
          inflation = Rate(0.03),
          expectedInflation = Rate(0.025),
          laggedHiringSlack = Share.One,
          startupAbsorptionRate = Share.One,
        ),
      ),
      s2 = base.s2.copy(operationalHiringSlack = Share.One),
      s7 = base.s7.copy(newInfl = Rate(0.03)),
      s8 = base.s8.copy(
        monetary = base.s8.monetary.copy(
          newExp = base.s8.monetary.newExp.copy(expectedInflation = Rate(0.025)),
        ),
      ),
      s9 = base.s9.copy(
        reassignedHouseholds = withUnemploymentShare(base.s9.reassignedHouseholds, base.s9.reassignedFirms, base.s2.newWage, 0.15),
      ),
    )

  private def netBirths(in: WorldAssemblyEconomics.StepInput): Int =
    WorldAssemblyEconomics.runStep(in, assemblyRandomness(1234L)).newWorld.flows.netFirmBirths

  "DemandEconomics.compute" should "smooth sector hiring plans from lagged decision signals while keeping same-month pressure fixed" in {
    val weakLagged   = withSeedSignals(
      baseline.world,
      _.copy(sectorHiringSignal = multiplierVector(0.40)),
    )
    val strongLagged = withSeedSignals(
      baseline.world,
      _.copy(sectorHiringSignal = multiplierVector(1.60)),
    )
    val weakResult   = DemandEconomics.compute(
      DemandEconomics.Input(weakLagged, baseline.s2Pre.employed, baseline.s2Pre.living, baseline.s3.domesticCons),
    )
    val strongResult = DemandEconomics.compute(
      DemandEconomics.Input(strongLagged, baseline.s2Pre.employed, baseline.s2Pre.living, baseline.s3.domesticCons),
    )

    weakResult.sectorDemandPressure shouldBe strongResult.sectorDemandPressure
    weakResult.sectorMults shouldBe strongResult.sectorMults
    weakResult.avgDemandMult shouldBe strongResult.avgDemandMult

    val currentPressure = weakResult.sectorDemandPressure.head
    weakResult.sectorHiringSignal.head shouldBe Multiplier(0.40) * Share(0.65) + currentPressure * Share(0.35)
    strongResult.sectorHiringSignal.head shouldBe Multiplier(1.60) * Share(0.65) + currentPressure * Share(0.35)
    strongResult.sectorHiringSignal.head should be > weakResult.sectorHiringSignal.head
  }

  "SignalExtraction.compute" should "derive next-month decision inputs through one explicit post-to-pre boundary" in {
    val base            = entrySensitiveInput
    val finalHouseholds = withUnemploymentShare(base.s9.reassignedHouseholds, base.s9.reassignedFirms, base.s2.newWage, 0.22)
    val finalWorld      = base.w.copy(
      social = base.w.social.copy(demographics = base.s2.newDemographics),
      inflation = Rate(-0.01),
      mechanisms = base.w.mechanisms.copy(
        expectations = base.w.mechanisms.expectations.copy(expectedInflation = Rate(0.04)),
      ),
    )
    val extracted       = SignalExtraction.compute(
      SignalExtraction.Input(
        labor = SignalExtraction.LaborOutcomes(
          unemploymentRate = finalWorld.unemploymentRate(finalHouseholds.count(_.status.isInstanceOf[HhStatus.Employed])),
          laggedHiringSlack = Share.One,
          startupAbsorptionRate = Share(0.35),
        ),
        nominal = SignalExtraction.NominalOutcomes(
          inflation = finalWorld.inflation,
          expectedInflation = finalWorld.mechanisms.expectations.expectedInflation,
        ),
        demand = SignalExtraction.DemandOutcomes(
          sectorDemandMult = base.s4.sectorMults,
          sectorDemandPressure = base.s4.sectorDemandPressure,
          sectorHiringSignal = base.s4.sectorHiringSignal,
        ),
      ),
    )

    extracted.seedOut.unemploymentRate shouldBe finalWorld.unemploymentRate(finalHouseholds.count(_.status.isInstanceOf[HhStatus.Employed]))
    extracted.seedOut.inflation shouldBe Rate(-0.01)
    extracted.seedOut.expectedInflation shouldBe Rate(0.04)
    extracted.seedOut.laggedHiringSlack shouldBe Share.One
    extracted.seedOut.startupAbsorptionRate shouldBe Share(0.35)
    extracted.seedOut.sectorDemandMult shouldBe base.s4.sectorMults
    extracted.seedOut.sectorDemandPressure shouldBe base.s4.sectorDemandPressure
    extracted.seedOut.sectorHiringSignal shouldBe base.s4.sectorHiringSignal
    extracted.provenance.unemploymentRate.stage shouldBe MonthTraceStage.WorldAssemblyEconomics
    extracted.provenance.startupAbsorptionRate.stage shouldBe MonthTraceStage.StartupStaffing
  }

  "FirmEconomics.compute" should "ignore stale persisted demand signals when explicit OperationalSignals define the same-month surface" in {
    val staleWorld       = withSeedSignals(
      baseline.world,
      _.copy(
        sectorDemandMult = multiplierVector(0.35),
        sectorDemandPressure = multiplierVector(0.35),
        sectorHiringSignal = multiplierVector(0.35),
      ),
    )
    val explicitResult   = FirmEconomics.compute(baseFirmComputeInput(staleWorld, baseOperationalSignals, seed = 9001L))
    val freshWorldResult = FirmEconomics.compute(baseFirmComputeInput(baseline.world, baseOperationalSignals, seed = 9001L))
    val bridgedResult    = FirmEconomics.compute(
      baseFirmComputeInput(staleWorld, OperationalSignals.fromBridgedWorld(staleWorld), seed = 9001L),
    )

    explicitResult.sumNewLoans shouldBe freshWorldResult.sumNewLoans
    explicitResult.sumGrossInvestment shouldBe freshWorldResult.sumGrossInvestment
    explicitResult.sumInventoryChange shouldBe freshWorldResult.sumInventoryChange
    explicitResult.perBankWorkers shouldBe freshWorldResult.perBankWorkers

    explicitResult.sumNewLoans should not be bridgedResult.sumNewLoans
    explicitResult.perBankWorkers should not be bridgedResult.perBankWorkers
  }

  "BankingEconomics.compute" should "remain insensitive to stale persisted demand signals when explicit OperationalSignals are supplied" in {
    val staleWorld       = withSeedSignals(
      baseline.world,
      _.copy(
        sectorDemandMult = multiplierVector(0.35),
        sectorDemandPressure = multiplierVector(0.35),
        sectorHiringSignal = multiplierVector(0.35),
      ),
    )
    val explicitResult   = BankingEconomics.compute(baseBankingComputeInput(staleWorld, baseOperationalSignals, seed = 777L))
    val freshWorldResult = BankingEconomics.compute(baseBankingComputeInput(baseline.world, baseOperationalSignals, seed = 777L))

    explicitResult shouldBe freshWorldResult
  }

  "WorldAssemblyEconomics.runStep" should "derive entry unemployment from lagged decision signals instead of post-firm households" in {
    val base       = entrySensitiveInput
    val lowUnemp   = base.copy(
      s9 = base.s9.copy(
        reassignedHouseholds = withUnemploymentShare(base.s9.reassignedHouseholds, base.s9.reassignedFirms, base.s2.newWage, 0.04),
      ),
    )
    val highUnemp  = base.copy(
      s9 = base.s9.copy(
        reassignedHouseholds = withUnemploymentShare(base.s9.reassignedHouseholds, base.s9.reassignedFirms, base.s2.newWage, 0.15),
      ),
    )
    val lowLagged  = base.copy(
      w = withSeedSignals(base.w, _.copy(unemploymentRate = Share(0.04))),
    )
    val highLagged = base.copy(
      w = withSeedSignals(base.w, _.copy(unemploymentRate = Share(0.15))),
    )

    netBirths(highUnemp) shouldBe netBirths(lowUnemp)
    netBirths(highLagged) should be > netBirths(lowLagged)
  }

  it should "ignore assembled month-t inflation when entry uses lagged nominal signals" in {
    val base           = entrySensitiveInput
    val deflation      = base.copy(s7 = base.s7.copy(newInfl = Rate(-0.02)))
    val positive       = base.copy(s7 = base.s7.copy(newInfl = Rate(0.03)))
    val negativeLagged = base.copy(w = withSeedSignals(base.w, _.copy(inflation = Rate(-0.02))))
    val positiveLagged = base.copy(w = withSeedSignals(base.w, _.copy(inflation = Rate(0.03))))

    netBirths(positive) shouldBe netBirths(deflation)
    netBirths(positiveLagged) should be > netBirths(negativeLagged)
  }

  it should "ignore assembled month-t expected inflation when entry uses lagged nominal signals" in {
    val base           = entrySensitiveInput
    val negativeExp    = base.copy(
      s8 = base.s8.copy(
        monetary = base.s8.monetary.copy(
          newExp = base.s8.monetary.newExp.copy(expectedInflation = Rate(-0.01)),
        ),
      ),
    )
    val positiveExp    = base.copy(
      s8 = base.s8.copy(
        monetary = base.s8.monetary.copy(
          newExp = base.s8.monetary.newExp.copy(expectedInflation = Rate(0.025)),
        ),
      ),
    )
    val laggedNegative = base.copy(
      w = withSeedSignals(base.w, _.copy(expectedInflation = Rate(-0.01))),
    )
    val laggedPositive = base.copy(
      w = withSeedSignals(base.w, _.copy(expectedInflation = Rate(0.025))),
    )

    netBirths(positiveExp) shouldBe netBirths(negativeExp)
    netBirths(laggedPositive) should be > netBirths(laggedNegative)
  }

  it should "derive entry labor tightness from lagged decision signals instead of refreshed same-month slack" in {
    val base        = entrySensitiveInput
    val tight       = base.copy(s2 = base.s2.copy(operationalHiringSlack = Share(0.10)))
    val loose       = base.copy(s2 = base.s2.copy(operationalHiringSlack = Share.One))
    val tightLagged = base.copy(w = withSeedSignals(base.w, _.copy(laggedHiringSlack = Share(0.10))))
    val looseLagged = base.copy(w = withSeedSignals(base.w, _.copy(laggedHiringSlack = Share.One)))

    netBirths(loose) shouldBe netBirths(tight)
    netBirths(looseLagged) should be > netBirths(tightLagged)
  }

  it should "source startup absorption from lagged decision signals" in {
    val base   = entrySensitiveInput
    val weak   = base.copy(w = base.w.copy(pipeline = base.w.pipeline.copy(startupAbsorptionRate = Share(0.10))))
    val strong = base.copy(w = base.w.copy(pipeline = base.w.pipeline.copy(startupAbsorptionRate = Share.One)))

    netBirths(strong) should be > netBirths(weak)
  }

  it should "persist extracted same-month hiring slack into next-month decision signals" in {
    val input  = entrySensitiveInput
    val base   = input.copy(s2 = input.s2.copy(operationalHiringSlack = Share(0.21)))
    val result = WorldAssemblyEconomics.runStep(base, assemblyRandomness(1234L))

    result.newWorld.pipeline.operationalHiringSlack shouldBe Share(0.21)
    result.newWorld.pipeline.laggedHiringSlack shouldBe Share(0.21)
    result.newWorld.seedIn.laggedHiringSlack shouldBe Share(0.21)
  }

  it should "keep post-month assembly distinct from the next-month seed boundary" in {
    val input = entrySensitiveInput.copy(s2 = entrySensitiveInput.s2.copy(operationalHiringSlack = Share(0.21)))
    val post  = WorldAssemblyEconomics.runPostStep(input, assemblyRandomness(1234L))

    post.world.pipeline.operationalHiringSlack shouldBe Share(0.21)
    post.world.seedIn shouldBe input.w.seedIn
    post.world.pipeline.sectorDemandMult shouldBe input.w.pipeline.sectorDemandMult
    post.world.pipeline.sectorDemandPressure shouldBe input.w.pipeline.sectorDemandPressure
    post.world.pipeline.sectorHiringSignal shouldBe input.w.pipeline.sectorHiringSignal
  }

  it should "keep explicit OperationalSignals aligned with post-reconcile labor slack" in {
    baseOperationalSignals.operationalHiringSlack shouldBe baseline.s2.operationalHiringSlack
  }

  "WorldAssemblyEconomics.compute" should "persist demand and hiring signals from current demand output in the bridge path" in {
    val stalePressure = Vector.fill(p.sectorDefs.length)(Multiplier(0.33))
    val staleHiring   = Vector.fill(p.sectorDefs.length)(Multiplier(1.77))
    val staleWorld    = baseline.world.copy(
      pipeline = baseline.world.pipeline.copy(
        sectorDemandPressure = stalePressure,
        sectorHiringSignal = staleHiring,
      ),
    )

    val assembled = WorldAssemblyEconomics.compute(baseComputeInput(staleWorld))

    assembled.world.pipeline.sectorDemandPressure shouldBe baseline.s4.sectorDemandPressure
    assembled.world.pipeline.sectorHiringSignal shouldBe baseline.s4.sectorHiringSignal
    assembled.world.pipeline.sectorDemandPressure should not be stalePressure
    assembled.world.pipeline.sectorHiringSignal should not be staleHiring
  }
