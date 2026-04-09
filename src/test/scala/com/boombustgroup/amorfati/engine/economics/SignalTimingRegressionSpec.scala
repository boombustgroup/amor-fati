package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.{DecisionSignals, World}
import com.boombustgroup.amorfati.init.WorldInit
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
      s1: FiscalConstraintEconomics.Output,
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
    val init  = WorldInit.initialize(seed)
    val world = init.world
    val rng   = new scala.util.Random(seed)

    val fiscal = FiscalConstraintEconomics.compute(world, init.banks)
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
    val s3     = HouseholdIncomeEconomics.compute(world, init.firms, init.households, init.banks, s1.lendingBaseRate, s1.resWage, s2Pre.newWage, rng)
    val s4     = DemandEconomics.compute(DemandEconomics.Input(world, s2Pre.employed, s2Pre.living, s3.domesticCons))
    val s5     = FirmEconomics.runStep(world, init.firms, init.households, init.banks, s1, s2Pre, s3, s4, rng)
    val living = s5.ioFirms.filter(Firm.isAlive)
    val s2     = LaborEconomics.reconcilePostFirmStep(world, s1, s2Pre, living, s5.households)
    val s6     = HouseholdFinancialEconomics.compute(world, s1.m, s2.employed, s3.hhAgg, rng)
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
      rng,
    )
    val s8     = OpenEconEconomics.runStep(OpenEconEconomics.StepInput(world, s1, s2, s3, s4, s5, s6, s7, init.banks, rng))
    val s9     = BankingEconomics.runStep(BankingEconomics.StepInput(world, s1, s2, s3, s4, s5, s6, s7, s8, init.banks, rng))

    PipelineFixture(world, init.firms, init.households, init.banks, s1, s2, s3, s4, s5, s6, s7, s8, s9)

  private def baseStepInput: WorldAssemblyEconomics.StepInput =
    WorldAssemblyEconomics.StepInput(
      baseline.world,
      baseline.firms,
      baseline.households,
      baseline.banks,
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

  private def baseComputeInput(world: World): WorldAssemblyEconomics.Input =
    WorldAssemblyEconomics.Input(
      w = world,
      firms = baseline.firms,
      households = baseline.households,
      banks = baseline.banks,
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
      rng = new scala.util.Random(42L),
      migRng = new scala.util.Random(4242L),
    )

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
    WorldAssemblyEconomics.runStep(in, new scala.util.Random(1234L), new scala.util.Random(5678L)).newWorld.flows.netFirmBirths

  "WorldAssemblyEconomics.extractSignals" should "derive next-month decision inputs through one explicit post-to-pre boundary" in {
    val base            = entrySensitiveInput
    val finalHouseholds = withUnemploymentShare(base.s9.reassignedHouseholds, base.s9.reassignedFirms, base.s2.newWage, 0.22)
    val finalWorld      = base.w.copy(
      social = base.w.social.copy(demographics = base.s2.newDemographics),
      inflation = Rate(-0.01),
      mechanisms = base.w.mechanisms.copy(
        expectations = base.w.mechanisms.expectations.copy(expectedInflation = Rate(0.04)),
      ),
    )
    val extracted       = WorldAssemblyEconomics.extractSignals(base, finalWorld, finalHouseholds, 0.35)

    extracted.unemploymentRate shouldBe finalWorld.unemploymentRate(finalHouseholds.count(_.status.isInstanceOf[HhStatus.Employed]))
    extracted.inflation shouldBe Rate(-0.01)
    extracted.expectedInflation shouldBe Rate(0.04)
    extracted.laggedHiringSlack shouldBe Share.One
    extracted.startupAbsorptionRate shouldBe Share(0.35)
    extracted.sectorDemandMult shouldBe base.s4.sectorMults
    extracted.sectorDemandPressure shouldBe base.s4.sectorDemandPressure
    extracted.sectorHiringSignal shouldBe base.s4.sectorHiringSignal
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
    val weak   = base.copy(w = base.w.copy(pipeline = base.w.pipeline.copy(startupAbsorptionRate = 0.10)))
    val strong = base.copy(w = base.w.copy(pipeline = base.w.pipeline.copy(startupAbsorptionRate = 1.0)))

    netBirths(strong) should be > netBirths(weak)
  }

  it should "persist extracted same-month hiring slack into next-month decision signals" in {
    val input  = entrySensitiveInput
    val base   = input.copy(s2 = input.s2.copy(operationalHiringSlack = Share(0.21)))
    val result = WorldAssemblyEconomics.runStep(base, new scala.util.Random(1234L), new scala.util.Random(5678L))

    result.newWorld.pipeline.operationalHiringSlack shouldBe Share(0.21)
    result.newWorld.pipeline.laggedHiringSlack shouldBe Share(0.21)
    result.newWorld.seedIn.laggedHiringSlack shouldBe Share(0.21)
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
