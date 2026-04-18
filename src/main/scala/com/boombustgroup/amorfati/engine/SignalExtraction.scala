package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.agents.Household
import com.boombustgroup.amorfati.types.*

/** Provenance stage tags for extracted next-month signals. */
enum MonthTraceStage:
  case LaborEconomics
  case DemandEconomics
  case PriceEquityEconomics
  case OpenEconEconomics
  case WorldAssemblyEconomics
  case StartupStaffing

/** Typed provenance for one extracted next-month signal. */
case class SignalProvenance[A](
    value: A,
    stage: MonthTraceStage,
    source: String,
)

/** Minimal provenance view for the fields persisted into the next month. */
case class SeedOutProvenance(
    unemploymentRate: SignalProvenance[Share],
    inflation: SignalProvenance[Rate],
    expectedInflation: SignalProvenance[Rate],
    laggedHiringSlack: SignalProvenance[Share],
    startupAbsorptionRate: SignalProvenance[Share],
    sectorDemandMult: SignalProvenance[Vector[Multiplier]],
    sectorDemandPressure: SignalProvenance[Vector[Multiplier]],
    sectorHiringSignal: SignalProvenance[Vector[Multiplier]],
)

/** Explicit post-to-pre transition boundary for monthly execution.
  *
  * This component owns the derivation of next-month [[DecisionSignals]] from
  * realized month-`t` outcomes. World assembly may collect the realized inputs,
  * but it should not own the extraction semantics itself.
  */
object SignalExtraction:

  /** Realized labor outcomes that feed the next month's decision surface. */
  case class LaborOutcomes(
      unemploymentRate: Share,
      laggedHiringSlack: Share,
      startupAbsorptionRate: Share,
  )

  /** Realized nominal outcomes that feed the next month's decision surface. */
  case class NominalOutcomes(
      inflation: Rate,
      expectedInflation: Rate,
  )

  /** Same-month demand signals persisted into the next month's seed. */
  case class DemandOutcomes(
      sectorDemandMult: Vector[Multiplier],
      sectorDemandPressure: Vector[Multiplier],
      sectorHiringSignal: Vector[Multiplier],
  )

  /** Minimal typed input required to derive next-month decision signals. */
  case class Input(
      labor: LaborOutcomes,
      nominal: NominalOutcomes,
      demand: DemandOutcomes,
  )

  /** Canonical builder used by both world assembly and typed month-boundary
    * code so seed extraction always reads the same realized fields.
    */
  private def inputFromRealizedOutcomes(
      unemploymentRate: Share,
      laggedHiringSlack: Share,
      startupAbsorptionRate: Share,
      inflation: Rate,
      expectedInflation: Rate,
      sectorDemandMult: Vector[Multiplier],
      sectorDemandPressure: Vector[Multiplier],
      sectorHiringSignal: Vector[Multiplier],
  ): Input =
    Input(
      labor = LaborOutcomes(
        unemploymentRate = unemploymentRate,
        laggedHiringSlack = laggedHiringSlack,
        startupAbsorptionRate = startupAbsorptionRate,
      ),
      nominal = NominalOutcomes(
        inflation = inflation,
        expectedInflation = expectedInflation,
      ),
      demand = DemandOutcomes(
        sectorDemandMult = sectorDemandMult,
        sectorDemandPressure = sectorDemandPressure,
        sectorHiringSignal = sectorHiringSignal,
      ),
    )

  /** Canonical extraction from the assembled post-month state.
    *
    * This is the single bridge from realized month-`t` stock/flow outcomes to
    * the next month's decision seed. Callers that need next-boundary signals
    * should use this instead of rebuilding unemployment and demand inputs.
    */
  def fromPostMonth(
      world: World,
      households: Vector[Household.State],
      operationalHiringSlack: Share,
      startupAbsorptionRate: Share,
      demand: DemandOutcomes,
  ): Output =
    val employed = Household.countEmployed(households)
    compute(
      inputFromRealizedOutcomes(
        unemploymentRate = world.unemploymentRate(employed),
        laggedHiringSlack = operationalHiringSlack,
        startupAbsorptionRate = startupAbsorptionRate,
        inflation = world.inflation,
        expectedInflation = world.mechanisms.expectations.expectedInflation,
        sectorDemandMult = demand.sectorDemandMult,
        sectorDemandPressure = demand.sectorDemandPressure,
        sectorHiringSignal = demand.sectorHiringSignal,
      ),
    )

  /** Extraction result: persisted seed plus typed provenance for audit/trace.
    */
  case class Output(
      seedOut: DecisionSignals,
      provenance: SeedOutProvenance,
  )

  def compute(in: Input): Output =
    val seedOut = DecisionSignals(
      unemploymentRate = in.labor.unemploymentRate,
      inflation = in.nominal.inflation,
      expectedInflation = in.nominal.expectedInflation,
      laggedHiringSlack = in.labor.laggedHiringSlack,
      startupAbsorptionRate = in.labor.startupAbsorptionRate,
      sectorDemandMult = in.demand.sectorDemandMult,
      sectorDemandPressure = in.demand.sectorDemandPressure,
      sectorHiringSignal = in.demand.sectorHiringSignal,
    )

    Output(
      seedOut = seedOut,
      provenance = SeedOutProvenance(
        unemploymentRate = SignalProvenance(
          seedOut.unemploymentRate,
          MonthTraceStage.WorldAssemblyEconomics,
          "finalHouseholds employment count -> assembledWorld.unemploymentRate",
        ),
        inflation = SignalProvenance(
          seedOut.inflation,
          MonthTraceStage.PriceEquityEconomics,
          "s7.newInfl -> assembledWorld.inflation",
        ),
        expectedInflation = SignalProvenance(
          seedOut.expectedInflation,
          MonthTraceStage.OpenEconEconomics,
          "s8.monetary.newExp.expectedInflation -> assembledWorld.mechanisms.expectations.expectedInflation",
        ),
        laggedHiringSlack = SignalProvenance(
          seedOut.laggedHiringSlack,
          MonthTraceStage.LaborEconomics,
          "s2.operationalHiringSlack",
        ),
        startupAbsorptionRate = SignalProvenance(
          seedOut.startupAbsorptionRate,
          MonthTraceStage.StartupStaffing,
          "WorldAssemblyEconomics.applyStartupStaffing.startupAbsorptionRate",
        ),
        sectorDemandMult = SignalProvenance(
          seedOut.sectorDemandMult,
          MonthTraceStage.DemandEconomics,
          "s4.sectorMults",
        ),
        sectorDemandPressure = SignalProvenance(
          seedOut.sectorDemandPressure,
          MonthTraceStage.DemandEconomics,
          "s4.sectorDemandPressure",
        ),
        sectorHiringSignal = SignalProvenance(
          seedOut.sectorHiringSignal,
          MonthTraceStage.DemandEconomics,
          "s4.sectorHiringSignal",
        ),
      ),
    )
