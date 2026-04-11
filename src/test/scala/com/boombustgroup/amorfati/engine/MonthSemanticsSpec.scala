package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.compiletime.testing.typeCheckErrors

class MonthSemanticsSpec extends AnyFlatSpec with Matchers:

  "MonthSemantics" should "preserve typed wrappers for pre, same-month, and next-pre boundaries" in {
    val signals = DecisionSignals(
      unemploymentRate = Share(0.08),
      inflation = Rate(0.03),
      expectedInflation = Rate(0.025),
      laggedHiringSlack = Share(0.42),
      startupAbsorptionRate = Share(0.65),
      sectorDemandMult = Vector(Multiplier(0.9), Multiplier(1.1)),
      sectorDemandPressure = Vector(Multiplier(0.8), Multiplier(1.2)),
      sectorHiringSignal = Vector(Multiplier(0.85), Multiplier(1.15)),
    )

    val seedIn: MonthSemantics.SeedIn =
      MonthSemantics.seedIn(signals)

    val operationalRaw                          = OperationalSignals(
      sectorDemandMult = signals.sectorDemandMult,
      sectorDemandPressure = signals.sectorDemandPressure,
      sectorHiringSignal = signals.sectorHiringSignal,
      operationalHiringSlack = Share(0.37),
    )
    val operational: MonthSemantics.Operational =
      MonthSemantics.operational(operationalRaw)

    val extracted                       = SignalExtraction.compute(
      SignalExtraction.Input(
        labor = SignalExtraction.LaborOutcomes(
          unemploymentRate = signals.unemploymentRate,
          laggedHiringSlack = operationalRaw.operationalHiringSlack,
          startupAbsorptionRate = signals.startupAbsorptionRate,
        ),
        nominal = SignalExtraction.NominalOutcomes(
          inflation = signals.inflation,
          expectedInflation = signals.expectedInflation,
        ),
        demand = SignalExtraction.DemandOutcomes(
          sectorDemandMult = signals.sectorDemandMult,
          sectorDemandPressure = signals.sectorDemandPressure,
          sectorHiringSignal = signals.sectorHiringSignal,
        ),
      ),
    )
    val seedOut: MonthSemantics.SeedOut =
      MonthSemantics.seedOut(extracted)

    seedIn.decisionSignals shouldBe signals
    operational.operationalSignals shouldBe operationalRaw
    seedOut.signalExtraction shouldBe extracted
    seedOut.nextSeed.laggedHiringSlack shouldBe operationalRaw.operationalHiringSlack
  }

  it should "hide generic tagging and unwrap helpers behind alias-specific transitions" in {
    typeCheckErrors("""
      import com.boombustgroup.amorfati.engine.*
      import com.boombustgroup.amorfati.types.*

      val signals = DecisionSignals(
        unemploymentRate = Share(0.08),
        inflation = Rate(0.03),
        expectedInflation = Rate(0.025),
        laggedHiringSlack = Share(0.42),
        startupAbsorptionRate = Share(0.65),
        sectorDemandMult = Vector(Multiplier(0.9), Multiplier(1.1)),
        sectorDemandPressure = Vector(Multiplier(0.8), Multiplier(1.2)),
        sectorHiringSignal = Vector(Multiplier(0.85), Multiplier(1.15)),
      )

      MonthSemantics.At[DecisionSignals, MonthSemantics.Pre](signals)
    """) should not be empty

    typeCheckErrors("""
      import com.boombustgroup.amorfati.engine.*
      import com.boombustgroup.amorfati.types.*

      val signals = DecisionSignals(
        unemploymentRate = Share(0.08),
        inflation = Rate(0.03),
        expectedInflation = Rate(0.025),
        laggedHiringSlack = Share(0.42),
        startupAbsorptionRate = Share(0.65),
        sectorDemandMult = Vector(Multiplier(0.9), Multiplier(1.1)),
        sectorDemandPressure = Vector(Multiplier(0.8), Multiplier(1.2)),
        sectorHiringSignal = Vector(Multiplier(0.85), Multiplier(1.15)),
      )

      MonthSemantics.seedIn(signals).value
    """) should not be empty

    typeCheckErrors("""
      import com.boombustgroup.amorfati.engine.*
      import com.boombustgroup.amorfati.types.*

      val signals = DecisionSignals(
        unemploymentRate = Share(0.08),
        inflation = Rate(0.03),
        expectedInflation = Rate(0.025),
        laggedHiringSlack = Share(0.42),
        startupAbsorptionRate = Share(0.65),
        sectorDemandMult = Vector(Multiplier(0.9), Multiplier(1.1)),
        sectorDemandPressure = Vector(Multiplier(0.8), Multiplier(1.2)),
        sectorHiringSignal = Vector(Multiplier(0.85), Multiplier(1.15)),
      )

      MonthSemantics.seedOut(signals)
    """) should not be empty
  }
