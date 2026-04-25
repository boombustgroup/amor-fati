package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.compiletime.testing.typeCheckErrors

class MonthSemanticsSpec extends AnyFlatSpec with Matchers:

  "MonthSemantics" should "preserve typed wrappers for pre, same-month, and next-pre boundaries" in {
    val signals = DecisionSignals(
      unemploymentRate = Share.decimal(8, 2),
      inflation = Rate.decimal(3, 2),
      expectedInflation = Rate.decimal(25, 3),
      laggedHiringSlack = Share.decimal(42, 2),
      startupAbsorptionRate = Share.decimal(65, 2),
      sectorDemandMult = Vector(Multiplier.decimal(9, 1), Multiplier.decimal(11, 1)),
      sectorDemandPressure = Vector(Multiplier.decimal(8, 1), Multiplier.decimal(12, 1)),
      sectorHiringSignal = Vector(Multiplier.decimal(85, 2), Multiplier.decimal(115, 2)),
    )

    val seedIn: MonthSemantics.SeedIn =
      MonthSemantics.seedIn(signals)

    val operationalRaw                          = OperationalSignals(
      sectorDemandMult = signals.sectorDemandMult,
      sectorDemandPressure = signals.sectorDemandPressure,
      sectorHiringSignal = signals.sectorHiringSignal,
      operationalHiringSlack = Share.decimal(37, 2),
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
        unemploymentRate = Share.decimal(8, 2),
        inflation = Rate.decimal(3, 2),
        expectedInflation = Rate.decimal(25, 3),
        laggedHiringSlack = Share.decimal(42, 2),
        startupAbsorptionRate = Share.decimal(65, 2),
        sectorDemandMult = Vector(Multiplier.decimal(9, 1), Multiplier.decimal(11, 1)),
        sectorDemandPressure = Vector(Multiplier.decimal(8, 1), Multiplier.decimal(12, 1)),
        sectorHiringSignal = Vector(Multiplier.decimal(85, 2), Multiplier.decimal(115, 2)),
      )

      MonthSemantics.At[DecisionSignals, MonthSemantics.Pre](signals)
    """) should not be empty

    typeCheckErrors("""
      import com.boombustgroup.amorfati.engine.*
      import com.boombustgroup.amorfati.types.*

      val signals = DecisionSignals(
        unemploymentRate = Share.decimal(8, 2),
        inflation = Rate.decimal(3, 2),
        expectedInflation = Rate.decimal(25, 3),
        laggedHiringSlack = Share.decimal(42, 2),
        startupAbsorptionRate = Share.decimal(65, 2),
        sectorDemandMult = Vector(Multiplier.decimal(9, 1), Multiplier.decimal(11, 1)),
        sectorDemandPressure = Vector(Multiplier.decimal(8, 1), Multiplier.decimal(12, 1)),
        sectorHiringSignal = Vector(Multiplier.decimal(85, 2), Multiplier.decimal(115, 2)),
      )

      MonthSemantics.seedIn(signals).value
    """) should not be empty

    typeCheckErrors("""
      import com.boombustgroup.amorfati.engine.*
      import com.boombustgroup.amorfati.types.*

      val signals = DecisionSignals(
        unemploymentRate = Share.decimal(8, 2),
        inflation = Rate.decimal(3, 2),
        expectedInflation = Rate.decimal(25, 3),
        laggedHiringSlack = Share.decimal(42, 2),
        startupAbsorptionRate = Share.decimal(65, 2),
        sectorDemandMult = Vector(Multiplier.decimal(9, 1), Multiplier.decimal(11, 1)),
        sectorDemandPressure = Vector(Multiplier.decimal(8, 1), Multiplier.decimal(12, 1)),
        sectorHiringSignal = Vector(Multiplier.decimal(85, 2), Multiplier.decimal(115, 2)),
      )

      MonthSemantics.seedOut(signals)
    """) should not be empty
  }
