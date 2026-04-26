package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.types.{Multiplier, Rate, Share}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MonthTraceSpec extends AnyFlatSpec with Matchers:

  private val sectorDemandMult     = Vector(Multiplier.decimal(11, 1), Multiplier.decimal(9, 1))
  private val sectorDemandPressure = Vector(Multiplier.decimal(12, 1), Multiplier.decimal(8, 1))
  private val sectorHiringSignal   = Vector(Multiplier.decimal(105, 2), Multiplier.decimal(95, 2))

  "MonthTimingTrace" should "reject duplicate envelope keys" in {
    val err = intercept[IllegalArgumentException]:
      MonthTimingTrace(
        Vector(
          MonthTimingEnvelope(MonthTimingEnvelopeKey.Labor, MonthTimingPayload.LaborSignals(Share.decimal(4, 1))),
          MonthTimingEnvelope(MonthTimingEnvelopeKey.Labor, MonthTimingPayload.LaborSignals(Share.decimal(6, 1))),
        ),
      )

    err.getMessage should include("unique envelope keys")
    err.getMessage should include(MonthTimingEnvelopeKey.Labor.toString)
  }

  it should "fail clearly when a required payload is missing" in {
    val err = intercept[IllegalStateException]:
      MonthTimingTrace(Vector.empty).laborSignals

    err.getMessage should include("Labor")
    err.getMessage should include(MonthTimingEnvelopeKey.Labor.toString)
  }

  it should "build typed envelopes from timing inputs in a stable order" in {
    val trace = MonthTimingTrace.fromInputs(
      MonthTimingInputs(
        labor = MonthTimingPayload.LaborSignals(Share.decimal(4, 1)),
        demand = MonthTimingPayload.DemandSignals(
          sectorDemandMult = sectorDemandMult,
          sectorDemandPressure = sectorDemandPressure,
          sectorHiringSignal = sectorHiringSignal,
        ),
        nominal = MonthTimingPayload.NominalSignals(
          realizedInflation = Rate.decimal(3, 2),
          expectedInflation = Rate.decimal(25, 3),
        ),
        firmDynamics = MonthTimingPayload.FirmDynamics(
          startupAbsorptionRate = Share.decimal(7, 1),
          firmBirths = 12,
          firmDeaths = 4,
          netFirmBirths = 8,
        ),
      ),
    )

    trace.envelopes.map(_.key) shouldBe Vector(
      MonthTimingEnvelopeKey.Labor,
      MonthTimingEnvelopeKey.Demand,
      MonthTimingEnvelopeKey.Nominal,
      MonthTimingEnvelopeKey.Firm,
    )
    trace.laborSignals.operationalHiringSlack shouldBe Share.decimal(4, 1)
    trace.demandSignals.sectorDemandMult shouldBe sectorDemandMult
    trace.nominalSignals.realizedInflation shouldBe Rate.decimal(3, 2)
    trace.firmDynamics.netFirmBirths shouldBe 8
  }

  "SeedTransitionTrace" should "derive the seed transition from typed month boundaries" in {
    val seedIn  = MonthSemantics.seedIn(
      DecisionSignals(
        unemploymentRate = Share.decimal(8, 2),
        inflation = Rate.decimal(2, 2),
        expectedInflation = Rate.decimal(25, 3),
        laggedHiringSlack = Share.decimal(9, 1),
        startupAbsorptionRate = Share.decimal(7, 1),
        sectorDemandMult = sectorDemandMult,
        sectorDemandPressure = sectorDemandPressure,
        sectorHiringSignal = sectorHiringSignal,
      ),
    )
    val seedOut = MonthSemantics.seedOut(
      SignalExtraction.compute(
        SignalExtraction.Input(
          labor = SignalExtraction.LaborOutcomes(
            unemploymentRate = Share.decimal(7, 2),
            laggedHiringSlack = Share.decimal(85, 2),
            startupAbsorptionRate = Share.decimal(75, 2),
          ),
          nominal = SignalExtraction.NominalOutcomes(
            inflation = Rate.decimal(3, 2),
            expectedInflation = Rate.decimal(28, 3),
          ),
          demand = SignalExtraction.DemandOutcomes(
            sectorDemandMult = sectorDemandMult,
            sectorDemandPressure = sectorDemandPressure,
            sectorHiringSignal = sectorHiringSignal,
          ),
        ),
      ),
    )

    val transition = SeedTransitionTrace.from(seedIn, seedOut)

    transition.seedIn shouldBe seedIn.decisionSignals
    transition.seedOut shouldBe seedOut.nextSeed
    transition.provenance shouldBe seedOut.provenance
  }
