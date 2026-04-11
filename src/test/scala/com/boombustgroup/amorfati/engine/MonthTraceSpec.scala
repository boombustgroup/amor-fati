package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.types.{Multiplier, Rate, Share}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MonthTraceSpec extends AnyFlatSpec with Matchers:

  private val sectorDemandMult     = Vector(Multiplier(1.1), Multiplier(0.9))
  private val sectorDemandPressure = Vector(Multiplier(1.2), Multiplier(0.8))
  private val sectorHiringSignal   = Vector(Multiplier(1.05), Multiplier(0.95))

  "MonthTimingTrace" should "reject duplicate envelope keys" in {
    val err = intercept[IllegalArgumentException]:
      MonthTimingTrace(
        Vector(
          MonthTimingEnvelope(MonthTimingEnvelopeKey.LaborSignals, MonthTimingPayload.LaborSignals(Share(0.4))),
          MonthTimingEnvelope(MonthTimingEnvelopeKey.LaborSignals, MonthTimingPayload.LaborSignals(Share(0.6))),
        ),
      )

    err.getMessage should include("unique envelope keys")
    err.getMessage should include(MonthTimingEnvelopeKey.LaborSignals.toString)
  }

  it should "fail clearly when a required payload is missing" in {
    val err = intercept[IllegalStateException]:
      MonthTimingTrace(Vector.empty).laborSignals

    err.getMessage should include("LaborSignals")
    err.getMessage should include(MonthTimingEnvelopeKey.LaborSignals.toString)
  }

  it should "build typed envelopes from timing inputs in a stable order" in {
    val trace = MonthTimingTrace.fromInputs(
      MonthTimingInputs(
        labor = MonthTimingPayload.LaborSignals(Share(0.4)),
        demand = MonthTimingPayload.DemandSignals(
          sectorDemandMult = sectorDemandMult,
          sectorDemandPressure = sectorDemandPressure,
          sectorHiringSignal = sectorHiringSignal,
        ),
        nominal = MonthTimingPayload.NominalSignals(
          realizedInflation = Rate(0.03),
          expectedInflation = Rate(0.025),
        ),
        firmDynamics = MonthTimingPayload.FirmDynamics(
          startupAbsorptionRate = Share(0.7),
          firmBirths = 12,
          firmDeaths = 4,
          netFirmBirths = 8,
        ),
      ),
    )

    trace.envelopes.map(_.key) shouldBe Vector(
      MonthTimingEnvelopeKey.LaborSignals,
      MonthTimingEnvelopeKey.DemandSignals,
      MonthTimingEnvelopeKey.NominalSignals,
      MonthTimingEnvelopeKey.FirmDynamics,
    )
    trace.laborSignals.operationalHiringSlack shouldBe Share(0.4)
    trace.demandSignals.sectorDemandMult shouldBe sectorDemandMult
    trace.nominalSignals.realizedInflation shouldBe Rate(0.03)
    trace.firmDynamics.netFirmBirths shouldBe 8
  }

  "SeedTransitionTrace" should "derive the seed transition from typed month boundaries" in {
    val seedIn  = MonthSemantics.At[DecisionSignals, MonthSemantics.Pre](
      DecisionSignals(
        unemploymentRate = Share(0.08),
        inflation = Rate(0.02),
        expectedInflation = Rate(0.025),
        laggedHiringSlack = Share(0.9),
        startupAbsorptionRate = Share(0.7),
        sectorDemandMult = sectorDemandMult,
        sectorDemandPressure = sectorDemandPressure,
        sectorHiringSignal = sectorHiringSignal,
      ),
    )
    val seedOut = MonthSemantics.At[SignalExtraction.Output, MonthSemantics.NextPre](
      SignalExtraction.compute(
        SignalExtraction.inputFromRealizedOutcomes(
          unemploymentRate = Share(0.07),
          laggedHiringSlack = Share(0.85),
          startupAbsorptionRate = Share(0.75),
          inflation = Rate(0.03),
          expectedInflation = Rate(0.028),
          sectorDemandMult = sectorDemandMult,
          sectorDemandPressure = sectorDemandPressure,
          sectorHiringSignal = sectorHiringSignal,
        ),
      ),
    )

    val transition = SeedTransitionTrace.from(seedIn, seedOut)

    transition.seedIn shouldBe seedIn.value
    transition.seedOut shouldBe seedOut.value.seedOut
    transition.provenance shouldBe seedOut.value.provenance
  }
