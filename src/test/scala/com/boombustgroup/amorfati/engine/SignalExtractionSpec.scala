package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SignalExtractionSpec extends AnyFlatSpec with Matchers:

  "SignalExtraction.compute" should "produce next-month DecisionSignals from explicit realized inputs" in {
    val result = SignalExtraction.compute(
      SignalExtraction.Input(
        labor = SignalExtraction.LaborOutcomes(
          unemploymentRate = Share("0.18"),
          laggedHiringSlack = Share("0.72"),
          startupAbsorptionRate = Share("0.41"),
        ),
        nominal = SignalExtraction.NominalOutcomes(
          inflation = Rate("0.03"),
          expectedInflation = Rate("0.025"),
        ),
        demand = SignalExtraction.DemandOutcomes(
          sectorDemandMult = Vector(Multiplier("1.1"), Multiplier("0.9")),
          sectorDemandPressure = Vector(Multiplier("1.3"), Multiplier("0.95")),
          sectorHiringSignal = Vector(Multiplier("1.2"), Multiplier("0.98")),
        ),
      ),
    )

    result.seedOut shouldBe DecisionSignals(
      unemploymentRate = Share("0.18"),
      inflation = Rate("0.03"),
      expectedInflation = Rate("0.025"),
      laggedHiringSlack = Share("0.72"),
      startupAbsorptionRate = Share("0.41"),
      sectorDemandMult = Vector(Multiplier("1.1"), Multiplier("0.9")),
      sectorDemandPressure = Vector(Multiplier("1.3"), Multiplier("0.95")),
      sectorHiringSignal = Vector(Multiplier("1.2"), Multiplier("0.98")),
    )
  }

  it should "derive typed seed provenance from the extraction boundary itself" in {
    val result = SignalExtraction.compute(
      SignalExtraction.Input(
        labor = SignalExtraction.LaborOutcomes(
          unemploymentRate = Share("0.11"),
          laggedHiringSlack = Share("0.66"),
          startupAbsorptionRate = Share("0.55"),
        ),
        nominal = SignalExtraction.NominalOutcomes(
          inflation = Rate("-0.01"),
          expectedInflation = Rate("0.02"),
        ),
        demand = SignalExtraction.DemandOutcomes(
          sectorDemandMult = Vector(Multiplier.One),
          sectorDemandPressure = Vector(Multiplier("1.15")),
          sectorHiringSignal = Vector(Multiplier("1.08")),
        ),
      ),
    )

    result.provenance.unemploymentRate.stage shouldBe MonthTraceStage.WorldAssemblyEconomics
    result.provenance.inflation.stage shouldBe MonthTraceStage.PriceEquityEconomics
    result.provenance.expectedInflation.stage shouldBe MonthTraceStage.OpenEconEconomics
    result.provenance.laggedHiringSlack.stage shouldBe MonthTraceStage.LaborEconomics
    result.provenance.startupAbsorptionRate.stage shouldBe MonthTraceStage.StartupStaffing
    result.provenance.sectorDemandMult.stage shouldBe MonthTraceStage.DemandEconomics
    result.provenance.sectorDemandPressure.stage shouldBe MonthTraceStage.DemandEconomics
    result.provenance.sectorHiringSignal.stage shouldBe MonthTraceStage.DemandEconomics
    result.provenance.startupAbsorptionRate.value shouldBe Share("0.55")
    result.provenance.startupAbsorptionRate.source shouldBe "WorldAssemblyEconomics.applyStartupStaffing.startupAbsorptionRate"
    result.provenance.sectorDemandPressure.source shouldBe "s4.sectorDemandPressure"
    result.provenance.sectorHiringSignal.source shouldBe "s4.sectorHiringSignal"
  }
