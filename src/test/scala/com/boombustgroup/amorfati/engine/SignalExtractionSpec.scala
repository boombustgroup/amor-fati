package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SignalExtractionSpec extends AnyFlatSpec with Matchers:

  "SignalExtraction.compute" should "produce next-month DecisionSignals from explicit realized inputs" in {
    val result = SignalExtraction.compute(
      SignalExtraction.Input(
        labor = SignalExtraction.LaborOutcomes(
          unemploymentRate = Share.decimal(18, 2),
          laggedHiringSlack = Share.decimal(72, 2),
          startupAbsorptionRate = Share.decimal(41, 2),
        ),
        nominal = SignalExtraction.NominalOutcomes(
          inflation = Rate.decimal(3, 2),
          expectedInflation = Rate.decimal(25, 3),
        ),
        demand = SignalExtraction.DemandOutcomes(
          sectorDemandMult = Vector(Multiplier.decimal(11, 1), Multiplier.decimal(9, 1)),
          sectorDemandPressure = Vector(Multiplier.decimal(13, 1), Multiplier.decimal(95, 2)),
          sectorHiringSignal = Vector(Multiplier.decimal(12, 1), Multiplier.decimal(98, 2)),
        ),
      ),
    )

    result.seedOut shouldBe DecisionSignals(
      unemploymentRate = Share.decimal(18, 2),
      inflation = Rate.decimal(3, 2),
      expectedInflation = Rate.decimal(25, 3),
      laggedHiringSlack = Share.decimal(72, 2),
      startupAbsorptionRate = Share.decimal(41, 2),
      sectorDemandMult = Vector(Multiplier.decimal(11, 1), Multiplier.decimal(9, 1)),
      sectorDemandPressure = Vector(Multiplier.decimal(13, 1), Multiplier.decimal(95, 2)),
      sectorHiringSignal = Vector(Multiplier.decimal(12, 1), Multiplier.decimal(98, 2)),
    )
  }

  it should "derive typed seed provenance from the extraction boundary itself" in {
    val result = SignalExtraction.compute(
      SignalExtraction.Input(
        labor = SignalExtraction.LaborOutcomes(
          unemploymentRate = Share.decimal(11, 2),
          laggedHiringSlack = Share.decimal(66, 2),
          startupAbsorptionRate = Share.decimal(55, 2),
        ),
        nominal = SignalExtraction.NominalOutcomes(
          inflation = Rate.decimal(-1, 2),
          expectedInflation = Rate.decimal(2, 2),
        ),
        demand = SignalExtraction.DemandOutcomes(
          sectorDemandMult = Vector(Multiplier.One),
          sectorDemandPressure = Vector(Multiplier.decimal(115, 2)),
          sectorHiringSignal = Vector(Multiplier.decimal(108, 2)),
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
    result.provenance.startupAbsorptionRate.value shouldBe Share.decimal(55, 2)
    result.provenance.startupAbsorptionRate.source shouldBe "WorldAssemblyEconomics.applyStartupStaffing.startupAbsorptionRate"
    result.provenance.sectorDemandPressure.source shouldBe "s4.sectorDemandPressure"
    result.provenance.sectorHiringSignal.source shouldBe "s4.sectorHiringSignal"
  }
