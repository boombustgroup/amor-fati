package com.boombustgroup.amorfati.accounting.matrix

import com.boombustgroup.amorfati.engine.flows.FlowMechanism
import com.boombustgroup.amorfati.engine.ledger.RuntimeMechanismSurvivability
import com.boombustgroup.ledger.{AssetType, EntitySector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SfcMatrixRegistrySpec extends AnyFlatSpec with Matchers:

  "SfcMatrixRegistry" should "classify every public sector, asset, and emitted mechanism" in {
    SfcMatrixRegistry.sectors.map(_.sector).toSet shouldBe EntitySector.values.toSet
    SfcMatrixRegistry.instruments.map(_.asset).toSet shouldBe AssetType.values.toSet
    SfcMatrixRegistry.mechanisms.map(_.mechanism.toInt).toSet shouldBe FlowMechanism.emittedRuntimeMechanisms.map(_.toInt)
  }

  it should "preserve the Poland-facing sector order" in {
    SfcMatrixRegistry.sectors.map(_.sector) shouldBe Vector(
      EntitySector.Households,
      EntitySector.Firms,
      EntitySector.Banks,
      EntitySector.Government,
      EntitySector.NBP,
      EntitySector.Insurance,
      EntitySector.Funds,
      EntitySector.Foreign,
    )
  }

  it should "declare complete rows only for supported holder-issuer slices" in {
    import SfcMatrixRegistry.RowCompleteness.*

    SfcMatrixRegistry.instrument(AssetType.FirmLoan).completeness shouldBe ClassifiedGap
    SfcMatrixRegistry.instrument(AssetType.ConsumerLoan).completeness shouldBe ClassifiedGap
    SfcMatrixRegistry.instrument(AssetType.GovBondHTM).completeness shouldBe Complete
    SfcMatrixRegistry.instrument(AssetType.CorpBond).completeness shouldBe Complete
    SfcMatrixRegistry.instrument(AssetType.MortgageLoan).completeness shouldBe ClassifiedGap
    SfcMatrixRegistry.instrument(AssetType.StandingFacility).completeness shouldBe Excluded
  }

  it should "carry runtime survivability metadata for mechanism rows" in {
    SfcMatrixRegistry.mechanism(FlowMechanism.GovBondPrimaryMarket).survivability shouldBe
      RuntimeMechanismSurvivability.Classification.RoundTrippableStock
    SfcMatrixRegistry.mechanism(FlowMechanism.BankFirmInterest).survivability shouldBe
      RuntimeMechanismSurvivability.Classification.UnsupportedOrMetricOnly
  }

end SfcMatrixRegistrySpec
