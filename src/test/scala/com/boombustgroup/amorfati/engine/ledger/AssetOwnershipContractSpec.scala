package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.ledger.AssetOwnershipContract.*
import com.boombustgroup.amorfati.engine.flows.RuntimeLedgerTopology
import com.boombustgroup.ledger.{AssetType, EntitySector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AssetOwnershipContractSpec extends AnyFlatSpec with Matchers:

  "AssetOwnershipContract" should "classify every public ledger asset exactly once" in {
    val grouped = publicAssets.groupBy(_.asset)

    grouped.keySet shouldBe AssetType.values.toSet
    grouped.values.foreach(_ should have size 1)
  }

  it should "preserve fund-slot granularity in supported persisted pairs" in {
    supportedPairs should contain(SupportedPair(SectorId.Fixed(EntitySector.Funds, FundRuntimeIndex.Zus), AssetType.Cash))
    supportedPairs should not contain SupportedPair(
      SectorId.Fixed(EntitySector.Funds, FundRuntimeIndex.QuasiFiscal),
      AssetType.Cash,
    )
    isSupportedPersistedPair(EntitySector.Funds, AssetType.Cash, FundRuntimeIndex.Zus) shouldBe true
    isSupportedPersistedPair(EntitySector.Funds, AssetType.Cash, FundRuntimeIndex.QuasiFiscal) shouldBe false
  }

  it should "distinguish persisted dynamic owners from appended runtime shells" in {
    val topology = RuntimeLedgerTopology.nonZeroPopulation

    isSupportedPersistedPair(topology, EntitySector.Firms, AssetType.Cash, 0) shouldBe true
    isSupportedPersistedPair(topology, EntitySector.Firms, AssetType.Cash, topology.firms.persistedCount - 1) shouldBe true
    isSupportedPersistedPair(topology, EntitySector.Firms, AssetType.Cash, topology.firms.aggregate) shouldBe false
    isSupportedPersistedPair(topology, EntitySector.Banks, AssetType.Reserve, topology.banks.aggregate) shouldBe false
    isSupportedPersistedPair(EntitySector.Firms, AssetType.Cash, topology.firms.aggregate) shouldBe true
  }

  it should "track currently unsupported persisted and metric families explicitly" in {
    import LedgerTestFixtures.enrichedSimState

    val runtime = enrichedSimState()

    presentUnsupportedFamilies(runtime) shouldBe unsupportedFamilies.map(_.id).toSet
  }

  it should "classify foreign equity share and BoP stocks as metric-only audit exceptions" in {
    val metricOnlyIds = unsupportedFamilies
      .filter(_.category == UnsupportedCategory.MetricOnly)
      .map(_.id)
      .toSet

    metricOnlyIds should contain allOf (
      UnsupportedFamilyId.EquityForeignOwnershipShare,
      UnsupportedFamilyId.BopExternalPositionMetrics,
    )
    isSupportedPersistedPair(EntitySector.Foreign, AssetType.Equity, 0) shouldBe false
    publicAsset(AssetType.ForeignAsset).supportedSlots shouldBe Set(SectorId.Fixed(EntitySector.NBP, 0))
  }

  it should "mark orphan public assets as outside the current engine contract" in {
    orphanPublicAssets shouldBe Set(AssetType.StandingFacility)
    publicAsset(AssetType.StandingFacility).status shouldBe PublicAssetStatus.PublicAssetWithoutEngineContract
    publicAsset(AssetType.Capital).status shouldBe PublicAssetStatus.UnsupportedPersistedStock
    publicAsset(AssetType.Capital).supportedSlots shouldBe Set(SectorId.Dynamic(EntitySector.Banks))
  }

  it should "expose non-persisted runtime shells separately from supported stock owners" in {
    val settlementShells = nonPersistedRuntimeShells
      .filter(_.category == RuntimeShellCategory.SettlementShell)
      .map(shell => (shell.sector, shell.name))
      .toSet

    settlementShells shouldBe Set(
      (EntitySector.Government, TreasuryRuntimeContract.TreasuryBudgetSettlement.name),
      (EntitySector.Government, TreasuryRuntimeContract.TaxpayerCollection.name),
      (EntitySector.NBP, NbpRuntimeContract.ReserveSettlementLiability.name),
      (EntitySector.NBP, NbpRuntimeContract.StandingFacilityBackstop.name),
      (EntitySector.Foreign, ForeignRuntimeContract.TradeSettlement.name),
      (EntitySector.Foreign, ForeignRuntimeContract.IncomeSettlement.name),
      (EntitySector.Foreign, ForeignRuntimeContract.CapitalSettlement.name),
      (EntitySector.Foreign, ForeignRuntimeContract.TransferSettlement.name),
    )
    nonPersistedRuntimeShells.map(shell => (shell.sector, shell.name)).toSet.size shouldBe nonPersistedRuntimeShells.size
  }

end AssetOwnershipContractSpec
