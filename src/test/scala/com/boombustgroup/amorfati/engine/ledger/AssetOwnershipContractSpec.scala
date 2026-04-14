package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.ledger.AssetOwnershipContract.*
import com.boombustgroup.ledger.{AssetType, EntitySector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AssetOwnershipContractSpec extends AnyFlatSpec with Matchers:

  import LedgerTestFixtures.enrichedSimState

  "AssetOwnershipContract" should "classify every public ledger asset exactly once" in {
    val grouped = publicAssets.groupBy(_.asset)

    grouped.keySet shouldBe AssetType.values.toSet
    grouped.values.foreach(_ should have size 1)
  }

  it should "accept every materialized adapter balance and preserve fund-slot granularity" in {
    val runtime = enrichedSimState()
    val ledger  = LedgerStateAdapter.toMutableWorldState(runtime)

    ledger.snapshot.keySet.foreach { case (sector, asset, index) =>
      isSupportedPersistedPair(sector, asset, index) shouldBe true
    }

    supportedPairs should contain(SupportedPair(SectorId.Fixed(EntitySector.Funds, LedgerStateAdapter.FundIndex.Zus), AssetType.Cash))
    supportedPairs should not contain SupportedPair(
      SectorId.Fixed(EntitySector.Funds, LedgerStateAdapter.FundIndex.QuasiFiscal),
      AssetType.Cash,
    )
    isSupportedPersistedPair(EntitySector.Funds, AssetType.Cash, LedgerStateAdapter.FundIndex.Zus) shouldBe true
    isSupportedPersistedPair(EntitySector.Funds, AssetType.Cash, LedgerStateAdapter.FundIndex.QuasiFiscal) shouldBe false
  }

  it should "track currently unsupported persisted families explicitly" in {
    val runtime = enrichedSimState()

    presentUnsupportedFamilies(runtime) shouldBe unsupportedFamilies.map(_.id).toSet
  }

  it should "mark orphan public assets as outside the current engine contract" in {
    orphanPublicAssets shouldBe Set(AssetType.StandingFacility, AssetType.Capital)
    publicAsset(AssetType.StandingFacility).status shouldBe PublicAssetStatus.PublicAssetWithoutEngineContract
    publicAsset(AssetType.Capital).status shouldBe PublicAssetStatus.PublicAssetWithoutEngineContract
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
      (EntitySector.Foreign, "Foreign.Aggregate"),
    )
    nonPersistedRuntimeShells.map(shell => (shell.sector, shell.index)).toSet.size shouldBe nonPersistedRuntimeShells.size
  }

end AssetOwnershipContractSpec
