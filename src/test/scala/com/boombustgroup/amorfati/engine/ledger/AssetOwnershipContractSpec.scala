package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.ledger.AssetOwnershipContract.*
import com.boombustgroup.ledger.{AssetType, EntitySector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AssetOwnershipContractSpec extends AnyFlatSpec with Matchers:

  import LedgerTestFixtures.enrichedSimState

  "AssetOwnershipContract" should "classify every public ledger asset exactly once" in {
    publicAssets.map(_.asset).toSet shouldBe AssetType.values.toSet
  }

  it should "match the supported persisted pairs materialized by LedgerStateAdapter" in {
    val runtime = enrichedSimState()
    val ledger  = LedgerStateAdapter.toMutableWorldState(runtime)

    val materializedPairs = ledger.snapshot.keySet.map { case (sector, asset, _) =>
      SupportedPair(sector, asset)
    }

    materializedPairs shouldBe supportedPairs
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
      (EntitySector.Government, "Government.Budget"),
      (EntitySector.Government, "Government.TaxpayerPool"),
      (EntitySector.NBP, "NBP.Aggregate"),
      (EntitySector.Foreign, "Foreign.Aggregate"),
    )
    nonPersistedRuntimeShells.map(shell => (shell.sector, shell.index)).toSet.size shouldBe nonPersistedRuntimeShells.size
  }

end AssetOwnershipContractSpec
