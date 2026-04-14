package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.ledger.AssetOwnershipContract.RuntimeShellCategory
import com.boombustgroup.ledger.{AssetType, EntitySector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NbpRuntimeContractSpec extends AnyFlatSpec with Matchers:

  "NbpRuntimeContract" should "separate persisted NBP assets from reserve settlement liabilities" in {
    NbpRuntimeContract.GovBondAssetStock.persistedAsStock shouldBe true
    NbpRuntimeContract.GovBondAssetStock.sector shouldBe EntitySector.NBP
    NbpRuntimeContract.GovBondAssetStock.asset shouldBe AssetType.GovBondHTM

    NbpRuntimeContract.FxReserveAssetStock.persistedAsStock shouldBe true
    NbpRuntimeContract.FxReserveAssetStock.asset shouldBe AssetType.ForeignAsset

    NbpRuntimeContract.ReserveSettlementLiability.persistedAsStock shouldBe false
    NbpRuntimeContract.ReserveSettlementLiability.asset shouldBe AssetType.Reserve
  }

  it should "align with the engine ownership contract" in {
    AssetOwnershipContract.isSupportedPersistedPair(
      NbpRuntimeContract.GovBondAssetStock.sector,
      NbpRuntimeContract.GovBondAssetStock.asset,
      NbpRuntimeContract.GovBondAssetStock.index,
    ) shouldBe true

    AssetOwnershipContract.isSupportedPersistedPair(
      NbpRuntimeContract.FxReserveAssetStock.sector,
      NbpRuntimeContract.FxReserveAssetStock.asset,
      NbpRuntimeContract.FxReserveAssetStock.index,
    ) shouldBe true

    AssetOwnershipContract.isSupportedPersistedPair(
      NbpRuntimeContract.ReserveSettlementLiability.sector,
      NbpRuntimeContract.ReserveSettlementLiability.asset,
      NbpRuntimeContract.ReserveSettlementLiability.index,
    ) shouldBe false

    AssetOwnershipContract.nonPersistedRuntimeShells should contain(
      AssetOwnershipContract.RuntimeShell(
        NbpRuntimeContract.ReserveSettlementLiability.sector,
        NbpRuntimeContract.ReserveSettlementLiability.index,
        NbpRuntimeContract.ReserveSettlementLiability.name,
        RuntimeShellCategory.SettlementShell,
      ),
    )
  }

end NbpRuntimeContractSpec
