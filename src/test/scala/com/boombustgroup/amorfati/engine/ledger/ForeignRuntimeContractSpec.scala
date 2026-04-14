package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.ledger.AssetOwnershipContract.RuntimeShellCategory
import com.boombustgroup.ledger.{AssetType, EntitySector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ForeignRuntimeContractSpec extends AnyFlatSpec with Matchers:

  "ForeignRuntimeContract" should "separate foreign bond stock ownership from settlement shells" in {
    ForeignRuntimeContract.GovBondHolderStock.persistedAsStock shouldBe true
    ForeignRuntimeContract.GovBondHolderStock.sector shouldBe EntitySector.Foreign
    ForeignRuntimeContract.GovBondHolderStock.asset shouldBe AssetType.GovBondHTM

    all(ForeignRuntimeContract.RuntimeShells.map(_.persistedAsStock)) shouldBe false
    all(ForeignRuntimeContract.RuntimeShells.map(_.asset)) shouldBe AssetType.Cash
  }

  it should "align with the engine ownership contract" in {
    AssetOwnershipContract.isSupportedPersistedPair(
      ForeignRuntimeContract.GovBondHolderStock.sector,
      ForeignRuntimeContract.GovBondHolderStock.asset,
      ForeignRuntimeContract.GovBondHolderStock.index,
    ) shouldBe true

    ForeignRuntimeContract.RuntimeShells.foreach { shell =>
      AssetOwnershipContract.isSupportedPersistedPair(shell.sector, shell.asset, shell.index) shouldBe false

      AssetOwnershipContract.nonPersistedRuntimeShells should contain(
        AssetOwnershipContract.RuntimeShell(
          shell.sector,
          shell.index,
          shell.name,
          RuntimeShellCategory.SettlementShell,
        ),
      )
    }
  }

end ForeignRuntimeContractSpec
