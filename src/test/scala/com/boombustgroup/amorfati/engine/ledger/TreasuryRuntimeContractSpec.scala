package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.ledger.AssetOwnershipContract.RuntimeShellCategory
import com.boombustgroup.ledger.{AssetType, EntitySector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TreasuryRuntimeContractSpec extends AnyFlatSpec with Matchers:

  "TreasuryRuntimeContract" should "separate issuer stock from treasury settlement shells" in {
    TreasuryRuntimeContract.SovereignIssuerGovBondStock.persistedAsStock shouldBe true
    TreasuryRuntimeContract.SovereignIssuerGovBondStock.sector shouldBe EntitySector.Government
    TreasuryRuntimeContract.IssuerStockAsset shouldBe AssetType.GovBondHTM

    TreasuryRuntimeContract.TreasuryBudgetSettlement.persistedAsStock shouldBe false
    TreasuryRuntimeContract.TaxpayerCollection.persistedAsStock shouldBe false
  }

  it should "align with the engine ownership contract" in {
    AssetOwnershipContract.isSupportedPersistedPair(
      TreasuryRuntimeContract.SovereignIssuerGovBondStock.sector,
      TreasuryRuntimeContract.IssuerStockAsset,
      TreasuryRuntimeContract.SovereignIssuerGovBondStock.index,
    ) shouldBe true

    AssetOwnershipContract.isSupportedPersistedPair(
      TreasuryRuntimeContract.TreasuryBudgetSettlement.sector,
      AssetType.Cash,
      TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
    ) shouldBe false

    AssetOwnershipContract.isSupportedPersistedPair(
      TreasuryRuntimeContract.TaxpayerCollection.sector,
      AssetType.Cash,
      TreasuryRuntimeContract.TaxpayerCollection.index,
    ) shouldBe false

    AssetOwnershipContract.nonPersistedRuntimeShells should contain allOf (
      AssetOwnershipContract.RuntimeShell(
        TreasuryRuntimeContract.TreasuryBudgetSettlement.sector,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.name,
        RuntimeShellCategory.SettlementShell,
      ),
      AssetOwnershipContract.RuntimeShell(
        TreasuryRuntimeContract.TaxpayerCollection.sector,
        TreasuryRuntimeContract.TaxpayerCollection.index,
        TreasuryRuntimeContract.TaxpayerCollection.name,
        RuntimeShellCategory.SettlementShell,
      ),
    )
  }

end TreasuryRuntimeContractSpec
