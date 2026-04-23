package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.flows.RuntimeLedgerTopology
import com.boombustgroup.amorfati.engine.ledger.AssetOwnershipContract.RuntimeShellCategory
import com.boombustgroup.ledger.{AssetType, EntitySector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MortgageRuntimeContractSpec extends AnyFlatSpec with Matchers:

  "MortgageRuntimeContract" should "keep mortgage principal settlement outside persisted stock owners" in {
    val topology = RuntimeLedgerTopology.nonZeroPopulation
    val node     = MortgageRuntimeContract.principalSettlement(topology)

    node.sector shouldBe EntitySector.Households
    node.index shouldBe topology.households.mortgagePrincipalSettlement
    node.role shouldBe MortgageRuntimeContract.Role.PrincipalSettlement
    node.asset shouldBe AssetType.MortgageLoan
    node.persistedAsStock shouldBe false

    // Topology-free checks are registry-level: dynamic household ownership is a sector wildcard.
    AssetOwnershipContract.isSupportedPersistedPair(EntitySector.Households, AssetType.MortgageLoan, 0) shouldBe true
    AssetOwnershipContract.isSupportedPersistedPair(EntitySector.Households, AssetType.MortgageLoan, node.index) shouldBe true
    AssetOwnershipContract.isSupportedPersistedPair(EntitySector.Banks, AssetType.MortgageLoan, topology.banks.aggregate) shouldBe false

    // Topology-aware checks reject live aggregate/shell indices outside the persisted household slice.
    AssetOwnershipContract.isSupportedPersistedPair(topology, EntitySector.Households, AssetType.MortgageLoan, 0) shouldBe true
    AssetOwnershipContract.isSupportedPersistedPair(topology, EntitySector.Households, AssetType.MortgageLoan, node.index) shouldBe false
    AssetOwnershipContract.isSupportedPersistedPair(topology, EntitySector.Banks, AssetType.MortgageLoan, topology.banks.aggregate) shouldBe false
  }

  it should "register the template settlement shell in the ownership contract" in {
    AssetOwnershipContract.nonPersistedRuntimeShells should contain(
      AssetOwnershipContract.RuntimeShell(
        MortgageRuntimeContract.TemplatePrincipalSettlement.sector,
        MortgageRuntimeContract.TemplatePrincipalSettlement.index,
        MortgageRuntimeContract.TemplatePrincipalSettlement.name,
        RuntimeShellCategory.SettlementShell,
      ),
    )
  }

end MortgageRuntimeContractSpec
