package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.flows.RuntimeLedgerTopology
import com.boombustgroup.ledger.{AssetType, EntitySector}

/** Explicit runtime contract for aggregate mortgage principal execution.
  *
  * The persisted mortgage stock slice is household-side only. Runtime execution
  * therefore uses a household-sector principal settlement shell instead of a
  * bank-sector `MortgageLoan` mirror. The shell keeps mortgage principal
  * evidence O(1) per emitted mechanism; per-household mortgage stocks remain
  * owned by the semantic housing state until a cheap holder-resolved projection
  * exists.
  */
object MortgageRuntimeContract:

  enum Role:
    case PrincipalSettlement

  case class RuntimeNode(
      name: String,
      sector: EntitySector,
      index: Int,
      role: Role,
      persistedAsStock: Boolean,
      asset: AssetType,
  )

  def principalSettlement(topology: RuntimeLedgerTopology): RuntimeNode =
    RuntimeNode(
      name = "Households.MortgagePrincipalSettlement",
      sector = EntitySector.Households,
      index = topology.households.mortgagePrincipalSettlement,
      role = Role.PrincipalSettlement,
      persistedAsStock = false,
      asset = AssetType.MortgageLoan,
    )

  /** Zero-population shape template for ownership-contract registration.
    *
    * `TemplatePrincipalSettlement.index` comes from
    * `principalSettlement(RuntimeLedgerTopology.zeroPopulation)`, so it is not
    * meaningful for live simulations. Runtime logic and membership checks must
    * obtain the concrete index from `principalSettlement` using the actual
    * `RuntimeLedgerTopology`.
    */
  val TemplatePrincipalSettlement: RuntimeNode =
    principalSettlement(RuntimeLedgerTopology.zeroPopulation)

end MortgageRuntimeContract
