package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.flows.AggregateBatchContract
import com.boombustgroup.ledger.{AssetType, EntitySector}

/** Explicit runtime contract for government treasury settlement shells.
  *
  * The engine currently uses synthetic aggregate government nodes during batch
  * execution. This contract makes the distinction explicit:
  *   - sovereign issuer stock is the persisted `Government / GovBondHTM` slot
  *   - treasury budget settlement is a non-persisted runtime cash shell
  *   - taxpayer collection is a non-persisted runtime routing shell
  */
object TreasuryRuntimeContract:

  enum RuntimeRole:
    case SovereignIssuerStock
    case TreasuryBudgetSettlementShell
    case TaxpayerCollectionShell

  case class RuntimeNode(
      name: String,
      sector: EntitySector,
      index: Int,
      role: RuntimeRole,
      persistedAsStock: Boolean,
  )

  val IssuerStockAsset: AssetType = AssetType.GovBondHTM

  val SovereignIssuerGovBondStock: RuntimeNode =
    RuntimeNode(
      name = "Government.GovBondOutstanding",
      sector = EntitySector.Government,
      index = 0,
      role = RuntimeRole.SovereignIssuerStock,
      persistedAsStock = true,
    )

  val TreasuryBudgetSettlement: RuntimeNode =
    RuntimeNode(
      name = "Government.TreasuryBudgetSettlement",
      sector = EntitySector.Government,
      index = AggregateBatchContract.GovernmentIndex.Budget,
      role = RuntimeRole.TreasuryBudgetSettlementShell,
      persistedAsStock = false,
    )

  val TaxpayerCollection: RuntimeNode =
    RuntimeNode(
      name = "Government.TaxpayerCollectionShell",
      sector = EntitySector.Government,
      index = AggregateBatchContract.GovernmentIndex.TaxpayerPool,
      role = RuntimeRole.TaxpayerCollectionShell,
      persistedAsStock = false,
    )

end TreasuryRuntimeContract
