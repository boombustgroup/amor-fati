package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.ledger.{AssetType, EntitySector}

/** Explicit runtime contract for government treasury settlement shells.
  *
  * Runtime execution now uses a topology aligned with persisted sector
  * ownership plus explicit shell slots. This contract makes the government-side
  * distinction explicit:
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

  val IssuerStockAsset: AssetType        = AssetType.GovBondHTM
  val SovereignIssuerIndex: Int          = 0
  val TreasuryBudgetSettlementIndex: Int = 1
  val TaxpayerCollectionIndex: Int       = 2

  val SovereignIssuerGovBondStock: RuntimeNode =
    RuntimeNode(
      name = "Government.GovBondOutstanding",
      sector = EntitySector.Government,
      index = SovereignIssuerIndex,
      role = RuntimeRole.SovereignIssuerStock,
      persistedAsStock = true,
    )

  val TreasuryBudgetSettlement: RuntimeNode =
    RuntimeNode(
      name = "Government.TreasuryBudgetSettlement",
      sector = EntitySector.Government,
      index = TreasuryBudgetSettlementIndex,
      role = RuntimeRole.TreasuryBudgetSettlementShell,
      persistedAsStock = false,
    )

  val TaxpayerCollection: RuntimeNode =
    RuntimeNode(
      name = "Government.TaxpayerCollectionShell",
      sector = EntitySector.Government,
      index = TaxpayerCollectionIndex,
      role = RuntimeRole.TaxpayerCollectionShell,
      persistedAsStock = false,
    )

end TreasuryRuntimeContract
