package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.flows.AggregateBatchContract
import com.boombustgroup.ledger.{AssetType, EntitySector}

/** Explicit runtime contract for NBP settlement and asset-side stock roles.
  *
  * The engine persists NBP asset-side stocks, but reserve-side monetary
  * operations execute through an aggregate runtime shell. This contract names
  * those roles explicitly:
  *   - NBP government bond holdings are a persisted stock
  *   - NBP FX reserves are a persisted stock
  *   - reserve settlement is a non-persisted delta-only liability shell
  */
object NbpRuntimeContract:

  enum RuntimeRole:
    case GovBondAssetStock
    case FxReserveAssetStock
    case ReserveSettlementLiabilityShell

  case class RuntimeNode(
      name: String,
      sector: EntitySector,
      index: Int,
      role: RuntimeRole,
      persistedAsStock: Boolean,
      asset: AssetType,
  )

  val GovBondAssetStock: RuntimeNode =
    RuntimeNode(
      name = "NBP.GovBondPortfolio",
      sector = EntitySector.NBP,
      index = 0,
      role = RuntimeRole.GovBondAssetStock,
      persistedAsStock = true,
      asset = AssetType.GovBondHTM,
    )

  val FxReserveAssetStock: RuntimeNode =
    RuntimeNode(
      name = "NBP.FxReserveAssets",
      sector = EntitySector.NBP,
      index = 0,
      role = RuntimeRole.FxReserveAssetStock,
      persistedAsStock = true,
      asset = AssetType.ForeignAsset,
    )

  val ReserveSettlementLiability: RuntimeNode =
    RuntimeNode(
      name = "NBP.ReserveSettlementLiability",
      sector = EntitySector.NBP,
      index = AggregateBatchContract.NbpIndex.Aggregate,
      role = RuntimeRole.ReserveSettlementLiabilityShell,
      persistedAsStock = false,
      asset = AssetType.Reserve,
    )

end NbpRuntimeContract
