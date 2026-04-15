package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.ledger.{AssetType, EntitySector}

/** Explicit runtime contract for NBP settlement and asset-side stock roles.
  *
  * The engine persists NBP asset-side stocks, while reserve-side monetary
  * operations execute through an explicit runtime shell in the shared runtime
  * topology. This contract names those roles explicitly:
  *   - NBP government bond holdings are a persisted stock
  *   - NBP FX reserves are a persisted stock
  *   - reserve settlement is a non-persisted delta-only liability shell
  */
object NbpRuntimeContract:

  enum RuntimeRole:
    case GovBondAssetStock
    case FxReserveAssetStock
    case ReserveSettlementLiabilityShell
    case StandingFacilityBackstopShell

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
      index = 1,
      role = RuntimeRole.ReserveSettlementLiabilityShell,
      persistedAsStock = false,
      asset = AssetType.Reserve,
    )

  val StandingFacilityBackstop: RuntimeNode =
    RuntimeNode(
      name = "NBP.StandingFacilityBackstop",
      sector = EntitySector.NBP,
      index = 1,
      role = RuntimeRole.StandingFacilityBackstopShell,
      persistedAsStock = false,
      asset = AssetType.StandingFacility,
    )

end NbpRuntimeContract
