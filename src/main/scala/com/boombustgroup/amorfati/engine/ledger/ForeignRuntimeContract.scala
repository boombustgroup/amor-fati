package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.flows.AggregateBatchContract
import com.boombustgroup.ledger.{AssetType, EntitySector}

/** Explicit runtime contract for foreign-sector stock ownership and settlement
  * shells.
  *
  * The engine persists only one foreign stock-bearing slice: non-resident
  * government bond holdings. All other foreign-facing monthly cash channels are
  * delta-only settlement shells grouped by economic role.
  */
object ForeignRuntimeContract:

  enum RuntimeRole:
    case GovBondHolderStock
    case TradeSettlementShell
    case IncomeSettlementShell
    case CapitalSettlementShell
    case TransferSettlementShell

  case class RuntimeNode(
      name: String,
      sector: EntitySector,
      index: Int,
      role: RuntimeRole,
      persistedAsStock: Boolean,
      asset: AssetType,
  )

  val GovBondHolderStock: RuntimeNode =
    RuntimeNode(
      name = "Foreign.GovBondPortfolio",
      sector = EntitySector.Foreign,
      index = AggregateBatchContract.ForeignIndex.GovBondHolder,
      role = RuntimeRole.GovBondHolderStock,
      persistedAsStock = true,
      asset = AssetType.GovBondHTM,
    )

  val TradeSettlement: RuntimeNode =
    RuntimeNode(
      name = "Foreign.TradeSettlement",
      sector = EntitySector.Foreign,
      index = AggregateBatchContract.ForeignIndex.TradeSettlement,
      role = RuntimeRole.TradeSettlementShell,
      persistedAsStock = false,
      asset = AssetType.Cash,
    )

  val IncomeSettlement: RuntimeNode =
    RuntimeNode(
      name = "Foreign.IncomeSettlement",
      sector = EntitySector.Foreign,
      index = AggregateBatchContract.ForeignIndex.IncomeSettlement,
      role = RuntimeRole.IncomeSettlementShell,
      persistedAsStock = false,
      asset = AssetType.Cash,
    )

  val CapitalSettlement: RuntimeNode =
    RuntimeNode(
      name = "Foreign.CapitalSettlement",
      sector = EntitySector.Foreign,
      index = AggregateBatchContract.ForeignIndex.CapitalSettlement,
      role = RuntimeRole.CapitalSettlementShell,
      persistedAsStock = false,
      asset = AssetType.Cash,
    )

  val TransferSettlement: RuntimeNode =
    RuntimeNode(
      name = "Foreign.TransferSettlement",
      sector = EntitySector.Foreign,
      index = AggregateBatchContract.ForeignIndex.TransferSettlement,
      role = RuntimeRole.TransferSettlementShell,
      persistedAsStock = false,
      asset = AssetType.Cash,
    )

  val RuntimeShells: Vector[RuntimeNode] =
    Vector(TradeSettlement, IncomeSettlement, CapitalSettlement, TransferSettlement)

end ForeignRuntimeContract
