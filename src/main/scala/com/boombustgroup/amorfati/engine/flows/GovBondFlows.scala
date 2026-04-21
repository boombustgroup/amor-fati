package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.engine.economics.BankingEconomics
import com.boombustgroup.amorfati.engine.ledger.{ForeignRuntimeContract, NbpRuntimeContract, TreasuryRuntimeContract}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Holder-resolved government-bond runtime circuits.
  *
  * The banking waterfall owns the actual settlement amounts. This emitter turns
  * those executed movements into batches over real holder slots, so runtime
  * evidence no longer routes SPW activity through synthetic BondMarket or
  * Bondholders nodes.
  */
object GovBondFlows:

  def emitBatches(
      input: BankingEconomics.GovBondRuntimeMovements,
  )(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    Vector.concat(
      emitPrimaryMarket(input.primaryByBank),
      emitBankSale(
        "foreignPurchaseByBank",
        input.foreignPurchaseByBank,
        EntitySector.Foreign,
        ForeignRuntimeContract.GovBondHolderStock.index,
        FlowMechanism.GovBondForeignPurchase,
      ),
      emitBankSale(
        "nbpQePurchaseByBank",
        input.nbpQePurchaseByBank,
        EntitySector.NBP,
        NbpRuntimeContract.GovBondAssetStock.index,
        FlowMechanism.NbpQeGovBondPurchase,
      ),
      emitBankSale("ppkPurchaseByBank", input.ppkPurchaseByBank, EntitySector.Funds, topology.funds.ppk, FlowMechanism.PpkBondPurchase),
      emitBankSale(
        "insurancePurchaseByBank",
        input.insurancePurchaseByBank,
        EntitySector.Insurance,
        topology.insurance.persistedOwner,
        FlowMechanism.InsuranceGovBondPurchase,
      ),
      emitBankSale("tfiPurchaseByBank", input.tfiPurchaseByBank, EntitySector.Funds, topology.funds.nbfi, FlowMechanism.TfiGovBondPurchase),
    )

  private def emitPrimaryMarket(amountsByBank: Vector[PLN])(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val issuance   = amountsByBank.map(_.max(PLN.Zero))
    val redemption = amountsByBank.map(amount => if amount < PLN.Zero then -amount else PLN.Zero)
    Vector.concat(
      emitGovernmentToBanks(issuance, FlowMechanism.GovBondPrimaryMarket),
      emitBankSale(
        "primaryByBank",
        redemption,
        EntitySector.Government,
        TreasuryRuntimeContract.SovereignIssuerGovBondStock.index,
        FlowMechanism.GovBondPrimaryMarket,
      ),
    )

  private def emitGovernmentToBanks(
      amountsByBank: Vector[PLN],
      mechanism: MechanismId,
  )(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val nonZero = persistedBankTargets("primaryByBank", amountsByBank).collect:
      case (amount, bankIndex) if amount > PLN.Zero => amount.toLong -> bankIndex
    if nonZero.isEmpty then Vector.empty
    else
      Vector(
        BatchedFlow.Broadcast(
          from = EntitySector.Government,
          fromIndex = TreasuryRuntimeContract.SovereignIssuerGovBondStock.index,
          to = EntitySector.Banks,
          amounts = nonZero.map(_._1).toArray,
          targetIndices = nonZero.map(_._2).toArray,
          asset = AssetType.GovBondHTM,
          mechanism = mechanism,
        ),
      )

  private def emitBankSale(
      fieldName: String,
      amountsByBank: Vector[PLN],
      toSector: EntitySector,
      toIndex: Int,
      mechanism: MechanismId,
  )(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val amounts = paddedBankAmounts(fieldName, amountsByBank)
    if amounts.forall(_ == 0L) then Vector.empty
    else
      Vector(
        BatchedFlow.Scatter(
          from = EntitySector.Banks,
          to = toSector,
          amounts = amounts,
          targetIndices = Array.fill(topology.banks.sectorSize)(toIndex),
          asset = AssetType.GovBondHTM,
          mechanism = mechanism,
        ),
      )

  private def paddedBankAmounts(fieldName: String, amountsByBank: Vector[PLN])(using topology: RuntimeLedgerTopology): Array[Long] =
    requireBankAmountCount(fieldName, amountsByBank)
    val persisted = amountsByBank.map(_.toLong).toArray
    persisted ++ Array.fill(topology.banks.sectorSize - persisted.length)(0L)

  private def persistedBankTargets(
      fieldName: String,
      amountsByBank: Vector[PLN],
  )(using topology: RuntimeLedgerTopology): Vector[(PLN, Int)] =
    requireBankAmountCount(fieldName, amountsByBank)
    amountsByBank.zipWithIndex

  private def requireBankAmountCount(
      fieldName: String,
      amountsByBank: Vector[PLN],
  )(using topology: RuntimeLedgerTopology): Unit =
    require(
      amountsByBank.length <= topology.banks.persistedCount,
      s"GovBondFlows.$fieldName expected at most ${topology.banks.persistedCount} persisted bank amounts, got ${amountsByBank.length}",
    )

end GovBondFlows
