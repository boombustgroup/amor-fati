package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Runtime evidence for BGK/PFR quasi-fiscal monetary channels.
  *
  * Quasi-fiscal bonds are separate from SPW, so they use
  * `AssetType.QuasiFiscalBond`. Subsidized lending has two explicit runtime
  * legs: the quasi-fiscal loan portfolio movement and the bank-deposit
  * creation/destruction side of that credit channel.
  */
object QuasiFiscalFlows:

  case class Input(
      bankBondIssuance: PLN,
      nbpBondAbsorption: PLN,
      bankBondAmortization: PLN,
      nbpBondAmortization: PLN,
      lending: PLN,
      repayment: PLN,
  )

  def emitBatches(input: Input)(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    Vector.concat(
      emitBankBondIssuance(input.bankBondIssuance),
      AggregateBatchedEmission.transfer(
        EntitySector.Funds,
        topology.funds.quasiFiscal,
        EntitySector.NBP,
        topology.nbp.persistedOwner,
        input.nbpBondAbsorption,
        AssetType.QuasiFiscalBond,
        FlowMechanism.QuasiFiscalNbpAbsorption,
      ),
      emitBankBondAmortization(input.bankBondAmortization),
      AggregateBatchedEmission.transfer(
        EntitySector.NBP,
        topology.nbp.persistedOwner,
        EntitySector.Funds,
        topology.funds.quasiFiscal,
        input.nbpBondAmortization,
        AssetType.QuasiFiscalBond,
        FlowMechanism.QuasiFiscalBondAmortization,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Funds,
        topology.funds.quasiFiscal,
        EntitySector.Firms,
        topology.firms.aggregate,
        input.lending,
        AssetType.NbfiLoan,
        FlowMechanism.QuasiFiscalLending,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.aggregate,
        EntitySector.Funds,
        topology.funds.quasiFiscal,
        input.repayment,
        AssetType.NbfiLoan,
        FlowMechanism.QuasiFiscalRepayment,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Banks,
        topology.banks.aggregate,
        EntitySector.Firms,
        topology.firms.aggregate,
        input.lending,
        AssetType.DemandDeposit,
        FlowMechanism.QuasiFiscalLendingDeposit,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.aggregate,
        EntitySector.Banks,
        topology.banks.aggregate,
        input.repayment,
        AssetType.DemandDeposit,
        FlowMechanism.QuasiFiscalRepaymentDeposit,
      ),
    )

  private def emitBankBondIssuance(amount: PLN)(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val allocated = distributeAcrossPersistedBanks("bankBondIssuance", amount)
    val nonZero   = allocated.zipWithIndex.collect { case (raw, bankIndex) if raw > 0L => raw -> bankIndex }
    if nonZero.isEmpty then Vector.empty
    else
      Vector(
        BatchedFlow.Broadcast(
          from = EntitySector.Funds,
          fromIndex = topology.funds.quasiFiscal,
          to = EntitySector.Banks,
          amounts = nonZero.map(_._1).toArray,
          targetIndices = nonZero.map(_._2).toArray,
          asset = AssetType.QuasiFiscalBond,
          mechanism = FlowMechanism.QuasiFiscalBondIssuance,
        ),
      )

  private def emitBankBondAmortization(amount: PLN)(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val persisted = distributeAcrossPersistedBanks("bankBondAmortization", amount)
    val amounts   = persisted ++ Array.fill(topology.banks.sectorSize - topology.banks.persistedCount)(0L)
    if amounts.forall(_ == 0L) then Vector.empty
    else
      Vector(
        BatchedFlow.Scatter(
          from = EntitySector.Banks,
          to = EntitySector.Funds,
          amounts = amounts,
          targetIndices = Array.fill(topology.banks.sectorSize)(topology.funds.quasiFiscal),
          asset = AssetType.QuasiFiscalBond,
          mechanism = FlowMechanism.QuasiFiscalBondAmortization,
        ),
      )

  private def distributeAcrossPersistedBanks(fieldName: String, amount: PLN)(using topology: RuntimeLedgerTopology): Array[Long] =
    if amount <= PLN.Zero then Array.fill(topology.banks.persistedCount)(0L)
    else
      require(
        topology.banks.persistedCount > 0,
        s"QuasiFiscalFlows.$fieldName requires at least one persisted bank for positive amount $amount",
      )
      Distribute.distribute(amount.distributeRaw, Array.fill(topology.banks.persistedCount)(1L))

end QuasiFiscalFlows
