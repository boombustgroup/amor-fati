package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** PPK (Pracownicze Plany Kapitalowe) mechanism emitting flows.
  *
  * Same logic as SocialSecurity.ppkStep for employee + employer contributions.
  * Government-bond purchases are emitted by GovBondFlows after the banking
  * waterfall resolves actual sold amounts.
  *
  * Account IDs: 0 = HH, 1 = PPK
  */
object PpkFlows:

  val HH_ACCOUNT: Int  = 0
  val PPK_ACCOUNT: Int = 1

  case class PpkInput(employed: Int, wage: PLN)

  def emitBatches(input: PpkInput)(using p: SimParams, topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val contributions = input.employed * (input.wage * (p.social.ppkEmployeeRate + p.social.ppkEmployerRate))
    AggregateBatchedEmission.transfer(
      EntitySector.Households,
      topology.households.aggregate,
      EntitySector.Funds,
      topology.funds.ppk,
      contributions,
      AssetType.Cash,
      FlowMechanism.PpkContribution,
    )

  def emit(input: PpkInput)(using p: SimParams): Vector[Flow] =
    val contributions = input.employed * (input.wage * (p.social.ppkEmployeeRate + p.social.ppkEmployerRate))

    val flows = Vector.newBuilder[Flow]

    if contributions > PLN.Zero then flows += Flow(HH_ACCOUNT, PPK_ACCOUNT, contributions.toLong, FlowMechanism.PpkContribution.toInt)

    flows.result()
