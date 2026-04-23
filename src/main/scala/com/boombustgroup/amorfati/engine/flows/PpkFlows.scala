package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.agents.SocialSecurity
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

  case class PpkInput(ppk: SocialSecurity.PpkState)

  object PpkInput:
    def apply(employed: Int, wage: PLN)(using p: SimParams): PpkInput =
      PpkInput(SocialSecurity.ppkStep(employed, wage))

  def emitBatches(input: PpkInput)(using @scala.annotation.unused p: SimParams, topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    AggregateBatchedEmission.transfer(
      EntitySector.Households,
      topology.households.aggregate,
      EntitySector.Funds,
      topology.funds.ppk,
      input.ppk.contributions,
      AssetType.Cash,
      FlowMechanism.PpkContribution,
    )

  def emit(input: PpkInput)(using @scala.annotation.unused p: SimParams): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]

    if input.ppk.contributions > PLN.Zero then flows += Flow(HH_ACCOUNT, PPK_ACCOUNT, input.ppk.contributions.toLong, FlowMechanism.PpkContribution.toInt)

    flows.result()
