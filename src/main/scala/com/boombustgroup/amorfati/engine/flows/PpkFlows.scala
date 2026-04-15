package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** PPK (Pracownicze Plany Kapitalowe) mechanism emitting flows.
  *
  * Same logic as SocialSecurity.ppkStep. Employee + employer contributions, PPK
  * buys government bonds proportional to contributions x bond allocation.
  *
  * Account IDs: 0 = HH, 1 = PPK, 2 = GovBondMarket
  */
object PpkFlows:

  val HH_ACCOUNT: Int          = 0
  val PPK_ACCOUNT: Int         = 1
  val BOND_MARKET_ACCOUNT: Int = 2

  case class PpkInput(employed: Int, wage: PLN)

  def emitBatches(input: PpkInput)(using p: SimParams, topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val contributions = input.employed * (input.wage * (p.social.ppkEmployeeRate + p.social.ppkEmployerRate))
    val bondPurchase  = contributions * p.social.ppkBondAlloc
    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.aggregate,
        EntitySector.Funds,
        topology.funds.ppk,
        contributions,
        AssetType.Cash,
        FlowMechanism.PpkContribution,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Funds,
        topology.funds.ppk,
        EntitySector.Funds,
        topology.funds.bondMarket,
        bondPurchase,
        AssetType.GovBondHTM,
        FlowMechanism.PpkBondPurchase,
      ),
    )

  def emit(input: PpkInput)(using p: SimParams): Vector[Flow] =
    val contributions = input.employed * (input.wage * (p.social.ppkEmployeeRate + p.social.ppkEmployerRate))
    val bondPurchase  = contributions * p.social.ppkBondAlloc

    val flows = Vector.newBuilder[Flow]

    if contributions > PLN.Zero then flows += Flow(HH_ACCOUNT, PPK_ACCOUNT, contributions.toLong, FlowMechanism.PpkContribution.toInt)

    if bondPurchase > PLN.Zero then flows += Flow(PPK_ACCOUNT, BOND_MARKET_ACCOUNT, bondPurchase.toLong, FlowMechanism.PpkBondPurchase.toInt)

    flows.result()
