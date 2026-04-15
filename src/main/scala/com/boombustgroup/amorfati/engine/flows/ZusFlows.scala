package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.ledger.TreasuryRuntimeContract
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** ZUS/FUS mechanism emitting flows instead of mutating state.
  *
  * Same economic logic as SocialSecurity.zusStep — same formulas, same
  * calibration. But instead of returning ZusState via .copy(), returns
  * Vector[Flow] for the interpreter.
  *
  * Three flows per month:
  *   1. Contributions: HH → FUS (employed × wage × rate × scale)
  *   2. Pensions: FUS → HH (retirees × basePension)
  *   3. Gov subvention: GOV → FUS (covers deficit, if any)
  *
  * Account IDs (flat namespace for Flow): 0 = HH aggregate, 1 = FUS, 2 = GOV
  */
object ZusFlows:

  val HH_ACCOUNT: Int  = 0
  val FUS_ACCOUNT: Int = 1
  val GOV_ACCOUNT: Int = 2

  case class ZusInput(
      employed: Int,
      wage: PLN,
      nRetirees: Int,
  )

  def emitBatches(input: ZusInput)(using p: SimParams, topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val contributions = input.employed * (input.wage * p.social.zusContribRate * p.social.zusScale)
    val pensions      = input.nRetirees * p.social.zusBasePension
    val deficit       = pensions - contributions
    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.aggregate,
        EntitySector.Funds,
        topology.funds.zus,
        contributions,
        AssetType.Cash,
        FlowMechanism.ZusContribution,
      ),
      AggregateBatchedEmission
        .transfer(
          EntitySector.Funds,
          topology.funds.zus,
          EntitySector.Households,
          topology.households.aggregate,
          pensions,
          AssetType.Cash,
          FlowMechanism.ZusPension,
        ),
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        EntitySector.Funds,
        topology.funds.zus,
        deficit,
        AssetType.Cash,
        FlowMechanism.ZusGovSubvention,
      ),
    )

  def emit(input: ZusInput)(using p: SimParams): Vector[Flow] =
    val contributions = input.employed * (input.wage * p.social.zusContribRate * p.social.zusScale)
    val pensions      = input.nRetirees * p.social.zusBasePension
    val deficit       = pensions - contributions

    val flows = Vector.newBuilder[Flow]

    if contributions > PLN.Zero then
      flows += Flow(from = HH_ACCOUNT, to = FUS_ACCOUNT, amount = contributions.toLong, mechanism = FlowMechanism.ZusContribution.toInt)

    if pensions > PLN.Zero then flows += Flow(from = FUS_ACCOUNT, to = HH_ACCOUNT, amount = pensions.toLong, mechanism = FlowMechanism.ZusPension.toInt)

    if deficit > PLN.Zero then flows += Flow(from = GOV_ACCOUNT, to = FUS_ACCOUNT, amount = deficit.toLong, mechanism = FlowMechanism.ZusGovSubvention.toInt)

    flows.result()
