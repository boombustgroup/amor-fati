package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
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

  def emit(input: ZusInput)(using p: SimParams): Vector[Flow] =
    if !p.flags.zus then Vector.empty
    else
      val contributions = input.employed * (input.wage * p.social.zusContribRate * p.social.zusScale)
      val pensions      = input.nRetirees * p.social.zusBasePension
      val deficit       = pensions - contributions

      val flows = Vector.newBuilder[Flow]

      if contributions > PLN.Zero then
        flows += Flow(from = HH_ACCOUNT, to = FUS_ACCOUNT, amount = contributions.toLong, mechanism = FlowMechanism.ZusContribution.toInt)

      if pensions > PLN.Zero then flows += Flow(from = FUS_ACCOUNT, to = HH_ACCOUNT, amount = pensions.toLong, mechanism = FlowMechanism.ZusPension.toInt)

      if deficit > PLN.Zero then flows += Flow(from = GOV_ACCOUNT, to = FUS_ACCOUNT, amount = deficit.toLong, mechanism = FlowMechanism.ZusGovSubvention.toInt)

      flows.result()
