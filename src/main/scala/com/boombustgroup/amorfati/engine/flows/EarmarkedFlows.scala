package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Earmarked funds (FP, PFRON, FGSP) emitting flows.
  *
  * Same logic as EarmarkedFunds.step. Three funds, each with contributions,
  * spending, and gov subvention covering deficit.
  *
  * Account IDs: 0=HH, 1=FP, 2=PFRON, 3=FGSP, 4=GOV, 5=Services (spending sink)
  */
object EarmarkedFlows:

  val HH_ACCOUNT: Int       = 0
  val FP_ACCOUNT: Int       = 1
  val PFRON_ACCOUNT: Int    = 2
  val FGSP_ACCOUNT: Int     = 3
  val GOV_ACCOUNT: Int      = 4
  val SERVICES_ACCOUNT: Int = 5

  case class Input(
      employed: Int,
      wage: PLN,
      unempBenefitSpend: PLN,
      nBankruptFirms: Int,
      avgFirmWorkers: Int,
  )

  def emit(input: Input)(using p: SimParams): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]

    // FP: employer levy → unemployment benefits + ALMP
    val fpContrib = input.employed * (input.wage * p.earmarked.fpRate)
    val fpSpend   = input.unempBenefitSpend + input.employed * p.earmarked.fpAlmpSpendPerWorker
    val fpDeficit = fpSpend - fpContrib

    if fpContrib.toLong > 0L then flows += Flow(HH_ACCOUNT, FP_ACCOUNT, fpContrib.toLong, FlowMechanism.FpContribution.toInt)
    if fpSpend.toLong > 0L then flows += Flow(FP_ACCOUNT, SERVICES_ACCOUNT, fpSpend.toLong, FlowMechanism.FpSpending.toInt)
    if fpDeficit.toLong > 0L then flows += Flow(GOV_ACCOUNT, FP_ACCOUNT, fpDeficit.toLong, FlowMechanism.FpGovSubvention.toInt)

    // PFRON: flat levy → disability spending
    val pfronContrib = p.earmarked.pfronMonthlyRevenue
    val pfronSpend   = p.earmarked.pfronMonthlySpending
    val pfronDeficit = pfronSpend - pfronContrib

    if pfronContrib.toLong > 0L then flows += Flow(HH_ACCOUNT, PFRON_ACCOUNT, pfronContrib.toLong, FlowMechanism.PfronContribution.toInt)
    if pfronSpend.toLong > 0L then flows += Flow(PFRON_ACCOUNT, SERVICES_ACCOUNT, pfronSpend.toLong, FlowMechanism.PfronSpending.toInt)
    if pfronDeficit.toLong > 0L then flows += Flow(GOV_ACCOUNT, PFRON_ACCOUNT, pfronDeficit.toLong, FlowMechanism.PfronGovSubvention.toInt)

    // FGSP: payroll levy → bankruptcy payouts (counter-cyclical)
    val fgspContrib = input.employed * (input.wage * p.earmarked.fgspRate)
    val fgspSpend   = (input.nBankruptFirms * input.avgFirmWorkers) * p.earmarked.fgspPayoutPerWorker
    val fgspDeficit = fgspSpend - fgspContrib

    if fgspContrib.toLong > 0L then flows += Flow(HH_ACCOUNT, FGSP_ACCOUNT, fgspContrib.toLong, FlowMechanism.FgspContribution.toInt)
    if fgspSpend.toLong > 0L then flows += Flow(FGSP_ACCOUNT, SERVICES_ACCOUNT, fgspSpend.toLong, FlowMechanism.FgspSpending.toInt)
    if fgspDeficit.toLong > 0L then flows += Flow(GOV_ACCOUNT, FGSP_ACCOUNT, fgspDeficit.toLong, FlowMechanism.FgspGovSubvention.toInt)

    flows.result()
