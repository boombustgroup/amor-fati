package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.agents.EarmarkedFunds
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.ledger.TreasuryRuntimeContract
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

  case class Input(state: EarmarkedFunds.State)

  object Input:
    def apply(
        employed: Int,
        wage: PLN,
        unempBenefitSpend: PLN,
        nBankruptFirms: Int,
        avgFirmWorkers: Int,
    )(using p: SimParams): Input =
      Input(EarmarkedFunds.step(employed, wage, unempBenefitSpend, nBankruptFirms, avgFirmWorkers))

  def emitBatches(input: Input)(using @scala.annotation.unused p: SimParams, topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val state     = input.state
    val fpContrib = state.fpContributions
    val fpSpend   = state.fpSpending
    val fpDeficit = fpSpend - fpContrib

    val pfronContrib = state.pfronContributions
    val pfronSpend   = state.pfronSpending
    val pfronDeficit = pfronSpend - pfronContrib

    val fgspContrib = state.fgspContributions
    val fgspSpend   = state.fgspSpending
    val fgspDeficit = fgspSpend - fgspContrib

    Vector.concat(
      AggregateBatchedEmission
        .transfer(
          EntitySector.Households,
          topology.households.aggregate,
          EntitySector.Funds,
          topology.funds.fp,
          fpContrib,
          AssetType.Cash,
          FlowMechanism.FpContribution,
        ),
      AggregateBatchedEmission
        .transfer(EntitySector.Funds, topology.funds.fp, EntitySector.Firms, topology.firms.services, fpSpend, AssetType.Cash, FlowMechanism.FpSpending),
      AggregateBatchedEmission
        .transfer(
          EntitySector.Government,
          TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
          EntitySector.Funds,
          topology.funds.fp,
          fpDeficit,
          AssetType.Cash,
          FlowMechanism.FpGovSubvention,
        ),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.aggregate,
        EntitySector.Funds,
        topology.funds.pfron,
        pfronContrib,
        AssetType.Cash,
        FlowMechanism.PfronContribution,
      ),
      AggregateBatchedEmission
        .transfer(
          EntitySector.Funds,
          topology.funds.pfron,
          EntitySector.Firms,
          topology.firms.services,
          pfronSpend,
          AssetType.Cash,
          FlowMechanism.PfronSpending,
        ),
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        EntitySector.Funds,
        topology.funds.pfron,
        pfronDeficit,
        AssetType.Cash,
        FlowMechanism.PfronGovSubvention,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.aggregate,
        EntitySector.Funds,
        topology.funds.fgsp,
        fgspContrib,
        AssetType.Cash,
        FlowMechanism.FgspContribution,
      ),
      AggregateBatchedEmission
        .transfer(EntitySector.Funds, topology.funds.fgsp, EntitySector.Firms, topology.firms.services, fgspSpend, AssetType.Cash, FlowMechanism.FgspSpending),
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        EntitySector.Funds,
        topology.funds.fgsp,
        fgspDeficit,
        AssetType.Cash,
        FlowMechanism.FgspGovSubvention,
      ),
    )

  def emit(input: Input)(using @scala.annotation.unused p: SimParams): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]
    val state = input.state

    // FP: employer levy → unemployment benefits + ALMP
    val fpContrib = state.fpContributions
    val fpSpend   = state.fpSpending
    val fpDeficit = fpSpend - fpContrib

    if fpContrib > PLN.Zero then flows += Flow(HH_ACCOUNT, FP_ACCOUNT, fpContrib.toLong, FlowMechanism.FpContribution.toInt)
    if fpSpend > PLN.Zero then flows += Flow(FP_ACCOUNT, SERVICES_ACCOUNT, fpSpend.toLong, FlowMechanism.FpSpending.toInt)
    if fpDeficit > PLN.Zero then flows += Flow(GOV_ACCOUNT, FP_ACCOUNT, fpDeficit.toLong, FlowMechanism.FpGovSubvention.toInt)

    // PFRON: flat levy → disability spending
    val pfronContrib = state.pfronContributions
    val pfronSpend   = state.pfronSpending
    val pfronDeficit = pfronSpend - pfronContrib

    if pfronContrib > PLN.Zero then flows += Flow(HH_ACCOUNT, PFRON_ACCOUNT, pfronContrib.toLong, FlowMechanism.PfronContribution.toInt)
    if pfronSpend > PLN.Zero then flows += Flow(PFRON_ACCOUNT, SERVICES_ACCOUNT, pfronSpend.toLong, FlowMechanism.PfronSpending.toInt)
    if pfronDeficit > PLN.Zero then flows += Flow(GOV_ACCOUNT, PFRON_ACCOUNT, pfronDeficit.toLong, FlowMechanism.PfronGovSubvention.toInt)

    // FGSP: payroll levy → bankruptcy payouts (counter-cyclical)
    val fgspContrib = state.fgspContributions
    val fgspSpend   = state.fgspSpending
    val fgspDeficit = fgspSpend - fgspContrib

    if fgspContrib > PLN.Zero then flows += Flow(HH_ACCOUNT, FGSP_ACCOUNT, fgspContrib.toLong, FlowMechanism.FgspContribution.toInt)
    if fgspSpend > PLN.Zero then flows += Flow(FGSP_ACCOUNT, SERVICES_ACCOUNT, fgspSpend.toLong, FlowMechanism.FgspSpending.toInt)
    if fgspDeficit > PLN.Zero then flows += Flow(GOV_ACCOUNT, FGSP_ACCOUNT, fgspDeficit.toLong, FlowMechanism.FgspGovSubvention.toInt)

    flows.result()
