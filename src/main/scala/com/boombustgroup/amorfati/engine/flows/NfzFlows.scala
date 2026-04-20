package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.agents.SocialSecurity
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.ledger.TreasuryRuntimeContract
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** NFZ (National Health Fund) mechanism emitting flows.
  *
  * Runtime emitter for the NFZ state computed by [[SocialSecurity.nfzStep]] in
  * the economics pipeline. Keeping the batch input as `NfzState` makes the
  * runtime ledger and SFC semantic projection share one current-month source of
  * truth.
  *
  * Account IDs: 0 = HH, 1 = NFZ, 2 = GOV, 3 = Healthcare (spending sink)
  */
object NfzFlows:

  val HH_ACCOUNT: Int         = 0
  val NFZ_ACCOUNT: Int        = 1
  val GOV_ACCOUNT: Int        = 2
  val HEALTHCARE_ACCOUNT: Int = 3

  case class NfzInput(nfz: SocialSecurity.NfzState)

  object NfzInput:
    def fromDrivers(employed: Int, wage: PLN, workingAge: Int, nRetirees: Int)(using p: SimParams): NfzInput =
      NfzInput(SocialSecurity.nfzStep(employed, wage, workingAge, nRetirees))

  def emitBatches(input: NfzInput)(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val nfz = input.nfz
    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.aggregate,
        EntitySector.Funds,
        topology.funds.nfz,
        nfz.contributions,
        AssetType.Cash,
        FlowMechanism.NfzContribution,
      ),
      AggregateBatchedEmission
        .transfer(EntitySector.Funds, topology.funds.nfz, EntitySector.Firms, topology.firms.services, nfz.spending, AssetType.Cash, FlowMechanism.NfzSpending),
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        EntitySector.Funds,
        topology.funds.nfz,
        nfz.govSubvention,
        AssetType.Cash,
        FlowMechanism.NfzGovSubvention,
      ),
    )

  def emit(input: NfzInput): Vector[Flow] =
    val nfz = input.nfz

    val flows = Vector.newBuilder[Flow]

    if nfz.contributions > PLN.Zero then flows += Flow(HH_ACCOUNT, NFZ_ACCOUNT, nfz.contributions.toLong, FlowMechanism.NfzContribution.toInt)

    if nfz.spending > PLN.Zero then flows += Flow(NFZ_ACCOUNT, HEALTHCARE_ACCOUNT, nfz.spending.toLong, FlowMechanism.NfzSpending.toInt)

    if nfz.govSubvention > PLN.Zero then flows += Flow(GOV_ACCOUNT, NFZ_ACCOUNT, nfz.govSubvention.toLong, FlowMechanism.NfzGovSubvention.toInt)

    flows.result()
