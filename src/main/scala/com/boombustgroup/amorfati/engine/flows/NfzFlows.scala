package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.ledger.TreasuryRuntimeContract
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** NFZ (National Health Fund) mechanism emitting flows.
  *
  * Same logic as SocialSecurity.nfzStep. 9% skladka zdrowotna from employed,
  * spending = per-capita cost x (working-age + retirees x aging elasticity).
  * Deficit covered by government subvention.
  *
  * Account IDs: 0 = HH, 1 = NFZ, 2 = GOV, 3 = Healthcare (spending sink)
  */
object NfzFlows:

  val HH_ACCOUNT: Int         = 0
  val NFZ_ACCOUNT: Int        = 1
  val GOV_ACCOUNT: Int        = 2
  val HEALTHCARE_ACCOUNT: Int = 3

  case class NfzInput(
      employed: Int,
      wage: PLN,
      workingAge: Int,
      nRetirees: Int,
  )

  def emitBatches(input: NfzInput)(using p: SimParams): Vector[BatchedFlow] =
    import AggregateBatchContract.*
    val contributions = input.employed * (input.wage * p.social.nfzContribRate)
    val spending      =
      input.workingAge * p.social.nfzPerCapitaCost +
        input.nRetirees * (p.social.nfzPerCapitaCost * p.social.nfzAgingElasticity)
    val deficit       = spending - contributions
    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        HouseholdIndex.Aggregate,
        EntitySector.Funds,
        FundIndex.Nfz,
        contributions,
        AssetType.Cash,
        FlowMechanism.NfzContribution,
      ),
      AggregateBatchedEmission
        .transfer(EntitySector.Funds, FundIndex.Nfz, EntitySector.Firms, FirmIndex.Services, spending, AssetType.Cash, FlowMechanism.NfzSpending),
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        EntitySector.Funds,
        FundIndex.Nfz,
        deficit,
        AssetType.Cash,
        FlowMechanism.NfzGovSubvention,
      ),
    )

  def emit(input: NfzInput)(using p: SimParams): Vector[Flow] =
    val contributions = input.employed * (input.wage * p.social.nfzContribRate)
    val spending      =
      input.workingAge * p.social.nfzPerCapitaCost +
        input.nRetirees * (p.social.nfzPerCapitaCost * p.social.nfzAgingElasticity)
    val deficit       = spending - contributions

    val flows = Vector.newBuilder[Flow]

    if contributions > PLN.Zero then flows += Flow(HH_ACCOUNT, NFZ_ACCOUNT, contributions.toLong, FlowMechanism.NfzContribution.toInt)

    if spending > PLN.Zero then flows += Flow(NFZ_ACCOUNT, HEALTHCARE_ACCOUNT, spending.toLong, FlowMechanism.NfzSpending.toInt)

    if deficit > PLN.Zero then flows += Flow(GOV_ACCOUNT, NFZ_ACCOUNT, deficit.toLong, FlowMechanism.NfzGovSubvention.toInt)

    flows.result()
