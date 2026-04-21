package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Corporate bond market emitting flows.
  *
  * Coupon (Firm→Holders), default (gross principal write-off), issuance
  * (Holders→Firm), amortization (Firm→Holders principal return).
  *
  * Account IDs: 0=Firm, 1=BondHolders (aggregate runtime settlement shell)
  */
object CorpBondFlows:

  val FIRM_ACCOUNT: Int   = 0
  val HOLDER_ACCOUNT: Int = 1

  case class HolderBreakdown(
      banks: PLN,
      ppk: PLN,
      other: PLN,
      insurance: PLN,
      nbfi: PLN,
  )

  object HolderBreakdown:
    val zero: HolderBreakdown =
      HolderBreakdown(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

    def copyToOther(amount: PLN): HolderBreakdown =
      zero.copy(other = amount)

  case class Input(
      coupon: PLN,
      defaultAmount: PLN,
      issuance: PLN,
      amortization: PLN,
      couponRecipients: Option[HolderBreakdown] = None,
      defaultRecipients: Option[HolderBreakdown] = None,
      issuanceRecipients: Option[HolderBreakdown] = None,
      amortizationRecipients: Option[HolderBreakdown] = None,
  )

  def emitBatches(input: Input)(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    Vector.concat(
      emitToHolders(
        EntitySector.Firms,
        topology.firms.aggregate,
        input.couponRecipients.getOrElse(HolderBreakdown.copyToOther(input.coupon)),
        AssetType.Cash,
        FlowMechanism.CorpBondCoupon,
      ),
      emitFromHolders(
        EntitySector.Firms,
        topology.firms.aggregate,
        input.defaultRecipients.getOrElse(HolderBreakdown.copyToOther(input.defaultAmount)),
        AssetType.CorpBond,
        FlowMechanism.CorpBondDefault,
      ),
      emitToHolders(
        EntitySector.Firms,
        topology.firms.aggregate,
        input.issuanceRecipients.getOrElse(HolderBreakdown.copyToOther(input.issuance)),
        AssetType.CorpBond,
        FlowMechanism.CorpBondIssuance,
      ),
      emitFromHolders(
        EntitySector.Firms,
        topology.firms.aggregate,
        input.amortizationRecipients.getOrElse(HolderBreakdown.copyToOther(input.amortization)),
        AssetType.CorpBond,
        FlowMechanism.CorpBondAmortization,
      ),
    )

  private def emitToHolders(
      fromSector: EntitySector,
      fromIndex: Int,
      recipients: HolderBreakdown,
      asset: AssetType,
      mechanism: MechanismId,
  )(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    emitHolderTransfers(fromSector, fromIndex, recipients, asset, mechanism, fromIssuer = true)

  private def emitFromHolders(
      toSector: EntitySector,
      toIndex: Int,
      senders: HolderBreakdown,
      asset: AssetType,
      mechanism: MechanismId,
  )(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    emitHolderTransfers(toSector, toIndex, senders, asset, mechanism, fromIssuer = false)

  private def emitHolderTransfers(
      issuerSector: EntitySector,
      issuerIndex: Int,
      breakdown: HolderBreakdown,
      asset: AssetType,
      mechanism: MechanismId,
      fromIssuer: Boolean,
  )(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val holders = Vector(
      (EntitySector.Banks, topology.banks.aggregate, breakdown.banks),
      (EntitySector.Funds, topology.funds.ppk, breakdown.ppk),
      (EntitySector.Funds, topology.funds.corpBondOther, breakdown.other),
      (EntitySector.Insurance, topology.insurance.persistedOwner, breakdown.insurance),
      (EntitySector.Funds, topology.funds.nbfi, breakdown.nbfi),
    )
    holders.flatMap: (holderSector, holderIndex, amount) =>
      if fromIssuer then
        AggregateBatchedEmission.transfer(
          issuerSector,
          issuerIndex,
          holderSector,
          holderIndex,
          amount,
          asset,
          mechanism,
        )
      else
        AggregateBatchedEmission.transfer(
          holderSector,
          holderIndex,
          issuerSector,
          issuerIndex,
          amount,
          asset,
          mechanism,
        )

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]
    if input.coupon > PLN.Zero then flows += Flow(FIRM_ACCOUNT, HOLDER_ACCOUNT, input.coupon.toLong, FlowMechanism.CorpBondCoupon.toInt)
    if input.defaultAmount > PLN.Zero then flows += Flow(FIRM_ACCOUNT, HOLDER_ACCOUNT, input.defaultAmount.toLong, FlowMechanism.CorpBondDefault.toInt)
    if input.issuance > PLN.Zero then flows += Flow(HOLDER_ACCOUNT, FIRM_ACCOUNT, input.issuance.toLong, FlowMechanism.CorpBondIssuance.toInt)
    if input.amortization > PLN.Zero then flows += Flow(FIRM_ACCOUNT, HOLDER_ACCOUNT, input.amortization.toLong, FlowMechanism.CorpBondAmortization.toInt)
    flows.result()
