package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.flows.RuntimeLedgerTopology
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.{AssetType, EntitySector}

/** Materializes the ledger-supported part of the next financial state from
  * executed runtime ledger deltas.
  *
  * The runtime interpreter returns month deltas over concrete
  * `(EntitySector, AssetType, index)` accounts. This projection applies the
  * supported deltas that already have a one-to-one persisted owner in
  * [[LedgerFinancialState]]. Other ledger families still come from the
  * economics stages until their emitted batches model holder-resolved closing
  * stocks with the same precision.
  */
object RuntimeFlowProjection:

  /** Public-fund cash buckets that are currently first-class runtime accounts.
    *
    * These slots are fixed fund-sector owners in `RuntimeLedgerTopology` and
    * are also declared as supported persisted cash pairs in
    * [[AssetOwnershipContract]]. They are the first state slice where executed
    * runtime deltas, not stage-local formulas, own the next boundary value.
    */
  val MaterializedPublicFundCashSlots: Set[Int] =
    Set(
      FundRuntimeIndex.Zus,
      FundRuntimeIndex.Nfz,
      FundRuntimeIndex.Fp,
      FundRuntimeIndex.Pfron,
      FundRuntimeIndex.Fgsp,
      FundRuntimeIndex.Jst,
    )

  final case class PublicFundCashProjection(
      zusCash: PLN,
      nfzCash: PLN,
      fpCash: PLN,
      pfronCash: PLN,
      fgspCash: PLN,
      jstCash: PLN,
  )

  final case class Projection(
      ledgerFinancialState: LedgerFinancialState,
      publicFundCash: PublicFundCashProjection,
  )

  def materializeSupportedState(
      opening: LedgerFinancialState,
      semanticClosing: LedgerFinancialState,
      deltaLedger: Map[(EntitySector, AssetType, Int), Long],
      topology: RuntimeLedgerTopology,
  ): Projection =
    requireMaterializedSlotsAreSupported(topology)
    val publicFundCash = projectPublicFundCash(opening, deltaLedger)
    val projectedFunds = semanticClosing.funds.copy(
      zusCash = publicFundCash.zusCash,
      nfzCash = publicFundCash.nfzCash,
      fpCash = publicFundCash.fpCash,
      pfronCash = publicFundCash.pfronCash,
      fgspCash = publicFundCash.fgspCash,
      jstCash = publicFundCash.jstCash,
    )
    Projection(
      ledgerFinancialState = semanticClosing.copy(funds = projectedFunds),
      publicFundCash = publicFundCash,
    )

  def projectPublicFundCash(
      opening: LedgerFinancialState,
      deltaLedger: Map[(EntitySector, AssetType, Int), Long],
  ): PublicFundCashProjection =
    PublicFundCashProjection(
      zusCash = applyFundCashDelta(opening.funds.zusCash, FundRuntimeIndex.Zus, deltaLedger),
      nfzCash = applyFundCashDelta(opening.funds.nfzCash, FundRuntimeIndex.Nfz, deltaLedger),
      fpCash = applyFundCashDelta(opening.funds.fpCash, FundRuntimeIndex.Fp, deltaLedger),
      pfronCash = applyFundCashDelta(opening.funds.pfronCash, FundRuntimeIndex.Pfron, deltaLedger),
      fgspCash = applyFundCashDelta(opening.funds.fgspCash, FundRuntimeIndex.Fgsp, deltaLedger),
      jstCash = applyFundCashDelta(opening.funds.jstCash, FundRuntimeIndex.Jst, deltaLedger),
    )

  private def applyFundCashDelta(
      opening: PLN,
      fundIndex: Int,
      deltaLedger: Map[(EntitySector, AssetType, Int), Long],
  ): PLN =
    opening + PLN.fromRaw(deltaLedger.getOrElse((EntitySector.Funds, AssetType.Cash, fundIndex), 0L))

  private def requireMaterializedSlotsAreSupported(topology: RuntimeLedgerTopology): Unit =
    MaterializedPublicFundCashSlots.foreach: fundIndex =>
      require(
        AssetOwnershipContract.isSupportedPersistedPair(topology, EntitySector.Funds, AssetType.Cash, fundIndex),
        s"RuntimeFlowProjection cannot materialize unsupported fund cash slot $fundIndex",
      )

end RuntimeFlowProjection
