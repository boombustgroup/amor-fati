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
  * stocks with the same precision. Public fund cash and quasi-fiscal bond/loan
  * stocks are currently materialized from executed runtime deltas.
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

  /** Supported BGK/PFR stock slice recovered from executed quasi-fiscal runtime
    * deltas.
    */
  final case class QuasiFiscalProjection(
      bondsOutstanding: PLN,
      loanPortfolio: PLN,
      bankHoldings: PLN,
      nbpHoldings: PLN,
  )

  final case class Projection(
      ledgerFinancialState: LedgerFinancialState,
      publicFundCash: PublicFundCashProjection,
      quasiFiscal: QuasiFiscalProjection,
  )

  def materializeSupportedState(
      opening: LedgerFinancialState,
      semanticClosing: LedgerFinancialState,
      deltaLedger: Map[(EntitySector, AssetType, Int), Long],
      topology: RuntimeLedgerTopology,
  ): Projection =
    requireMaterializedSlotsAreSupported(topology)
    val publicFundCash = projectPublicFundCash(opening, deltaLedger)
    val quasiFiscal    = projectQuasiFiscal(opening, deltaLedger, topology)
    val projectedFunds = semanticClosing.funds.copy(
      zusCash = publicFundCash.zusCash,
      nfzCash = publicFundCash.nfzCash,
      fpCash = publicFundCash.fpCash,
      pfronCash = publicFundCash.pfronCash,
      fgspCash = publicFundCash.fgspCash,
      jstCash = publicFundCash.jstCash,
      quasiFiscal = LedgerFinancialState.QuasiFiscalBalances(
        bondsOutstanding = quasiFiscal.bondsOutstanding,
        loanPortfolio = quasiFiscal.loanPortfolio,
        bankHoldings = quasiFiscal.bankHoldings,
        nbpHoldings = quasiFiscal.nbpHoldings,
      ),
    )
    Projection(
      ledgerFinancialState = semanticClosing.copy(funds = projectedFunds),
      publicFundCash = publicFundCash,
      quasiFiscal = quasiFiscal,
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

  def projectQuasiFiscal(
      opening: LedgerFinancialState,
      deltaLedger: Map[(EntitySector, AssetType, Int), Long],
      topology: RuntimeLedgerTopology,
  ): QuasiFiscalProjection =
    requireQuasiFiscalSlotsAreSupported(topology)
    val openingQf       = opening.funds.quasiFiscal
    val issuerBondDelta =
      deltaLedger.getOrElse((EntitySector.Funds, AssetType.QuasiFiscalBond, FundRuntimeIndex.QuasiFiscal), 0L)
    val bankBondDelta   = (0 until topology.banks.persistedCount).iterator
      .map(bankIndex => deltaLedger.getOrElse((EntitySector.Banks, AssetType.QuasiFiscalBond, bankIndex), 0L))
      .sum
    val nbpBondDelta    =
      deltaLedger.getOrElse((EntitySector.NBP, AssetType.QuasiFiscalBond, topology.nbp.persistedOwner), 0L)
    val loanIssuerDelta =
      deltaLedger.getOrElse((EntitySector.Funds, AssetType.NbfiLoan, FundRuntimeIndex.QuasiFiscal), 0L)
    QuasiFiscalProjection(
      bondsOutstanding = openingQf.bondsOutstanding - PLN.fromRaw(issuerBondDelta),
      loanPortfolio = openingQf.loanPortfolio - PLN.fromRaw(loanIssuerDelta),
      bankHoldings = openingQf.bankHoldings + PLN.fromRaw(bankBondDelta),
      nbpHoldings = openingQf.nbpHoldings + PLN.fromRaw(nbpBondDelta),
    )

  private def requireMaterializedSlotsAreSupported(topology: RuntimeLedgerTopology): Unit =
    MaterializedPublicFundCashSlots.foreach: fundIndex =>
      require(
        AssetOwnershipContract.isSupportedPersistedPair(topology, EntitySector.Funds, AssetType.Cash, fundIndex),
        s"RuntimeFlowProjection cannot materialize unsupported fund cash slot $fundIndex",
      )

  private def requireQuasiFiscalSlotsAreSupported(topology: RuntimeLedgerTopology): Unit =
    val required = Vector(
      (EntitySector.Funds, AssetType.QuasiFiscalBond, FundRuntimeIndex.QuasiFiscal),
      (EntitySector.Funds, AssetType.NbfiLoan, FundRuntimeIndex.QuasiFiscal),
      (EntitySector.NBP, AssetType.QuasiFiscalBond, topology.nbp.persistedOwner),
    ) ++ (0 until topology.banks.persistedCount).map(bankIndex => (EntitySector.Banks, AssetType.QuasiFiscalBond, bankIndex))

    required.foreach:
      case (sector, asset, index) =>
        require(
          AssetOwnershipContract.isSupportedPersistedPair(topology, sector, asset, index),
          s"RuntimeFlowProjection cannot materialize unsupported quasi-fiscal slot ($sector,$asset,$index)",
        )

end RuntimeFlowProjection
