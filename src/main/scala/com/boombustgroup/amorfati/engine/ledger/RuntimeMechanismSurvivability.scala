package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.flows.{FlowMechanism, RuntimeLedgerTopology}
import com.boombustgroup.ledger.*

/** Audit contract for emitted runtime mechanisms.
  *
  * Snapshot round-trip tests prove that the currently supported persisted stock
  * slice can survive ledger execution. This matrix covers the complementary
  * question: when a runtime mechanism emits a batch, should auditors expect the
  * emitted legs to round-trip into persisted stock owners, remain execution
  * deltas, or stay outside the supported stock slice entirely?
  */
object RuntimeMechanismSurvivability:

  /** Survivability expectation for a mechanism emitted by runtime flow code. */
  enum Classification:
    /** All observed batch sides are supported persisted stock owners for the
      * concrete runtime topology.
      */
    case RoundTrippableStock

    /** The asset is in the supported stock family, but at least one side is a
      * non-persisted execution or settlement shell.
      */
    case ExecutionDeltaOnly

    /** The mechanism uses an unsupported persisted family or a public metric /
      * backstop asset without an engine persisted-stock contract.
      */
    case UnsupportedOrMetricOnly

  /** Explicit declaration for one emitted mechanism. */
  case class Declaration(
      mechanism: MechanismId,
      classification: Classification,
      note: String,
  )

  /** One concrete owner/asset side touched by a runtime batch. */
  case class BatchSide(
      sector: EntitySector,
      asset: AssetType,
      index: Int,
  )

  import Classification.*

  private def declared(
      classification: Classification,
      note: String,
      mechanisms: MechanismId*,
  ): Vector[Declaration] =
    mechanisms.toVector.map(Declaration(_, classification, note))

  val declarations: Vector[Declaration] = Vector.concat(
    declared(
      ExecutionDeltaOnly,
      "Social insurance flows route cash through household, government, fund, or firm runtime shells.",
      FlowMechanism.ZusContribution,
      FlowMechanism.ZusPension,
      FlowMechanism.ZusGovSubvention,
      FlowMechanism.NfzContribution,
      FlowMechanism.NfzSpending,
      FlowMechanism.NfzGovSubvention,
      FlowMechanism.PpkContribution,
      FlowMechanism.FpContribution,
      FlowMechanism.FpSpending,
      FlowMechanism.FpGovSubvention,
      FlowMechanism.PfronContribution,
      FlowMechanism.PfronSpending,
      FlowMechanism.PfronGovSubvention,
      FlowMechanism.FgspContribution,
      FlowMechanism.FgspSpending,
      FlowMechanism.FgspGovSubvention,
      FlowMechanism.JstRevenue,
      FlowMechanism.JstSpending,
      FlowMechanism.JstGovSubvention,
    ),
    declared(
      RoundTrippableStock,
      "Government-bond primary issuance/redemption and waterfall purchases now route through persisted issuer and holder stock slots.",
      FlowMechanism.GovBondPrimaryMarket,
      FlowMechanism.GovBondForeignPurchase,
      FlowMechanism.NbpQeGovBondPurchase,
      FlowMechanism.PpkBondPurchase,
      FlowMechanism.InsuranceGovBondPurchase,
      FlowMechanism.TfiGovBondPurchase,
    ),
    declared(
      ExecutionDeltaOnly,
      "Government budget flows use treasury settlement, taxpayer collection, and aggregate service-demand shells.",
      FlowMechanism.GovVatRevenue,
      FlowMechanism.GovExciseRevenue,
      FlowMechanism.GovCustomsDutyRevenue,
      FlowMechanism.GovPurchases,
      FlowMechanism.GovDebtService,
      FlowMechanism.GovCapitalInvestment,
      FlowMechanism.GovUnempBenefit,
      FlowMechanism.GovSocialTransfer,
      FlowMechanism.GovEuCofin,
    ),
    declared(
      ExecutionDeltaOnly,
      "Insurance reserve flows use the persisted insurance reserve owner plus the aggregate insurance execution shell.",
      FlowMechanism.InsLifePremium,
      FlowMechanism.InsNonLifePremium,
      FlowMechanism.InsLifeClaim,
      FlowMechanism.InsNonLifeClaim,
      FlowMechanism.InsInvestmentIncome,
    ),
    declared(
      ExecutionDeltaOnly,
      "Household and firm operating flows use aggregate runtime accounts rather than per-owner persisted stock slots.",
      FlowMechanism.HhConsumption,
      FlowMechanism.HhRent,
      FlowMechanism.HhPit,
      FlowMechanism.HhDebtService,
      FlowMechanism.HhDepositInterest,
      FlowMechanism.HhRemittance,
      FlowMechanism.HhCcOrigination,
      FlowMechanism.HhCcDebtService,
      FlowMechanism.HhCcDefault,
      FlowMechanism.HhTotalIncome,
      FlowMechanism.FirmCit,
      FlowMechanism.FirmLoanRepayment,
      FlowMechanism.FirmNewLoan,
      FlowMechanism.FirmInterestPaid,
      FlowMechanism.FirmCapex,
      FlowMechanism.FirmEquityIssuance,
      FlowMechanism.FirmIoPayment,
      FlowMechanism.FirmNplDefault,
      FlowMechanism.FirmProfitShifting,
      FlowMechanism.FirmFdiRepatriation,
      FlowMechanism.FirmGrossInvestment,
      FlowMechanism.InvestNetDepositFlow,
    ),
    declared(
      ExecutionDeltaOnly,
      "Market and open-economy flows are routed through aggregate issuer, investor, foreign, and settlement shells.",
      FlowMechanism.EquityDomDividend,
      FlowMechanism.EquityForDividend,
      FlowMechanism.EquityDividendTax,
      FlowMechanism.EquityGovDividend,
      FlowMechanism.CorpBondCoupon,
      FlowMechanism.CorpBondDefault,
      FlowMechanism.CorpBondIssuance,
      FlowMechanism.CorpBondAmortization,
      FlowMechanism.MortgageOrigination,
      FlowMechanism.MortgageRepayment,
      FlowMechanism.MortgageInterest,
      FlowMechanism.MortgageDefault,
      FlowMechanism.TradeExports,
      FlowMechanism.TradeImports,
      FlowMechanism.TourismExport,
      FlowMechanism.TourismImport,
      FlowMechanism.Fdi,
      FlowMechanism.PortfolioFlow,
      FlowMechanism.CarryTradeFlow,
      FlowMechanism.PrimaryIncome,
      FlowMechanism.EuFunds,
      FlowMechanism.DiasporaInflow,
      FlowMechanism.CapitalFlight,
    ),
    declared(
      ExecutionDeltaOnly,
      "Bank settlement and recapitalisation flows use reserve, deposit, or NBP shell accounts at aggregate level.",
      FlowMechanism.BankReserveInterest,
      FlowMechanism.BankStandingFacility,
      FlowMechanism.BankInterbankInterest,
      FlowMechanism.NbpFxSettlement,
      FlowMechanism.BankBailIn,
    ),
    declared(
      UnsupportedOrMetricOnly,
      "Bank P&L mechanisms use AssetType.Capital, which is persisted state but outside the supported ledger-owned stock slice.",
      FlowMechanism.BankFirmInterest,
      FlowMechanism.BankNplLoss,
      FlowMechanism.BankMortgageNplLoss,
      FlowMechanism.BankCcNplLoss,
      FlowMechanism.BankGovBondIncome,
      FlowMechanism.BankCorpBondCoupon,
      FlowMechanism.BankCorpBondLoss,
      FlowMechanism.BankBfgLevy,
      FlowMechanism.BankUnrealizedLoss,
      FlowMechanism.BankNbpRemittance,
    ),
    declared(
      UnsupportedOrMetricOnly,
      "Standing-facility backstop uses a public asset without an engine persisted-stock contract.",
      FlowMechanism.BankStandingFacilityBackstop,
    ),
  )

  private val duplicateMechanisms =
    declarations.groupBy(_.mechanism).collect { case (mechanism, rows) if rows.size > 1 => mechanism }

  require(
    duplicateMechanisms.isEmpty,
    s"RuntimeMechanismSurvivability must declare each emitted mechanism once: ${duplicateMechanisms.toVector.map(_.toInt).sorted.mkString(",")}.",
  )
  require(
    declarations.forall(_.note.trim.nonEmpty),
    "RuntimeMechanismSurvivability declarations need non-empty audit notes.",
  )

  val declarationsByMechanism: Map[MechanismId, Declaration] =
    declarations.map(row => row.mechanism -> row).toMap

  val declaredMechanisms: Set[MechanismId] =
    declarationsByMechanism.keySet

  def declarationFor(mechanism: MechanismId): Option[Declaration] =
    declarationsByMechanism.get(mechanism)

  def missingDeclarations(mechanisms: Set[MechanismId]): Set[MechanismId] =
    mechanisms.diff(declaredMechanisms)

  def batchSides(batch: BatchedFlow): Vector[BatchSide] =
    batch match
      case scatter: BatchedFlow.Scatter     =>
        scatter.amounts.indices.toVector
          .filter(scatter.amounts(_) != 0L)
          .flatMap: senderIndex =>
            Vector(
              BatchSide(scatter.from, scatter.asset, senderIndex),
              BatchSide(scatter.to, scatter.asset, scatter.targetIndices(senderIndex)),
            )
          .distinct
      case broadcast: BatchedFlow.Broadcast =>
        val sender  = BatchSide(broadcast.from, broadcast.asset, broadcast.fromIndex)
        val targets = broadcast.amounts.indices.toVector
          .filter(broadcast.amounts(_) != 0L)
          .map(targetPosition => BatchSide(broadcast.to, broadcast.asset, broadcast.targetIndices(targetPosition)))
        if targets.nonEmpty then (sender +: targets).distinct else Vector.empty

  def observedClassification(
      batch: BatchedFlow,
      topology: RuntimeLedgerTopology,
  ): Classification =
    val assetStatus = AssetOwnershipContract.publicAsset(batch.asset).status
    if assetStatus != AssetOwnershipContract.PublicAssetStatus.SupportedPersistedStock then UnsupportedOrMetricOnly
    else
      val sides = batchSides(batch)
      if sides.nonEmpty && sides.forall(side => AssetOwnershipContract.isSupportedPersistedPair(topology, side.sector, side.asset, side.index)) then
        RoundTrippableStock
      else ExecutionDeltaOnly

end RuntimeMechanismSurvivability
