package com.boombustgroup.amorfati.accounting.matrix

import com.boombustgroup.amorfati.accounting.Sfc
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.flows.FlowMechanism
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.engine.ledger.{CorporateBondOwnership, GovernmentBondCircuit}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.BuildInfo
import com.boombustgroup.ledger.{AssetType, BatchedFlow, EntitySector, MechanismId}

object SfcMatrixEvidence:

  enum SnapshotKind:
    case Opening
    case Closing

  enum GapKind:
    case IncompleteCoverage
    case ExplicitlyExcluded
    case UnsupportedDiagnostic

  enum OtherChangeKind:
    case TransactionReconciliation
    case Revaluation
    case DefaultOrWriteOff
    case CoverageGap

  final case class MatrixMetadata(
      seed: Long,
      executionMonth: Int,
      commit: String,
      schemaVersion: String,
      sfcStatus: String,
      matrixStatus: String,
  )

  final case class MatrixGap(
      matrix: String,
      rowKey: String,
      sector: Option[EntitySector],
      kind: GapKind,
      reason: String,
  )

  final case class BsmRow(
      asset: AssetType,
      cells: Map[EntitySector, Long],
      gaps: Vector[MatrixGap],
  ):
    def rowKey: String =
      asset.toString

    def amountRaw(sector: EntitySector): Long =
      cells.getOrElse(sector, 0L)

    def rowSumRaw: Long =
      cells.valuesIterator.sum

    def hasNonZeroCell: Boolean =
      cells.valuesIterator.exists(_ != 0L)

  final case class BsmEvidence(
      snapshotKind: SnapshotKind,
      rows: Vector[BsmRow],
      gaps: Vector[MatrixGap],
  ):
    def row(asset: AssetType): BsmRow =
      rows.find(_.asset == asset).getOrElse(BsmRow(asset, Map.empty, Vector.empty))

  final case class BatchContribution(
      batchIndex: Int,
      batchKind: String,
      from: EntitySector,
      to: EntitySector,
      amountRaw: Long,
      legs: Int,
  )

  final case class TfmRow(
      mechanism: MechanismId,
      asset: AssetType,
      cells: Map[EntitySector, Long],
      contributors: Vector[BatchContribution],
  ):
    def rowKey: String =
      s"${mechanism.toInt}:${asset.toString}"

    def amountRaw(sector: EntitySector): Long =
      cells.getOrElse(sector, 0L)

    def rowSumRaw: Long =
      cells.valuesIterator.sum

  final case class TfmEvidence(
      rows: Vector[TfmRow],
      omittedZeroBatches: Int,
      omittedZeroRows: Int,
      droppedRows: Vector[TfmRow],
      droppedNonRegistrySectors: Vector[EntitySector],
  ):
    def sectorTotals: Map[EntitySector, Long] =
      SfcMatrixRegistry.sectors
        .map(_.sector)
        .map { sector =>
          sector -> rows.iterator.map(_.amountRaw(sector)).sum
        }
        .toMap

    def assetSectorTotals: Map[(AssetType, EntitySector), Long] =
      rows
        .flatMap(row =>
          SfcMatrixRegistry.sectors.map: sector =>
            (row.asset, sector.sector) -> row.amountRaw(sector.sector),
        )
        .groupMapReduce(_._1)(_._2)(_ + _)

  final case class OtherChangeCell(
      asset: AssetType,
      sector: EntitySector,
      stockDeltaRaw: Long,
      transactionDeltaRaw: Long,
      otherChangeRaw: Long,
      kind: OtherChangeKind,
      reason: String,
  )

  final case class OtherChangesEvidence(
      cells: Vector[OtherChangeCell],
      gaps: Vector[MatrixGap],
  ):
    def nonZeroCells: Vector[OtherChangeCell] =
      cells.filter(cell => cell.stockDeltaRaw != 0L || cell.transactionDeltaRaw != 0L || cell.otherChangeRaw != 0L)

  final case class ReconciliationRowMetadata(
      label: String,
      source: String,
      assets: Vector[AssetType],
      mechanisms: Vector[MechanismId],
      note: String,
  )

  final case class StockFlowReconciliationCell(
      identity: Sfc.SfcIdentity,
      label: String,
      expectedRaw: Long,
      actualRaw: Long,
      residualRaw: Long,
      status: String,
      source: String,
      assets: Vector[AssetType],
      mechanisms: Vector[MechanismId],
      note: String,
  )

  final case class StockFlowReconciliationEvidence(
      rows: Vector[StockFlowReconciliationCell],
  ):
    def failures: Vector[StockFlowReconciliationCell] =
      rows.filter(_.residualRaw != 0L)

  enum MatrixValidationError:
    case BsmRowSumError(snapshot: SnapshotKind, asset: AssetType, actualRaw: Long, reason: String)
    case TfmRowSumError(rowKey: String, mechanism: MechanismId, asset: AssetType, actualRaw: Long)
    case TfmNonRegistrySectorError(sectors: Vector[EntitySector])
    case SfcValidationFailed(errors: Vector[Sfc.SfcIdentityError])

  final case class MatrixValidationReport(errors: Vector[MatrixValidationError]):
    def isValid: Boolean =
      errors.isEmpty

  final case class MatrixEvidenceBundle(
      metadata: MatrixMetadata,
      openingBsm: BsmEvidence,
      closingBsm: BsmEvidence,
      tfm: TfmEvidence,
      otherChanges: OtherChangesEvidence,
      reconciliation: StockFlowReconciliationEvidence,
      validation: MatrixValidationReport,
  )

  object BsmEvidence:
    def fromStepOpening(step: FlowSimulation.StepOutput): BsmEvidence =
      fromState(SnapshotKind.Opening, step.stateIn)

    def fromStepClosing(step: FlowSimulation.StepOutput): BsmEvidence =
      fromState(SnapshotKind.Closing, step.nextState)

    def fromState(snapshotKind: SnapshotKind, state: FlowSimulation.SimState): BsmEvidence =
      val ledger = state.ledgerFinancialState
      val bonds  = GovernmentBondCircuit.from(ledger)
      val corp   = CorporateBondOwnership.stockStateFromLedger(ledger)

      val bankCapital = state.banks.iterator.map(_.capital).sumPln
      val entries     = Vector(
        // Deposits and loans.
        (AssetType.DemandDeposit, EntitySector.Households, ledger.households.iterator.map(_.demandDeposit).sumPln),
        (AssetType.DemandDeposit, EntitySector.Banks, -ledger.banks.iterator.map(_.demandDeposit).sumPln),
        (AssetType.TermDeposit, EntitySector.Banks, -ledger.banks.iterator.map(_.termDeposit).sumPln),
        (AssetType.FirmLoan, EntitySector.Banks, ledger.banks.iterator.map(_.firmLoan).sumPln),
        (AssetType.FirmLoan, EntitySector.Firms, -ledger.firms.iterator.map(_.firmLoan).sumPln),
        (AssetType.ConsumerLoan, EntitySector.Banks, ledger.banks.iterator.map(_.consumerLoan).sumPln),
        (AssetType.ConsumerLoan, EntitySector.Households, -ledger.households.iterator.map(_.consumerLoan).sumPln),
        (AssetType.MortgageLoan, EntitySector.Households, -ledger.households.iterator.map(_.mortgageLoan).sumPln),
        // Government and corporate bond circuits.
        (AssetType.GovBondHTM, EntitySector.Banks, bonds.bankHoldings),
        (AssetType.GovBondHTM, EntitySector.Government, -bonds.outstanding),
        (AssetType.GovBondHTM, EntitySector.NBP, bonds.nbpHoldings),
        (AssetType.GovBondHTM, EntitySector.Insurance, bonds.insuranceHoldings),
        (AssetType.GovBondHTM, EntitySector.Funds, bonds.ppkHoldings + bonds.tfiHoldings),
        (AssetType.GovBondHTM, EntitySector.Foreign, bonds.foreignHoldings),
        (AssetType.QuasiFiscalBond, EntitySector.Banks, ledger.funds.quasiFiscal.bankHoldings),
        (AssetType.QuasiFiscalBond, EntitySector.NBP, ledger.funds.quasiFiscal.nbpHoldings),
        (AssetType.QuasiFiscalBond, EntitySector.Funds, -ledger.funds.quasiFiscal.bondsOutstanding),
        (AssetType.CorpBond, EntitySector.Firms, -corp.outstanding),
        (AssetType.CorpBond, EntitySector.Banks, corp.bankHoldings),
        (AssetType.CorpBond, EntitySector.Insurance, corp.insuranceHoldings),
        (AssetType.CorpBond, EntitySector.Funds, corp.ppkHoldings + corp.otherHoldings + corp.nbfiHoldings),
        // Central-bank, equity, insurance, NBFI, public fund, and diagnostic stocks.
        (AssetType.Reserve, EntitySector.Banks, ledger.banks.iterator.map(_.reserve).sumPln),
        (AssetType.InterbankLoan, EntitySector.Banks, ledger.banks.iterator.map(_.interbankLoan).sumPln),
        (AssetType.Equity, EntitySector.Households, ledger.households.iterator.map(_.equity).sumPln),
        (AssetType.Equity, EntitySector.Firms, -ledger.firms.iterator.map(_.equity).sumPln),
        (AssetType.Equity, EntitySector.Insurance, ledger.insurance.equityHoldings),
        (AssetType.Equity, EntitySector.Funds, ledger.funds.nbfi.equityHoldings),
        (AssetType.LifeReserve, EntitySector.Insurance, -ledger.insurance.lifeReserve),
        (AssetType.NonLifeReserve, EntitySector.Insurance, -ledger.insurance.nonLifeReserve),
        (AssetType.TfiUnit, EntitySector.Funds, -ledger.funds.nbfi.tfiUnit),
        (AssetType.NbfiLoan, EntitySector.Funds, ledger.funds.nbfi.nbfiLoanStock + ledger.funds.quasiFiscal.loanPortfolio),
        (AssetType.Cash, EntitySector.Firms, ledger.firms.iterator.map(_.cash).sumPln),
        (
          AssetType.Cash,
          EntitySector.Funds,
          ledger.funds.zusCash + ledger.funds.nfzCash + ledger.funds.fpCash + ledger.funds.pfronCash +
            ledger.funds.fgspCash + ledger.funds.jstCash + ledger.funds.nbfi.cashHoldings,
        ),
        (AssetType.Capital, EntitySector.Banks, -bankCapital),
        (AssetType.ForeignAsset, EntitySector.NBP, ledger.nbp.foreignAssets),
      )

      val rawCells = entries.foldLeft(Map.empty[(AssetType, EntitySector), Long].withDefaultValue(0L)):
        case (acc, (asset, sector, amount)) =>
          val key = (asset, sector)
          acc.updated(key, acc(key) + amount.toLong)

      val rows = SfcMatrixRegistry.instruments.map: instrument =>
        val cells = SfcMatrixRegistry.sectors
          .map(_.sector)
          .map(sector => sector -> rawCells((instrument.asset, sector)))
          .toMap
          .filter(_._2 != 0L)
        val gaps  = gapsForBsmRow(snapshotKind, instrument, cells)
        BsmRow(instrument.asset, cells, gaps)

      BsmEvidence(snapshotKind, rows, rows.flatMap(_.gaps))

    private def gapsForBsmRow(
        snapshotKind: SnapshotKind,
        instrument: SfcMatrixRegistry.InstrumentMetadata,
        cells: Map[EntitySector, Long],
    ): Vector[MatrixGap] =
      val matrix = s"BSM ${snapshotKind.toString.toLowerCase}"
      instrument.completeness match
        case SfcMatrixRegistry.RowCompleteness.Complete      => Vector.empty
        case SfcMatrixRegistry.RowCompleteness.ClassifiedGap =>
          Vector(MatrixGap(matrix, instrument.asset.toString, None, GapKind.IncompleteCoverage, instrument.note))
        case SfcMatrixRegistry.RowCompleteness.Excluded      =>
          val kind =
            if instrument.category == SfcMatrixRegistry.InstrumentCategory.UnsupportedDiagnostic then GapKind.UnsupportedDiagnostic
            else GapKind.ExplicitlyExcluded
          Option.when(cells.valuesIterator.exists(_ != 0L))(MatrixGap(matrix, instrument.asset.toString, None, kind, instrument.note)).toVector

  object TfmEvidence:
    def fromStep(step: FlowSimulation.StepOutput): TfmEvidence =
      fromBatches(step.flows)

    def fromBatches(batches: Vector[BatchedFlow]): TfmEvidence =
      final case class Acc(
          cells: Map[(MechanismId, AssetType, EntitySector), Long],
          contributors: Map[(MechanismId, AssetType), Vector[BatchContribution]],
          omittedZeroBatches: Int,
      )

      def addCell(
          cells: Map[(MechanismId, AssetType, EntitySector), Long],
          mechanism: MechanismId,
          asset: AssetType,
          sector: EntitySector,
          delta: Long,
      ): Map[(MechanismId, AssetType, EntitySector), Long] =
        val key = (mechanism, asset, sector)
        cells.updated(key, cells.getOrElse(key, 0L) + delta)

      val emptyContributors =
        Map.empty[(MechanismId, AssetType), Vector[BatchContribution]].withDefaultValue(Vector.empty)
      val acc               = batches.zipWithIndex.foldLeft(Acc(Map.empty, emptyContributors, 0)):
        case (state, (batch, index)) =>
          val nonZeroAmounts = amounts(batch).filter(_ != 0L)
          if nonZeroAmounts.isEmpty then state.copy(omittedZeroBatches = state.omittedZeroBatches + 1)
          else
            val total = nonZeroAmounts.sum
            val cells = batch match
              case scatter: BatchedFlow.Scatter     =>
                scatter.amounts.indices.foldLeft(state.cells):
                  case (accCells, senderIndex) =>
                    val amount = scatter.amounts(senderIndex)
                    if amount == 0L then accCells
                    else
                      addCell(
                        addCell(accCells, scatter.mechanism, scatter.asset, scatter.from, -amount),
                        scatter.mechanism,
                        scatter.asset,
                        scatter.to,
                        amount,
                      )
              case broadcast: BatchedFlow.Broadcast =>
                broadcast.amounts.indices.foldLeft(
                  addCell(state.cells, broadcast.mechanism, broadcast.asset, broadcast.from, -total),
                ):
                  case (accCells, targetPosition) =>
                    val amount = broadcast.amounts(targetPosition)
                    if amount == 0L then accCells
                    else addCell(accCells, broadcast.mechanism, broadcast.asset, broadcast.to, amount)

            val key          = (batch.mechanism, batch.asset)
            val contribution = BatchContribution(
              batchIndex = index,
              batchKind = batch.getClass.getSimpleName.stripSuffix("$"),
              from = batch.from,
              to = batch.to,
              amountRaw = total,
              legs = nonZeroAmounts.size,
            )
            state.copy(
              cells = cells,
              contributors = state.contributors.updated(key, state.contributors(key) :+ contribution),
            )

      val registrySectors           = SfcMatrixRegistry.sectors.map(_.sector)
      val registrySectorSet         = registrySectors.toSet
      val keys                      = acc.cells.keysIterator.map((mechanism, asset, _) => (mechanism, asset)).toSet.toVector
      val candidateRows             = keys
        .map: (mechanism, asset) =>
          val cells = registrySectors
            .map(sector => sector -> acc.cells.getOrElse((mechanism, asset, sector), 0L))
            .toMap
            .filter(_._2 != 0L)
          TfmRow(mechanism, asset, cells, acc.contributors((mechanism, asset)))
      val (keptRows, droppedRows)   = candidateRows.partition(_.cells.nonEmpty)
      val rowOrdering               = (row: TfmRow) => (SfcMatrixRegistry.mechanismOrder(row.mechanism), SfcMatrixRegistry.instrumentOrder(row.asset))
      val droppedNonRegistrySectors = acc.cells.keysIterator
        .map((_, _, sector) => sector)
        .filterNot(registrySectorSet)
        .toSet
        .toVector
        .sortBy(_.ordinal)
      val rows                      = keptRows.sortBy(rowOrdering)

      TfmEvidence(
        rows = rows,
        omittedZeroBatches = acc.omittedZeroBatches,
        omittedZeroRows = droppedRows.size,
        droppedRows = droppedRows.sortBy(rowOrdering),
        droppedNonRegistrySectors = droppedNonRegistrySectors,
      )

    private def amounts(batch: BatchedFlow): Vector[Long] =
      batch match
        case scatter: BatchedFlow.Scatter     => scatter.amounts.toVector
        case broadcast: BatchedFlow.Broadcast => broadcast.amounts.toVector

  object OtherChangesEvidence:
    def from(opening: BsmEvidence, closing: BsmEvidence, tfm: TfmEvidence): OtherChangesEvidence =
      val txByAssetSector = tfm.assetSectorTotals.withDefaultValue(0L)
      val cells           = for
        instrument <- SfcMatrixRegistry.instruments
        sector     <- SfcMatrixRegistry.sectors.map(_.sector)
      yield
        val openingRaw     = opening.row(instrument.asset).amountRaw(sector)
        val closingRaw     = closing.row(instrument.asset).amountRaw(sector)
        val stockDelta     = closingRaw - openingRaw
        val txDelta        = txByAssetSector((instrument.asset, sector))
        val other          = stockDelta - txDelta
        val (kind, reason) = classifyOtherChange(instrument, other)
        OtherChangeCell(instrument.asset, sector, stockDelta, txDelta, other, kind, reason)

      val gaps = (opening.gaps ++ closing.gaps).distinct
      OtherChangesEvidence(cells, gaps)

    private def classifyOtherChange(
        instrument: SfcMatrixRegistry.InstrumentMetadata,
        otherChangeRaw: Long,
    ): (OtherChangeKind, String) =
      if otherChangeRaw == 0L then (OtherChangeKind.TransactionReconciliation, "Stock delta is explained by executed transaction flows.")
      else
        instrument.asset match
          case AssetType.ForeignAsset | AssetType.GovBondAFS | AssetType.GovBondHTM                                           =>
            (OtherChangeKind.Revaluation, "Residual is classified as valuation or mark-to-market evidence for this instrument family.")
          case AssetType.FirmLoan | AssetType.ConsumerLoan | AssetType.MortgageLoan | AssetType.CorpBond | AssetType.NbfiLoan =>
            (
              OtherChangeKind.DefaultOrWriteOff,
              "Residual is classified as default, write-off, amortization coverage, or unsupported borrower-side stock evidence.",
            )
          case _                                                                                                              =>
            instrument.completeness match
              case SfcMatrixRegistry.RowCompleteness.Complete =>
                (OtherChangeKind.TransactionReconciliation, "Residual is a complete-row stock-flow reconciliation difference.")
              case _                                          =>
                (OtherChangeKind.CoverageGap, instrument.note)

  object StockFlowReconciliationEvidence:
    def fromStep(step: FlowSimulation.StepOutput)(using SimParams): StockFlowReconciliationEvidence =
      val prev  = Sfc.RuntimeState(
        step.stateIn.world,
        step.stateIn.firms,
        step.stateIn.households,
        step.stateIn.banks,
        step.stateIn.ledgerFinancialState,
      )
      val curr  = Sfc.RuntimeState(
        step.nextState.world,
        step.nextState.firms,
        step.nextState.households,
        step.nextState.banks,
        step.nextState.ledgerFinancialState,
      )
      val flows = step.semanticFlows.copy(fofResidual = PLN.Zero)
      val rows  = Sfc
        .stockFlowReconciliationRows(prev, curr, flows)
        .map: row =>
          val rowMetadata = metadata(row.identity)
          StockFlowReconciliationCell(
            identity = row.identity,
            label = rowMetadata.label,
            expectedRaw = row.expected.toLong,
            actualRaw = row.actual.toLong,
            residualRaw = row.residual.toLong,
            status = if row.isValid then "pass" else "fail",
            source = rowMetadata.source,
            assets = rowMetadata.assets,
            mechanisms = rowMetadata.mechanisms,
            note = rowMetadata.note,
          )

      StockFlowReconciliationEvidence(rows)

    private def metadata(identity: Sfc.SfcIdentity): ReconciliationRowMetadata =
      import Sfc.SfcIdentity.*

      identity match
        case BankCapital                 =>
          rowMetadata(
            "Bank capital",
            "Actual delta from bank capital stocks; expected delta from bank P&L, default/write-off, valuation, provision, and capital-destruction channels.",
            Vector(AssetType.Capital),
            Vector(
              FlowMechanism.BankFirmInterest,
              FlowMechanism.HhDebtService,
              FlowMechanism.BankGovBondIncome,
              FlowMechanism.MortgageInterest,
              FlowMechanism.HhCcDebtService,
              FlowMechanism.BankCorpBondCoupon,
              FlowMechanism.HhDepositInterest,
              FlowMechanism.BankReserveInterest,
              FlowMechanism.BankStandingFacility,
              FlowMechanism.BankInterbankInterest,
              FlowMechanism.BankNplLoss,
              FlowMechanism.BankMortgageNplLoss,
              FlowMechanism.BankCcNplLoss,
              FlowMechanism.BankCorpBondLoss,
              FlowMechanism.BankBfgLevy,
              FlowMechanism.BankUnrealizedLoss,
            ),
            "Includes non-batch htmRealizedLoss, eclProvisionChange, and bankCapitalDestruction from BankingEconomics.",
          )
        case BankDeposits                =>
          rowMetadata(
            "Bank deposits",
            "Actual delta from aggregate bank deposit stocks; expected delta from executed income, spending, loan, bail-in, insurance, fund, and quasi-fiscal deposit channels.",
            Vector(AssetType.DemandDeposit, AssetType.TermDeposit),
            Vector(
              FlowMechanism.HhTotalIncome,
              FlowMechanism.HhConsumption,
              FlowMechanism.InvestmentDepositSettlement,
              FlowMechanism.JstRevenue,
              FlowMechanism.JstSpending,
              FlowMechanism.EquityDomDividend,
              FlowMechanism.EquityForDividend,
              FlowMechanism.HhRemittance,
              FlowMechanism.DiasporaInflow,
              FlowMechanism.TourismExport,
              FlowMechanism.TourismImport,
              FlowMechanism.BankBailIn,
              FlowMechanism.FirmNewLoan,
              FlowMechanism.FirmLoanRepayment,
              FlowMechanism.HhCcOrigination,
              FlowMechanism.InsLifePremium,
              FlowMechanism.InsNonLifePremium,
              FlowMechanism.InsLifeClaim,
              FlowMechanism.InsNonLifeClaim,
              FlowMechanism.TfiDepositDrain,
              FlowMechanism.QuasiFiscalLendingDeposit,
              FlowMechanism.QuasiFiscalRepaymentDeposit,
            ),
            "Deposit stock reconciliation is not computed as a residual; every expected component is sourced from the semantic flow surface.",
          )
        case Nfa                         =>
          rowMetadata(
            "Net foreign assets",
            "Actual delta from BoP NFA stock; expected delta from current account plus the independent FX valuation effect.",
            Vector(AssetType.ForeignAsset),
            Vector(
              FlowMechanism.TradeExports,
              FlowMechanism.TradeImports,
              FlowMechanism.TourismExport,
              FlowMechanism.TourismImport,
              FlowMechanism.PrimaryIncome,
              FlowMechanism.EuFunds,
              FlowMechanism.DiasporaInflow,
              FlowMechanism.HhRemittance,
              FlowMechanism.CapitalFlight,
            ),
            "The FX valuation leg is OpenEconEconomics.valuationEffect, not stockDelta minus transactions.",
          )
        case BondClearing                =>
          rowMetadata(
            "Government bond clearing",
            "Actual level check: supported holder stocks must equal government bonds outstanding.",
            Vector(AssetType.GovBondHTM, AssetType.GovBondAFS),
            Vector(
              FlowMechanism.GovBondPrimaryMarket,
              FlowMechanism.GovBondForeignPurchase,
              FlowMechanism.NbpQeGovBondPurchase,
              FlowMechanism.InsuranceGovBondPurchase,
              FlowMechanism.TfiGovBondPurchase,
            ),
            "This is a holder/issuer clearing identity, not a valuation residual.",
          )
        case InterbankNetting            =>
          rowMetadata(
            "Interbank netting",
            "Actual level check: interbank positions must net to zero.",
            Vector(AssetType.InterbankLoan),
            Vector(FlowMechanism.BankInterbankInterest),
            "Single-bank runs pass trivially; multi-bank runs expose netting errors.",
          )
        case MortgageStock               =>
          rowMetadata(
            "Mortgage stock",
            "Actual delta from household mortgage stock; expected delta from origination minus principal repayment and defaults.",
            Vector(AssetType.MortgageLoan),
            Vector(FlowMechanism.MortgageOrigination, FlowMechanism.MortgageRepayment, FlowMechanism.MortgageDefault),
            "The current holder side is household-resolved; bank-side mortgage stock remains a documented BSM coverage gap.",
          )
        case FlowOfFunds                 =>
          rowMetadata(
            "Flow of funds",
            "Runtime ledger conservation is checked by Sfc.validate through the executed delta ledger.",
            Vector(AssetType.Cash),
            Vector(FlowMechanism.HhConsumption, FlowMechanism.GovPurchases, FlowMechanism.TradeExports, FlowMechanism.InvestmentDepositSettlement),
            "The old hand-assembled residual is neutralized in production validation to avoid double-counting the ledger conservation check.",
          )
        case ConsumerCredit              =>
          rowMetadata(
            "Consumer credit",
            "Actual delta from consumer loan stocks; expected delta from origination minus debt service and defaults.",
            Vector(AssetType.ConsumerLoan),
            Vector(FlowMechanism.HhCcOrigination, FlowMechanism.HhCcDebtService, FlowMechanism.HhCcDefault),
            "Bank loss recognition is separately included in the bank-capital identity.",
          )
        case CorpBondStock               =>
          rowMetadata(
            "Corporate bond stock",
            "Actual delta from corporate bond issuer stock; expected delta from issuance minus amortization and defaults.",
            Vector(AssetType.CorpBond),
            Vector(FlowMechanism.CorpBondIssuance, FlowMechanism.CorpBondAmortization, FlowMechanism.CorpBondDefault),
            "Holder loss recognition is separately represented through bank and insurance/fund channels.",
          )
        case NbfiCredit                  =>
          rowMetadata(
            "NBFI credit",
            "Actual delta from NBFI loan stock; expected delta from origination minus repayment and defaults.",
            Vector(AssetType.NbfiLoan),
            Vector(FlowMechanism.NbfiOrigination, FlowMechanism.NbfiRepayment, FlowMechanism.NbfiDefault),
            "Quasi-fiscal credit has a separate exact identity.",
          )
        case QuasiFiscalBondStock        =>
          rowMetadata(
            "Quasi-fiscal bond stock",
            "Actual delta from BGK/PFR bonds outstanding; expected delta from issuance minus amortization.",
            Vector(AssetType.QuasiFiscalBond),
            Vector(FlowMechanism.QuasiFiscalBondIssuance, FlowMechanism.QuasiFiscalBondAmortization),
            "NBP absorption and amortization are split out in holder identities.",
          )
        case QuasiFiscalBondClearing     =>
          rowMetadata(
            "Quasi-fiscal bond clearing",
            "Actual level check: commercial-bank plus NBP quasi-fiscal holdings must equal bonds outstanding.",
            Vector(AssetType.QuasiFiscalBond),
            Vector(
              FlowMechanism.QuasiFiscalBondIssuance,
              FlowMechanism.QuasiFiscalNbpAbsorption,
              FlowMechanism.QuasiFiscalBondAmortization,
              FlowMechanism.QuasiFiscalNbpBondAmortization,
            ),
            "This is a holder/issuer clearing identity.",
          )
        case QuasiFiscalBankBondHoldings =>
          rowMetadata(
            "Quasi-fiscal bank bond holdings",
            "Actual delta from commercial-bank quasi-fiscal bond holdings; expected delta from bank issuance minus bank amortization.",
            Vector(AssetType.QuasiFiscalBond),
            Vector(FlowMechanism.QuasiFiscalBondIssuance, FlowMechanism.QuasiFiscalBondAmortization),
            "Bank leg excludes NBP absorption and NBP amortization.",
          )
        case QuasiFiscalNbpBondHoldings  =>
          rowMetadata(
            "Quasi-fiscal NBP bond holdings",
            "Actual delta from NBP quasi-fiscal bond holdings; expected delta from NBP absorption minus NBP amortization.",
            Vector(AssetType.QuasiFiscalBond),
            Vector(FlowMechanism.QuasiFiscalNbpAbsorption, FlowMechanism.QuasiFiscalNbpBondAmortization),
            "This keeps quasi-fiscal monetary financing inspectable as its own channel.",
          )
        case QuasiFiscalCredit           =>
          rowMetadata(
            "Quasi-fiscal credit",
            "Actual delta from BGK/PFR subsidized loan portfolio; expected delta from lending minus repayment.",
            Vector(AssetType.NbfiLoan),
            Vector(FlowMechanism.QuasiFiscalLending, FlowMechanism.QuasiFiscalRepayment),
            "Deposit creation/destruction from lending is reconciled in the bank-deposit identity.",
          )
        case other                       =>
          rowMetadata(
            other.toString,
            "Legacy diagnostic identity outside the stock-flow reconciliation artifact.",
            Vector.empty,
            Vector.empty,
            "This identity is not part of the 15 exact stock-flow rows rendered here.",
          )

    private def rowMetadata(
        label: String,
        source: String,
        assets: Vector[AssetType],
        mechanisms: Vector[MechanismId],
        note: String,
    ): ReconciliationRowMetadata =
      ReconciliationRowMetadata(label, source, assets, mechanisms, note)

  object MatrixValidation:
    def validate(
        opening: BsmEvidence,
        closing: BsmEvidence,
        tfm: TfmEvidence,
        sfcResult: Sfc.SfcResult,
    ): MatrixValidationReport =
      val sfcErrors = sfcResult.left.toOption
        .filter(_.nonEmpty)
        .map(MatrixValidationError.SfcValidationFailed.apply)
        .toVector

      val bsmErrors = Vector(opening, closing).flatMap: bsm =>
        bsm.rows.flatMap: row =>
          val instrument = SfcMatrixRegistry.instrument(row.asset)
          if instrument.completeness == SfcMatrixRegistry.RowCompleteness.Complete && row.rowSumRaw != 0L then
            Vector(
              MatrixValidationError.BsmRowSumError(
                bsm.snapshotKind,
                row.asset,
                row.rowSumRaw,
                instrument.note,
              ),
            )
          else Vector.empty

      val tfmErrors = tfm.rows.collect:
        case row if row.rowSumRaw != 0L =>
          MatrixValidationError.TfmRowSumError(row.rowKey, row.mechanism, row.asset, row.rowSumRaw)

      val registryErrors =
        if tfm.droppedNonRegistrySectors.nonEmpty then Vector(MatrixValidationError.TfmNonRegistrySectorError(tfm.droppedNonRegistrySectors))
        else Vector.empty

      MatrixValidationReport(sfcErrors ++ bsmErrors ++ tfmErrors ++ registryErrors)

  object MatrixEvidenceBundle:
    def fromStep(seed: Long, step: FlowSimulation.StepOutput, commit: String = BuildInfo.gitCommit)(using SimParams): MatrixEvidenceBundle =
      val opening        = BsmEvidence.fromStepOpening(step)
      val closing        = BsmEvidence.fromStepClosing(step)
      val tfm            = TfmEvidence.fromStep(step)
      val other          = OtherChangesEvidence.from(opening, closing, tfm)
      val reconciliation = StockFlowReconciliationEvidence.fromStep(step)
      val validation     = MatrixValidation.validate(opening, closing, tfm, step.sfcResult)
      val sfcStatus      = if step.sfcResult.isRight then "pass" else "fail"
      val matrixStatus   = if validation.isValid then "pass" else "fail"
      val metadata       = MatrixMetadata(
        seed = seed,
        executionMonth = step.executionMonth.toInt,
        commit = commit,
        schemaVersion = SfcMatrixRegistry.SchemaVersion,
        sfcStatus = sfcStatus,
        matrixStatus = matrixStatus,
      )
      MatrixEvidenceBundle(metadata, opening, closing, tfm, other, reconciliation, validation)

end SfcMatrixEvidence
