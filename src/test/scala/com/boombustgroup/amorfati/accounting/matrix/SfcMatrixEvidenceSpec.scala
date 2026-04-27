package com.boombustgroup.amorfati.accounting.matrix

import com.boombustgroup.amorfati.accounting.matrix.SfcMatrixEvidence.*
import com.boombustgroup.amorfati.accounting.Sfc
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.flows.FlowMechanism
import com.boombustgroup.ledger.{AssetType, BatchedFlow, EntitySector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SfcMatrixEvidenceSpec extends AnyFlatSpec with Matchers:

  private given SimParams = SimParams.defaults

  private lazy val step =
    SfcMatrixEvidenceTestSupport.deterministicStep()

  private lazy val bundle =
    SfcMatrixEvidenceTestSupport.bundleFrom(step, seed = SfcMatrixEvidenceTestSupport.EvidenceSeed, commit = "test")

  "BsmEvidence" should "derive opening and closing matrices from ledger-owned state" in {
    val closing = bundle.closingBsm

    closing.row(AssetType.DemandDeposit).amountRaw(EntitySector.Households) should not be 0L
    closing.row(AssetType.FirmLoan).amountRaw(EntitySector.Banks) should not be 0L
    closing.row(AssetType.GovBondHTM).amountRaw(EntitySector.Government) should be < 0L
    closing.row(AssetType.CorpBond).amountRaw(EntitySector.Funds) should not be 0L
    closing.row(AssetType.ForeignAsset).amountRaw(EntitySector.NBP) should not be 0L
    closing.row(AssetType.Cash).amountRaw(EntitySector.Funds) should not be 0L
  }

  it should "validate complete financial-claim rows to zero for deterministic evidence" in {
    val completeAssets = SfcMatrixRegistry.instruments
      .filter(_.completeness == SfcMatrixRegistry.RowCompleteness.Complete)
      .map(_.asset)

    for
      bsm   <- Vector(bundle.openingBsm, bundle.closingBsm)
      asset <- completeAssets
    do
      withClue(s"${bsm.snapshotKind} $asset") {
        bsm.row(asset).rowSumRaw shouldBe 0L
      }
  }

  "TfmEvidence" should "derive transaction rows from executed batches and reconcile sector totals to the delta ledger" in {
    val tfm = bundle.tfm

    tfm.rows should not be empty
    all(tfm.rows.map(_.rowSumRaw)) shouldBe 0L
    tfm.rows.exists(_.contributors.nonEmpty) shouldBe true

    val expectedSectorTotals = EntitySector.values.toVector.map: sector =>
      sector -> step.execution.deltaLedger.iterator.collect { case ((`sector`, _, _), raw) =>
        raw
      }.sum

    tfm.sectorTotals shouldBe expectedSectorTotals.toMap
  }

  it should "count all-zero batches separately from omitted transaction rows" in {
    val zero    = BatchedFlow.Broadcast(
      from = EntitySector.Government,
      fromIndex = 0,
      to = EntitySector.Firms,
      amounts = Array(0L),
      targetIndices = Array(0),
      asset = AssetType.Cash,
      mechanism = FlowMechanism.GovPurchases,
    )
    val nonZero = zero.copy(amounts = Array(10L))

    val evidence = TfmEvidence.fromBatches(Vector(zero, nonZero))

    evidence.omittedZeroBatches shouldBe 1
    evidence.omittedZeroRows shouldBe 0
    evidence.droppedRows shouldBe empty
    evidence.rows should have size 1
    evidence.rows.head.rowSumRaw shouldBe 0L
  }

  it should "retain provenance for rows that net to zero after aggregation" in {
    val govToFirm = BatchedFlow.Broadcast(
      from = EntitySector.Government,
      fromIndex = 0,
      to = EntitySector.Firms,
      amounts = Array(10L),
      targetIndices = Array(0),
      asset = AssetType.Cash,
      mechanism = FlowMechanism.GovPurchases,
    )
    val firmToGov = govToFirm.copy(
      from = EntitySector.Firms,
      to = EntitySector.Government,
    )

    val evidence = TfmEvidence.fromBatches(Vector(govToFirm, firmToGov))

    evidence.omittedZeroBatches shouldBe 0
    evidence.omittedZeroRows shouldBe 1
    evidence.rows shouldBe empty
    evidence.droppedRows should have size 1
    evidence.droppedRows.head.contributors should have size 2
    evidence.droppedRows.head.cells shouldBe empty
  }

  "MatrixValidation" should "include SFC status and pass for deterministic evidence" in {
    bundle.metadata.sfcStatus shouldBe "pass"
    bundle.metadata.matrixStatus shouldBe "pass"
    bundle.validation.isValid shouldBe true
  }

  "StockFlowReconciliationEvidence" should "render exact identity rows from independent semantic flow channels" in {
    val reconciliation = bundle.reconciliation

    reconciliation.rows.map(_.identity).toSet should contain(Sfc.SfcIdentity.Nfa)
    reconciliation.rows.map(_.identity).toSet should contain(Sfc.SfcIdentity.BankCapital)
    reconciliation.failures shouldBe empty

    val nfa = reconciliation.rows.find(_.identity == Sfc.SfcIdentity.Nfa).get
    nfa.source should include("current account")
    nfa.note should include("valuationEffect")
  }

  "MatrixValidation" should "report actionable BSM and TFM perturbations" in {
    val openingIndex = bundle.openingBsm.rows.indexWhere(_.asset == AssetType.GovBondHTM)
    val openingRow   = bundle.openingBsm.rows(openingIndex)
    val badOpening   = bundle.openingBsm.copy(
      rows = bundle.openingBsm.rows.updated(
        openingIndex,
        openingRow.copy(cells = openingRow.cells.updated(EntitySector.Banks, openingRow.amountRaw(EntitySector.Banks) + 1L)),
      ),
    )
    val bsmReport    = MatrixValidation.validate(badOpening, bundle.closingBsm, bundle.tfm, Right(()))
    bsmReport.errors.exists(_.isInstanceOf[MatrixValidationError.BsmRowSumError]) shouldBe true

    val tfmRow    = bundle.tfm.rows.head
    val badTfmRow = tfmRow.copy(cells = tfmRow.cells.updated(EntitySector.Households, tfmRow.amountRaw(EntitySector.Households) + 1L))
    val badTfm    = bundle.tfm.copy(rows = bundle.tfm.rows.updated(0, badTfmRow))
    val tfmReport = MatrixValidation.validate(bundle.openingBsm, bundle.closingBsm, badTfm, Right(()))
    tfmReport.errors.exists(_.isInstanceOf[MatrixValidationError.TfmRowSumError]) shouldBe true

    val nonRegistryTfm    = bundle.tfm.copy(droppedNonRegistrySectors = Vector(EntitySector.Foreign))
    val nonRegistryReport = MatrixValidation.validate(bundle.openingBsm, bundle.closingBsm, nonRegistryTfm, Right(()))
    nonRegistryReport.errors.exists(_.isInstanceOf[MatrixValidationError.TfmNonRegistrySectorError]) shouldBe true
  }

end SfcMatrixEvidenceSpec
