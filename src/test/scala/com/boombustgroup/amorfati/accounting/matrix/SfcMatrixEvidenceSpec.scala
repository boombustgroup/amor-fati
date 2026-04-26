package com.boombustgroup.amorfati.accounting.matrix

import com.boombustgroup.amorfati.accounting.matrix.SfcMatrixEvidence.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.MonthRandomness
import com.boombustgroup.amorfati.engine.flows.{FlowMechanism, FlowSimulation}
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.ledger.{AssetType, BatchedFlow, EntitySector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SfcMatrixEvidenceSpec extends AnyFlatSpec with Matchers:

  private given SimParams = SimParams.defaults

  private lazy val step =
    val init = WorldInit.initialize(InitRandomness.Contract.fromSeed(1L))
    FlowSimulation.step(FlowSimulation.SimState.fromInit(init), MonthRandomness.Contract.fromSeed(1001L))

  private lazy val bundle =
    MatrixEvidenceBundle.fromStep(seed = 1L, step = step, commit = "test")

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

  it should "omit all-zero transaction rows explicitly" in {
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

    evidence.omittedZeroRows shouldBe 1
    evidence.rows should have size 1
    evidence.rows.head.rowSumRaw shouldBe 0L
  }

  "MatrixValidation" should "include SFC status and pass for deterministic evidence" in {
    bundle.metadata.sfcStatus shouldBe "pass"
    bundle.metadata.matrixStatus shouldBe "pass"
    bundle.validation.isValid shouldBe true
  }

  it should "report actionable BSM, TFM, and stock-flow reconciliation perturbations" in {
    val openingIndex = bundle.openingBsm.rows.indexWhere(_.asset == AssetType.GovBondHTM)
    val openingRow   = bundle.openingBsm.rows(openingIndex)
    val badOpening   = bundle.openingBsm.copy(
      rows = bundle.openingBsm.rows.updated(
        openingIndex,
        openingRow.copy(cells = openingRow.cells.updated(EntitySector.Banks, openingRow.amountRaw(EntitySector.Banks) + 1L)),
      ),
    )
    val bsmReport    = MatrixValidation.validate(badOpening, bundle.closingBsm, bundle.tfm, bundle.otherChanges, Right(()))
    bsmReport.errors.exists(_.isInstanceOf[MatrixValidationError.BsmRowSumError]) shouldBe true

    val tfmRow    = bundle.tfm.rows.head
    val badTfmRow = tfmRow.copy(cells = tfmRow.cells.updated(EntitySector.Households, tfmRow.amountRaw(EntitySector.Households) + 1L))
    val badTfm    = bundle.tfm.copy(rows = bundle.tfm.rows.updated(0, badTfmRow))
    val tfmReport = MatrixValidation.validate(bundle.openingBsm, bundle.closingBsm, badTfm, bundle.otherChanges, Right(()))
    tfmReport.errors.exists(_.isInstanceOf[MatrixValidationError.TfmRowSumError]) shouldBe true

    val otherCell   = bundle.otherChanges.cells.head
    val badOther    = bundle.otherChanges.copy(cells = bundle.otherChanges.cells.updated(0, otherCell.copy(otherChangeRaw = otherCell.otherChangeRaw + 1L)))
    val otherReport = MatrixValidation.validate(bundle.openingBsm, bundle.closingBsm, bundle.tfm, badOther, Right(()))
    otherReport.errors.exists(_.isInstanceOf[MatrixValidationError.StockFlowReconciliationError]) shouldBe true
  }

end SfcMatrixEvidenceSpec
