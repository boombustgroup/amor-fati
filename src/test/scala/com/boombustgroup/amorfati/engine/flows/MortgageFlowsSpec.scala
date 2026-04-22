package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.engine.ledger.{LedgerFinancialState, RuntimeFlowProjection}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MortgageFlowsSpec extends AnyFlatSpec with Matchers:

  "MortgageFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = MortgageFlows.emit(MortgageFlows.Input(PLN(5000000.0), PLN(2000000.0), PLN(1500000.0), PLN(300000.0)))
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "keep the flat mortgage book balance inside household-local accounts" in {
    val input    = MortgageFlows.Input(PLN(5000000.0), PLN(2000000.0), PLN(1500000.0), PLN(300000.0))
    val flows    = MortgageFlows.emit(input)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    balances(MortgageFlows.MORTGAGE_BOOK_ACCOUNT) shouldBe (input.principalRepayment + input.interest + input.defaultAmount - input.origination).toLong
  }

  it should "preserve SFC across 120 months" in {
    val input    = MortgageFlows.Input(PLN(5000000.0), PLN(2000000.0), PLN(1500000.0), PLN(300000.0))
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      balances = Interpreter.applyAll(balances, MortgageFlows.emit(input))
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }

  it should "keep mortgage-loan batches inside household borrower accounts" in {
    given RuntimeLedgerTopology = RuntimeLedgerTopology.nonZeroPopulation

    val input = MortgageFlows.Input(
      origination = PLN.fromRaw(30L),
      principalRepayment = PLN.fromRaw(13L),
      interest = PLN.fromRaw(5L),
      defaultAmount = PLN.fromRaw(7L),
      householdMortgageBalances = Vector(PLN.fromRaw(60L), PLN.fromRaw(40L), PLN.Zero),
    )

    val batches         = MortgageFlows.emitBatches(input)
    val mortgageBatches = batches.filter(_.asset == AssetType.MortgageLoan)

    mortgageBatches should have size 3
    all(mortgageBatches.map(_.from)) shouldBe EntitySector.Households
    all(mortgageBatches.map(_.to)) shouldBe EntitySector.Households
    mortgageBatches.exists(batch => batch.from == EntitySector.Banks || batch.to == EntitySector.Banks) shouldBe false

    val origination          = mortgageBatches.find(_.mechanism == FlowMechanism.MortgageOrigination).get
    origination shouldBe a[BatchedFlow.Broadcast]
    val originationBroadcast = origination.asInstanceOf[BatchedFlow.Broadcast]
    originationBroadcast.fromIndex shouldBe summon[RuntimeLedgerTopology].households.aggregate
    originationBroadcast.targetIndices.toVector shouldBe Vector(0, 1, 2)
    originationBroadcast.amounts.toVector shouldBe Vector(18L, 12L, 0L)

    val repayment        = mortgageBatches.find(_.mechanism == FlowMechanism.MortgageRepayment).get
    repayment shouldBe a[BatchedFlow.Scatter]
    val repaymentScatter = repayment.asInstanceOf[BatchedFlow.Scatter]
    repaymentScatter.targetIndices.toVector.distinct shouldBe Vector(summon[RuntimeLedgerTopology].households.aggregate)
    repaymentScatter.amounts.toVector shouldBe Vector(7L, 6L, 0L) ++ Vector.fill(
      summon[RuntimeLedgerTopology].households.sectorSize - summon[RuntimeLedgerTopology].households.persistedCount,
    )(0L)

    val defaultBatch   = mortgageBatches.find(_.mechanism == FlowMechanism.MortgageDefault).get
    defaultBatch shouldBe a[BatchedFlow.Scatter]
    val defaultScatter = defaultBatch.asInstanceOf[BatchedFlow.Scatter]
    defaultScatter.targetIndices.toVector.distinct shouldBe Vector(summon[RuntimeLedgerTopology].households.aggregate)
    defaultScatter.amounts.toVector shouldBe Vector(4L, 3L, 0L) ++ Vector.fill(
      summon[RuntimeLedgerTopology].households.sectorSize - summon[RuntimeLedgerTopology].households.persistedCount,
    )(0L)
  }

  it should "fall back to the household aggregate shell when there are no active mortgage borrowers" in {
    given topology: RuntimeLedgerTopology = RuntimeLedgerTopology.nonZeroPopulation

    val input = MortgageFlows.Input(
      origination = PLN.fromRaw(30L),
      principalRepayment = PLN.fromRaw(13L),
      interest = PLN.Zero,
      defaultAmount = PLN.fromRaw(7L),
      householdMortgageBalances = Vector.fill(topology.households.persistedCount)(PLN.Zero),
    )

    val mortgageBatches = MortgageFlows.emitBatches(input).filter(_.asset == AssetType.MortgageLoan)

    mortgageBatches should have size 3
    mortgageBatches.foreach:
      case broadcast: BatchedFlow.Broadcast =>
        broadcast.from shouldBe EntitySector.Households
        broadcast.fromIndex shouldBe topology.households.aggregate
        broadcast.to shouldBe EntitySector.Households
        broadcast.targetIndices.toVector shouldBe Vector(topology.households.aggregate)
      case other                            => fail(s"Expected aggregate shell Broadcast, got $other")

    mortgageBatches.find(_.mechanism == FlowMechanism.MortgageOrigination).map(RuntimeLedgerTopology.totalTransferred) shouldBe Some(30L)
    mortgageBatches.find(_.mechanism == FlowMechanism.MortgageRepayment).map(RuntimeLedgerTopology.totalTransferred) shouldBe Some(13L)
    mortgageBatches.find(_.mechanism == FlowMechanism.MortgageDefault).map(RuntimeLedgerTopology.totalTransferred) shouldBe Some(7L)
  }

  it should "materialize target closing mortgage balances from executed gross mechanism deltas" in {
    given topology: RuntimeLedgerTopology = RuntimeLedgerTopology.nonZeroPopulation

    val input = MortgageFlows.Input(
      origination = PLN.fromRaw(30L),
      principalRepayment = PLN.fromRaw(13L),
      interest = PLN.Zero,
      defaultAmount = PLN.fromRaw(7L),
      householdMortgageBalances = Vector(PLN.fromRaw(60L), PLN.fromRaw(40L), PLN.Zero),
      targetHouseholdMortgageBalances = Vector(PLN.fromRaw(65L), PLN.fromRaw(45L), PLN.Zero),
    )

    val executionState = topology.emptyExecutionState()
    val batches        = MortgageFlows.emitBatches(input)

    batches.find(_.mechanism == FlowMechanism.MortgageOrigination).map(RuntimeLedgerTopology.totalTransferred) shouldBe Some(30L)
    batches.find(_.mechanism == FlowMechanism.MortgageRepayment).map(RuntimeLedgerTopology.totalTransferred) shouldBe Some(13L)
    batches.find(_.mechanism == FlowMechanism.MortgageDefault).map(RuntimeLedgerTopology.totalTransferred) shouldBe Some(7L)

    ImperativeInterpreter.planAndApplyAll(executionState, batches) shouldBe Right(())

    val opening         = projectionFixture(
      Vector(
        LedgerFinancialState.HouseholdBalances(PLN.Zero, PLN.fromRaw(60L), PLN.Zero, PLN.Zero),
        LedgerFinancialState.HouseholdBalances(PLN.Zero, PLN.fromRaw(40L), PLN.Zero, PLN.Zero),
        LedgerFinancialState.HouseholdBalances(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      ),
    )
    val semanticClosing = opening.copy(
      households = Vector(
        LedgerFinancialState.HouseholdBalances(PLN.fromRaw(1L), PLN.fromRaw(65L), PLN.Zero, PLN.Zero),
        LedgerFinancialState.HouseholdBalances(PLN.fromRaw(2L), PLN.fromRaw(45L), PLN.Zero, PLN.Zero),
        LedgerFinancialState.HouseholdBalances(PLN.fromRaw(3L), PLN.Zero, PLN.Zero, PLN.Zero),
      ),
    )

    val projection = RuntimeFlowProjection.materializeSupportedState(
      opening = opening,
      semanticClosing = semanticClosing,
      deltaLedger = executionState.snapshot,
      topology = topology,
    )

    projection.ledgerFinancialState.households.map(_.mortgageLoan).take(3) shouldBe Vector(PLN.fromRaw(65L), PLN.fromRaw(45L), PLN.Zero)
    projection.ledgerFinancialState.households.map(_.demandDeposit).take(3) shouldBe Vector(PLN.fromRaw(1L), PLN.fromRaw(2L), PLN.fromRaw(3L))
  }

  it should "materialize household mortgage stock from executed runtime deltas" in {
    given topology: RuntimeLedgerTopology = RuntimeLedgerTopology.nonZeroPopulation

    val openingHouseholds = Vector(
      LedgerFinancialState.HouseholdBalances(PLN.Zero, PLN.fromRaw(60L), PLN.Zero, PLN.Zero),
      LedgerFinancialState.HouseholdBalances(PLN.Zero, PLN.fromRaw(40L), PLN.Zero, PLN.Zero),
      LedgerFinancialState.HouseholdBalances(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    )
    val input             = MortgageFlows.Input(
      origination = PLN.fromRaw(30L),
      principalRepayment = PLN.fromRaw(13L),
      interest = PLN.Zero,
      defaultAmount = PLN.fromRaw(7L),
      householdMortgageBalances = openingHouseholds.map(_.mortgageLoan),
    )
    val executionState    = topology.emptyExecutionState()
    val batches           = MortgageFlows.emitBatches(input)

    ImperativeInterpreter.planAndApplyAll(executionState, batches) shouldBe Right(())
    executionState.snapshot.keys.exists((sector, asset, _) => sector == EntitySector.Banks && asset == AssetType.MortgageLoan) shouldBe false

    val opening         = projectionFixture(openingHouseholds)
    val semanticClosing = opening.copy(
      households = LedgerFinancialState.settleHouseholdMortgageStock(openingHouseholds, PLN.fromRaw(110L)),
    )
    val projection      = RuntimeFlowProjection.materializeSupportedState(
      opening = opening,
      semanticClosing = semanticClosing,
      deltaLedger = executionState.snapshot,
      topology = topology,
    )

    projection.ledgerFinancialState.households.map(_.mortgageLoan).take(3) shouldBe Vector(PLN.fromRaw(67L), PLN.fromRaw(43L), PLN.Zero)
    LedgerFinancialState.householdMortgageStock(projection.ledgerFinancialState.households) shouldBe PLN.fromRaw(110L)
  }

  it should "preserve zero-mortgage households born after the runtime topology boundary" in {
    given topology: RuntimeLedgerTopology = RuntimeLedgerTopology.nonZeroPopulation

    val openingHouseholds = Vector(
      LedgerFinancialState.HouseholdBalances(PLN.Zero, PLN.fromRaw(60L), PLN.Zero, PLN.Zero),
      LedgerFinancialState.HouseholdBalances(PLN.Zero, PLN.fromRaw(40L), PLN.Zero, PLN.Zero),
      LedgerFinancialState.HouseholdBalances(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    )
    val opening           = projectionFixture(openingHouseholds)
    val semanticClosing   = opening.copy(
      households = openingHouseholds :+ LedgerFinancialState.HouseholdBalances(PLN.fromRaw(1L), PLN.Zero, PLN.Zero, PLN.Zero),
    )

    val projection = RuntimeFlowProjection.materializeSupportedState(
      opening = opening,
      semanticClosing = semanticClosing,
      deltaLedger = Map.empty,
      topology = topology,
    )

    projection.ledgerFinancialState.households should have size 4
    projection.ledgerFinancialState.households.last.demandDeposit shouldBe PLN.fromRaw(1L)
    projection.ledgerFinancialState.households.last.mortgageLoan shouldBe PLN.Zero
  }

  it should "reject non-zero mortgage balances beyond the runtime topology boundary" in {
    given topology: RuntimeLedgerTopology = RuntimeLedgerTopology.nonZeroPopulation

    val openingHouseholds = Vector(
      LedgerFinancialState.HouseholdBalances(PLN.Zero, PLN.fromRaw(60L), PLN.Zero, PLN.Zero),
      LedgerFinancialState.HouseholdBalances(PLN.Zero, PLN.fromRaw(40L), PLN.Zero, PLN.Zero),
      LedgerFinancialState.HouseholdBalances(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    )
    val opening           = projectionFixture(openingHouseholds)
    val semanticClosing   = opening.copy(
      households = openingHouseholds :+ LedgerFinancialState.HouseholdBalances(PLN.fromRaw(1L), PLN.fromRaw(999L), PLN.Zero, PLN.Zero),
    )

    val error = intercept[IllegalArgumentException] {
      RuntimeFlowProjection.materializeSupportedState(
        opening = opening,
        semanticClosing = semanticClosing,
        deltaLedger = Map.empty,
        topology = topology,
      )
    }

    error.getMessage should include("new household mortgageLoan")
  }

  private def projectionFixture(households: Vector[LedgerFinancialState.HouseholdBalances]): LedgerFinancialState =
    LedgerFinancialState(
      households = households,
      firms = Vector.empty,
      banks = Vector.empty,
      government = LedgerFinancialState.GovernmentBalances(PLN.Zero),
      foreign = LedgerFinancialState.ForeignBalances(PLN.Zero),
      nbp = LedgerFinancialState.NbpBalances(PLN.Zero, PLN.Zero),
      insurance = LedgerFinancialState.InsuranceBalances(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      funds = LedgerFinancialState.FundBalances(
        zusCash = PLN.Zero,
        nfzCash = PLN.Zero,
        ppkGovBondHoldings = PLN.Zero,
        ppkCorpBondHoldings = PLN.Zero,
        fpCash = PLN.Zero,
        pfronCash = PLN.Zero,
        fgspCash = PLN.Zero,
        jstCash = PLN.Zero,
        corpBondOtherHoldings = PLN.Zero,
        nbfi = LedgerFinancialState.NbfiFundBalances(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
        quasiFiscal = LedgerFinancialState.QuasiFiscalBalances(PLN.Zero, PLN.Zero),
      ),
    )
