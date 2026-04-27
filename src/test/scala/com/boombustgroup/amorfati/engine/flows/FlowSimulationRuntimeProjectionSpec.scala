package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.ledger.{AssetOwnershipContract, LedgerFinancialState, RuntimeFlowProjection}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.{AssetType, EntitySector, ImperativeInterpreter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FlowSimulationRuntimeProjectionSpec extends AnyFlatSpec with Matchers:
  import RuntimeFlowsTestSupport.*

  private given p: SimParams = SimParams.defaults

  "FlowSimulation.step" should "materialize public fund cash from executed runtime deltas" in {
    val state  = stateFromSeed()
    val result = stepWithSeed(state)

    RuntimeFlowProjection.MaterializedPublicFundCashSlots.foreach: fundIndex =>
      AssetOwnershipContract.isSupportedPersistedPair(result.execution.topology, EntitySector.Funds, AssetType.Cash, fundIndex) shouldBe true

    val projected = RuntimeFlowProjection.projectPublicFundCash(
      opening = state.ledgerFinancialState,
      deltaLedger = result.execution.deltaLedger,
    )
    val funds     = result.nextState.ledgerFinancialState.funds

    RuntimeFlowProjection.PublicFundCashProjection(
      zusCash = funds.zusCash,
      nfzCash = funds.nfzCash,
      fpCash = funds.fpCash,
      pfronCash = funds.pfronCash,
      fgspCash = funds.fgspCash,
      jstCash = funds.jstCash,
    ) shouldBe projected

    funds.nfzCash shouldBe state.ledgerFinancialState.funds.nfzCash +
      result.trace.executedFlows.nfzContributions +
      result.trace.executedFlows.nfzGovSubvention -
      result.trace.executedFlows.nfzSpending
    funds.jstCash shouldBe state.ledgerFinancialState.funds.jstCash + result.trace.executedFlows.jstDepositChange
  }

  it should "include explicit NFZ gov subvention when materializing a deterministic deficit fixture" in {
    val state    = stateFromSeed()
    val topology = RuntimeLedgerTopology.fromState(state)
    val opening  = state.ledgerFinancialState

    val nfzContributions = PLN(10)
    val nfzSpending      = PLN(30)
    val nfzGovSubvention = nfzSpending - nfzContributions
    nfzGovSubvention should be > PLN.Zero

    val stageOnlyNfzCash = opening.funds.nfzCash + nfzContributions - nfzSpending
    val semanticClosing  = opening.copy(
      funds = opening.funds.copy(nfzCash = stageOnlyNfzCash),
    )
    val executedDelta    = nfzContributions + nfzGovSubvention - nfzSpending

    val projection = RuntimeFlowProjection.materializeSupportedState(
      opening = opening,
      semanticClosing = semanticClosing,
      deltaLedger = Map((EntitySector.Funds, AssetType.Cash, topology.funds.nfz) -> executedDelta.toLong),
      topology = topology,
    )

    projection.publicFundCash.nfzCash shouldBe opening.funds.nfzCash + executedDelta
    projection.ledgerFinancialState.funds.nfzCash shouldBe opening.funds.nfzCash + executedDelta
    projection.ledgerFinancialState.funds.nfzCash should not equal stageOnlyNfzCash
  }

  it should "materialize quasi-fiscal bond and loan stocks from executed runtime deltas" in {
    val state    = stateFromSeed()
    val topology = RuntimeLedgerTopology.fromState(state)
    val opening  = state.ledgerFinancialState.copy(
      funds = state.ledgerFinancialState.funds.copy(
        quasiFiscal = LedgerFinancialState.QuasiFiscalBalances(
          bondsOutstanding = PLN(1000),
          loanPortfolio = PLN(100),
          bankHoldings = PLN(700),
          nbpHoldings = PLN(300),
        ),
      ),
    )

    AssetOwnershipContract.isSupportedPersistedPair(topology, EntitySector.Funds, AssetType.QuasiFiscalBond, topology.funds.quasiFiscal) shouldBe true
    AssetOwnershipContract.isSupportedPersistedPair(topology, EntitySector.Funds, AssetType.NbfiLoan, topology.funds.quasiFiscal) shouldBe true
    AssetOwnershipContract.isSupportedPersistedPair(topology, EntitySector.Banks, AssetType.QuasiFiscalBond, 0) shouldBe true
    AssetOwnershipContract.isSupportedPersistedPair(topology, EntitySector.NBP, AssetType.QuasiFiscalBond, topology.nbp.persistedOwner) shouldBe true

    val stageOnlyQf     = LedgerFinancialState.QuasiFiscalBalances(
      bondsOutstanding = PLN.Zero,
      loanPortfolio = PLN.Zero,
      bankHoldings = PLN.Zero,
      nbpHoldings = PLN.Zero,
    )
    val semanticClosing = opening.copy(funds = opening.funds.copy(quasiFiscal = stageOnlyQf))
    val deltaLedger     = Map(
      (EntitySector.Funds, AssetType.QuasiFiscalBond, topology.funds.quasiFiscal) -> PLN(-700).toLong,
      (EntitySector.Banks, AssetType.QuasiFiscalBond, 0)                          -> PLN(250).toLong,
      (EntitySector.Banks, AssetType.QuasiFiscalBond, 1)                          -> PLN(150).toLong,
      (EntitySector.NBP, AssetType.QuasiFiscalBond, topology.nbp.persistedOwner)  -> PLN(300).toLong,
      (EntitySector.Funds, AssetType.NbfiLoan, topology.funds.quasiFiscal)        -> PLN(-350).toLong,
    )

    val projection = RuntimeFlowProjection.materializeSupportedState(
      opening = opening,
      semanticClosing = semanticClosing,
      deltaLedger = deltaLedger,
      topology = topology,
    )

    projection.quasiFiscal shouldBe RuntimeFlowProjection.QuasiFiscalProjection(
      bondsOutstanding = PLN(1700),
      loanPortfolio = PLN(450),
      bankHoldings = PLN(1100),
      nbpHoldings = PLN(600),
    )
    projection.ledgerFinancialState.funds.quasiFiscal shouldBe LedgerFinancialState.QuasiFiscalBalances(
      bondsOutstanding = PLN(1700),
      loanPortfolio = PLN(450),
      bankHoldings = PLN(1100),
      nbpHoldings = PLN(600),
    )
    projection.ledgerFinancialState.funds.quasiFiscal should not equal stageOnlyQf
  }

  it should "leave household mortgage stocks to semantic closing while runtime principal stays shell-only" in {
    val state    = stateFromSeed()
    val topology = RuntimeLedgerTopology.fromState(state)
    val opening  = state.ledgerFinancialState
    val adjusted = opening.households.head.copy(mortgageLoan = opening.households.head.mortgageLoan + PLN(12345))

    val semanticClosing = opening.copy(households = opening.households.updated(0, adjusted))
    val runtimeState    = topology.emptyExecutionState()
    val batches         = MortgageFlows.emitBatches(MortgageFlows.Input(PLN(500000), PLN(200000), PLN(10000), PLN(50000)))(using topology)

    ImperativeInterpreter.planAndApplyAll(runtimeState, batches) shouldBe Right(())
    val snapshot         = runtimeState.snapshot
    val mortgageShellKey = (EntitySector.Households, AssetType.MortgageLoan, topology.households.mortgagePrincipalSettlement)
    snapshot.keys.exists { case (sector, asset, _) => sector == EntitySector.Banks && asset == AssetType.MortgageLoan } shouldBe false
    snapshot.keySet should contain(mortgageShellKey)
    snapshot(mortgageShellKey) should not be 0L

    val projection = RuntimeFlowProjection.materializeSupportedState(
      opening = opening,
      semanticClosing = semanticClosing,
      deltaLedger = snapshot,
      topology = topology,
    )

    projection.ledgerFinancialState.households shouldBe semanticClosing.households
  }

end FlowSimulationRuntimeProjectionSpec
