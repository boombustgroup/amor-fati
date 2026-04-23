package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.MonthRandomness
import com.boombustgroup.amorfati.engine.ledger.{AssetOwnershipContract, LedgerFinancialState, RuntimeFlowProjection}
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.{AssetType, EntitySector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FlowSimulationRuntimeProjectionSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "FlowSimulation.step" should "materialize public fund cash from executed runtime deltas" in {
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))

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
    val init     = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state    = FlowSimulation.SimState.fromInit(init)
    val topology = RuntimeLedgerTopology.fromState(state)
    val opening  = state.ledgerFinancialState

    val nfzContributions = PLN(10.0)
    val nfzSpending      = PLN(30.0)
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
    val init     = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state    = FlowSimulation.SimState.fromInit(init)
    val topology = RuntimeLedgerTopology.fromState(state)
    val opening  = state.ledgerFinancialState.copy(
      funds = state.ledgerFinancialState.funds.copy(
        quasiFiscal = LedgerFinancialState.QuasiFiscalBalances(
          bondsOutstanding = PLN(1000.0),
          loanPortfolio = PLN(100.0),
          bankHoldings = PLN(700.0),
          nbpHoldings = PLN(300.0),
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
      (EntitySector.Funds, AssetType.QuasiFiscalBond, topology.funds.quasiFiscal) -> PLN(-700.0).toLong,
      (EntitySector.Banks, AssetType.QuasiFiscalBond, 0)                          -> PLN(250.0).toLong,
      (EntitySector.Banks, AssetType.QuasiFiscalBond, 1)                          -> PLN(150.0).toLong,
      (EntitySector.NBP, AssetType.QuasiFiscalBond, topology.nbp.persistedOwner)  -> PLN(300.0).toLong,
      (EntitySector.Funds, AssetType.NbfiLoan, topology.funds.quasiFiscal)        -> PLN(-350.0).toLong,
    )

    val projection = RuntimeFlowProjection.materializeSupportedState(
      opening = opening,
      semanticClosing = semanticClosing,
      deltaLedger = deltaLedger,
      topology = topology,
    )

    projection.quasiFiscal shouldBe RuntimeFlowProjection.QuasiFiscalProjection(
      bondsOutstanding = PLN(1700.0),
      loanPortfolio = PLN(450.0),
      bankHoldings = PLN(1100.0),
      nbpHoldings = PLN(600.0),
    )
    projection.ledgerFinancialState.funds.quasiFiscal shouldBe LedgerFinancialState.QuasiFiscalBalances(
      bondsOutstanding = PLN(1700.0),
      loanPortfolio = PLN(450.0),
      bankHoldings = PLN(1100.0),
      nbpHoldings = PLN(600.0),
    )
    projection.ledgerFinancialState.funds.quasiFiscal should not equal stageOnlyQf
  }

end FlowSimulationRuntimeProjectionSpec
