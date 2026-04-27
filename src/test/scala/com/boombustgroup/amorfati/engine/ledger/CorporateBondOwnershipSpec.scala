package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.MonthRandomness
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.engine.markets.CorporateBondMarket
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CorporateBondOwnershipSpec extends AnyFlatSpec with Matchers:

  private given SimParams = SimParams.defaults

  private lazy val defaultInit: WorldInit.InitResult =
    WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))

  private lazy val defaultState: FlowSimulation.SimState =
    FlowSimulation.SimState.fromInit(defaultInit)

  private lazy val defaultNextState: FlowSimulation.SimState =
    FlowSimulation.step(defaultState, MonthRandomness.Contract.fromSeed(42L)).nextState

  private def sampleIssuerBalances(init: WorldInit.InitResult) =
    val firmStates = init.firms.take(2)
    val firms      = (0 until firmStates.length).map: index =>
      init.ledgerFinancialState.firms(index).copy(corpBond = PLN.fromLong((index + 1L) * 100L))
    (firmStates, firms.toVector)

  "CorporateBondOwnership" should "initialize issuer liabilities to the market outstanding stock" in {
    val ledger      = defaultState.ledgerFinancialState
    val issuerStock = CorporateBondOwnership.issuerOutstanding(ledger)
    val holderStock = CorporateBondOwnership.holderOutstanding(ledger)

    issuerStock shouldBe holderStock
  }

  it should "initialize insurance and NBFI holder stocks from the corporate bond market only" in {
    val marketStock = CorporateBondMarket.initialStock
    val ledger      = defaultInit.ledgerFinancialState
    val nbfi        = ledger.funds.nbfi

    ledger.insurance.corpBondHoldings shouldBe marketStock.insuranceHoldings
    nbfi.corpBondHoldings shouldBe marketStock.nbfiHoldings
    nbfi.cashHoldings shouldBe (nbfi.tfiUnit - nbfi.govBondHoldings - nbfi.corpBondHoldings - nbfi.equityHoldings).max(PLN.Zero)
    nbfi.govBondHoldings + nbfi.corpBondHoldings + nbfi.equityHoldings + nbfi.cashHoldings shouldBe nbfi.tfiUnit
  }

  it should "keep insurance and NBFI holder buckets ledger-owned after a simulation step" in {
    val nextState = defaultNextState
    val ledger    = nextState.ledgerFinancialState
    val stock     = CorporateBondOwnership.stockStateFromLedger(ledger)
    val nbfi      = ledger.funds.nbfi

    stock.insuranceHoldings shouldBe ledger.insurance.corpBondHoldings
    stock.nbfiHoldings shouldBe nbfi.corpBondHoldings
    nbfi.cashHoldings shouldBe (nbfi.tfiUnit - nbfi.govBondHoldings - nbfi.corpBondHoldings - nbfi.equityHoldings).max(PLN.Zero)
  }

  it should "project stock from ledger issuer and holder balances" in {
    val state          = LedgerTestFixtures.simState()
    val projectedStock = CorporateBondOwnership.stockStateFromLedger(state.ledgerFinancialState)

    projectedStock.outstanding shouldBe CorporateBondOwnership.issuerOutstanding(state.ledgerFinancialState)
    projectedStock.holderTotal shouldBe CorporateBondOwnership.holderOutstanding(state.ledgerFinancialState)
  }

  it should "amortize issuer liabilities pro rata and exactly at aggregate level" in {
    val (firmStates, firms) = sampleIssuerBalances(defaultInit)
    val amortization        = PLN.fromLong(60)
    val settled             = CorporateBondOwnership.applyAmortization(firms, firmStates, amortization)

    CorporateBondOwnership.issuerOutstanding(settled) shouldBe CorporateBondOwnership.issuerOutstanding(firms) - amortization
    settled.head.corpBond should be < firms.head.corpBond
    settled(1).corpBond should be < firms(1).corpBond
  }

  it should "clear defaulted issuer liabilities by firm id" in {
    val (firmStates, firms) = sampleIssuerBalances(defaultInit)
    val cleared             = CorporateBondOwnership.clearDefaultedIssuerDebt(firms, Set(firmStates.head.id))

    cleared.head.corpBond shouldBe PLN.Zero
    cleared(1).corpBond shouldBe firms(1).corpBond
  }
