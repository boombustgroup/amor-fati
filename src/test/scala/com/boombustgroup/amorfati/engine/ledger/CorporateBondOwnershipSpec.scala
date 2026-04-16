package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CorporateBondOwnershipSpec extends AnyFlatSpec with Matchers:

  private given SimParams = SimParams.defaults

  "CorporateBondOwnership" should "initialize issuer liabilities to the market outstanding stock" in {
    val init        = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val issuerStock = CorporateBondOwnership.issuerOutstanding(init.ledgerFinancialState)
    val holderStock = CorporateBondOwnership.holderOutstanding(FlowSimulation.SimState.fromInit(init).ledgerFinancialState)

    issuerStock shouldBe holderStock
  }

  it should "project stock from ledger issuer and holder balances" in {
    val state          = LedgerTestFixtures.simState()
    val projectedStock = LedgerBoundaryProjection.corporateBondStock(state.ledgerFinancialState)

    projectedStock.outstanding shouldBe CorporateBondOwnership.issuerOutstanding(state.ledgerFinancialState)
    projectedStock.holderTotal shouldBe CorporateBondOwnership.holderOutstanding(state.ledgerFinancialState)
  }

  it should "amortize issuer liabilities pro rata and exactly at aggregate level" in {
    val init         = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val firmStates   = init.firms.take(2)
    val firms        = firmStates
      .zipWithIndex
      .map: (firm, index) =>
        LedgerFinancialState.firmBalances(firm, corpBond = PLN.fromLong((index + 1L) * 100L))
    val amortization = PLN.fromLong(60)
    val settled      = CorporateBondOwnership.applyAmortization(firms, firmStates, amortization)

    CorporateBondOwnership.issuerOutstanding(settled) shouldBe CorporateBondOwnership.issuerOutstanding(firms) - amortization
    settled.head.corpBond should be < firms.head.corpBond
    settled(1).corpBond should be < firms(1).corpBond
  }

  it should "clear defaulted issuer liabilities by firm id" in {
    val init       = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val firmStates = init.firms.take(2)
    val firms      = firmStates
      .zipWithIndex
      .map: (firm, index) =>
        LedgerFinancialState.firmBalances(firm, corpBond = PLN.fromLong((index + 1L) * 100L))
    val cleared    = CorporateBondOwnership.clearDefaultedIssuerDebt(firms, Set(firmStates.head.id))

    cleared.head.corpBond shouldBe PLN.Zero
    cleared(1).corpBond shouldBe firms(1).corpBond
  }
