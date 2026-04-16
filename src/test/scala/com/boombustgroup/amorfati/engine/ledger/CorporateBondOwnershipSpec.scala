package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CorporateBondOwnershipSpec extends AnyFlatSpec with Matchers:

  private given SimParams = SimParams.defaults

  "CorporateBondOwnership" should "initialize issuer liabilities to the market outstanding stock" in {
    val init         = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val issuerStock  = CorporateBondOwnership.issuerOutstanding(init.firms)
    val boundaryView = init.world.financial.corporateBonds

    issuerStock shouldBe boundaryView.outstanding
  }

  it should "project outstanding from ledger issuer liabilities, not from the stale boundary view" in {
    val state          = LedgerTestFixtures.simState()
    val staleBoundary  = state.world.financial.corporateBonds.copy(outstanding = PLN(1))
    val projectedState = LedgerBoundaryProjection.corporateBondState(staleBoundary, state.ledgerFinancialState)

    projectedState.outstanding shouldBe CorporateBondOwnership.issuerOutstanding(state.ledgerFinancialState)
    projectedState.outstanding should not be staleBoundary.outstanding
  }

  it should "amortize issuer liabilities pro rata and exactly at aggregate level" in {
    val init         = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val firms        = init.firms
      .take(2)
      .zipWithIndex
      .map: (firm, index) =>
        firm.copy(bondDebt = PLN.fromLong((index + 1L) * 100L))
    val amortization = PLN.fromLong(60)
    val settled      = CorporateBondOwnership.applyAmortization(firms, amortization)

    CorporateBondOwnership.issuerOutstanding(settled) shouldBe CorporateBondOwnership.issuerOutstanding(firms) - amortization
    settled.head.bondDebt should be < firms.head.bondDebt
    settled(1).bondDebt should be < firms(1).bondDebt
  }

  it should "clear defaulted issuer liabilities by firm id" in {
    val init    = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val firms   = init.firms
      .take(2)
      .zipWithIndex
      .map: (firm, index) =>
        firm.copy(bondDebt = PLN.fromLong((index + 1L) * 100L))
    val cleared = CorporateBondOwnership.clearDefaultedIssuerDebt(firms, Set(firms.head.id))

    cleared.head.bondDebt shouldBe PLN.Zero
    cleared(1).bondDebt shouldBe firms(1).bondDebt
  }
