package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LedgerStateAdapterSpec extends AnyFlatSpec with Matchers:

  import LedgerTestFixtures.enrichedSimState

  "LedgerStateAdapter" should "preserve bank total deposits and extended holder mappings in the ledger financial state" in {
    val runtime = enrichedSimState()
    val ledger  = runtime.ledgerFinancialState

    ledger.banks.head.totalDeposits shouldBe PLN(603e6)
    ledger.banks.head.demandDeposit + ledger.banks.head.termDeposit shouldBe ledger.banks.head.totalDeposits
    ledger.foreign.govBondHoldings shouldBe PLN(778e6)
    ledger.funds.ppkCorpBondHoldings shouldBe PLN(33e6)
    ledger.funds.corpBondOtherHoldings shouldBe PLN(34e6)
    ledger.funds.jstCash shouldBe PLN(10e6)
  }

  it should "expose unsupported financial fields explicitly instead of forcing them into the ledger slice" in {
    val runtime     = enrichedSimState()
    val unsupported = LedgerStateAdapter.unsupportedSnapshot(runtime)

    unsupported.government.fiscalCumulativeDebt shouldBe runtime.world.gov.cumulativeDebt
    unsupported.nbp.qeCumulativePurchases shouldBe PLN(89e6)
    unsupported.jst.jstDebt shouldBe PLN(11e6)
    unsupported.quasiFiscal.bankHoldings shouldBe PLN(29e6)
    unsupported.banks.head.capital shouldBe PLN(310e6)
  }

  it should "carry ledger financial state explicitly inside SimState" in {
    val runtime = enrichedSimState()

    runtime.ledgerFinancialState shouldBe LedgerStateAdapter.captureLedgerFinancialState(
      runtime.world,
      runtime.firms,
      runtime.households,
      runtime.banks,
    )
  }
