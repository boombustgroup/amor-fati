package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.{AssetType, EntitySector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LedgerStateAdapterSpec extends AnyFlatSpec with Matchers:

  import LedgerTestFixtures.{enrichedSimState, simState}

  "LedgerStateAdapter" should "derive deterministic sector sizes from runtime populations" in {
    val state = simState()

    LedgerStateAdapter.sectorSizes(state) shouldBe Map(
      EntitySector.Households -> state.households.size,
      EntitySector.Firms      -> state.firms.size,
      EntitySector.Banks      -> state.banks.size,
      EntitySector.Government -> 1,
      EntitySector.NBP        -> 1,
      EntitySector.Insurance  -> 1,
      EntitySector.Funds      -> LedgerStateAdapter.FundIndex.Count,
      EntitySector.Foreign    -> 1,
    )
  }

  it should "round-trip the supported financial slice without semantic loss" in {
    val runtime  = enrichedSimState()
    val ledger   = LedgerStateAdapter.toMutableWorldState(runtime)
    val expected = LedgerStateAdapter.supportedSnapshot(runtime)

    LedgerStateAdapter.readSupported(ledger) shouldBe expected
  }

  it should "preserve bank total deposits and extended holder mappings in the supported slice" in {
    val runtime   = enrichedSimState()
    val supported = LedgerStateAdapter.supportedSnapshot(runtime)

    supported.banks.head.totalDeposits shouldBe PLN(603e6)
    supported.banks.head.demandDeposit + supported.banks.head.termDeposit shouldBe supported.banks.head.totalDeposits
    supported.foreign.govBondHoldings shouldBe PLN(778e6)
    supported.funds.ppkCorpBondHoldings shouldBe PLN(33e6)
    supported.funds.corpBondOtherHoldings shouldBe PLN(34e6)
    supported.funds.jstCash shouldBe PLN(10e6)
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

  it should "leave unsupported physical and mixed fields out of ledger balances" in {
    val runtime = enrichedSimState()
    val ledger  = LedgerStateAdapter.toMutableWorldState(runtime)

    runtime.firms.head.capitalStock should be > PLN.Zero
    runtime.banks.head.capital should be > PLN.Zero
    runtime.world.financial.quasiFiscal.bankHoldings should be > PLN.Zero
    runtime.world.financial.quasiFiscal.nbpHoldings should be > PLN.Zero

    ledger.snapshot.keySet should not contain ((EntitySector.Firms, AssetType.Capital, 0))
    ledger.snapshot.keySet should not contain ((EntitySector.Banks, AssetType.Cash, 0))
    ledger.snapshot.keySet should not contain ((EntitySector.Funds, AssetType.Reserve, LedgerStateAdapter.FundIndex.QuasiFiscal))
    ledger.snapshot.keySet should not contain ((EntitySector.Funds, AssetType.Cash, LedgerStateAdapter.FundIndex.QuasiFiscal))
  }
