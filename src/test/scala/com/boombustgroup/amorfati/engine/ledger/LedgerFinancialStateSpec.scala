package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.agents.{Banking, Firm, Household, Nbp, QuasiFiscal}
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LedgerFinancialStateSpec extends AnyFlatSpec with Matchers:

  private given SimParams = SimParams.defaults

  private lazy val defaultInit: WorldInit.InitResult =
    WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))

  "LedgerFinancialState.refreshHouseholdBalances" should "preserve existing ledger balances and initialize only new households" in {
    val init              = defaultInit
    val existing          = init.households.head
    val existingIndex     = existing.id.toInt
    val previous          = init.ledgerFinancialState.households.updated(
      existingIndex,
      init.ledgerFinancialState.households(existingIndex).copy(demandDeposit = PLN(123)),
    )
    val existingHousehold = existing
    val newHousehold      = init.households.last.copy(id = HhId(previous.length))
    val newStocks         =
      Household.FinancialStocks(demandDeposit = PLN(777), mortgageLoan = PLN(55), consumerLoan = PLN(11), equity = PLN(22))

    val refreshed =
      LedgerFinancialState.refreshHouseholdBalances(Vector(existingHousehold, newHousehold), init.households, previous, Map(newHousehold.id -> newStocks))

    refreshed.head.demandDeposit shouldBe PLN(123)
    refreshed.last shouldBe LedgerFinancialState.householdBalances(newStocks)
  }

  "LedgerFinancialState.settleHouseholdMortgageStock" should "write an aggregate closing mortgage stock into household rows" in {
    val households = Vector(
      LedgerFinancialState.HouseholdBalances(
        demandDeposit = PLN(1),
        mortgageLoan = PLN(10),
        consumerLoan = PLN.Zero,
        equity = PLN.Zero,
      ),
      LedgerFinancialState.HouseholdBalances(
        demandDeposit = PLN(2),
        mortgageLoan = PLN(30),
        consumerLoan = PLN.Zero,
        equity = PLN.Zero,
      ),
    )

    val settled = LedgerFinancialState.settleHouseholdMortgageStock(households, PLN(80))

    LedgerFinancialState.householdMortgageStock(settled) shouldBe PLN(80)
    settled.map(_.demandDeposit) shouldBe households.map(_.demandDeposit)
    settled.map(_.consumerLoan) shouldBe households.map(_.consumerLoan)
    settled.map(_.equity) shouldBe households.map(_.equity)
  }

  it should "allocate a positive closing mortgage stock across zero-debt household rows" in {
    val households = Vector.fill(2)(
      LedgerFinancialState.HouseholdBalances(
        demandDeposit = PLN.Zero,
        mortgageLoan = PLN.Zero,
        consumerLoan = PLN.Zero,
        equity = PLN.Zero,
      ),
    )

    val settled = LedgerFinancialState.settleHouseholdMortgageStock(households, PLN(30))

    LedgerFinancialState.householdMortgageStock(settled) shouldBe PLN(30)
  }

  "LedgerFinancialState.refreshFirmPopulationBalances" should "refresh execution stocks while preserving existing corporate bonds" in {
    val init          = defaultInit
    val existingIndex = init.firms.head.id.toInt
    val previous      = init.ledgerFinancialState.firms.updated(
      existingIndex,
      init.ledgerFinancialState.firms(existingIndex).copy(cash = PLN(123), corpBond = PLN(456)),
    )
    val closingStocks = Firm.FinancialStocks(cash = PLN(999), firmLoan = PLN(88), equity = PLN(77))
    val appended      = Firm.FinancialStocks(cash = PLN(777), firmLoan = PLN(55), equity = PLN(22))
    val recycled      = Firm.FinancialStocks(cash = PLN(333), firmLoan = PLN(44), equity = PLN(12))

    val stockRows         =
      previous.map(LedgerFinancialState.projectFirmFinancialStocks).updated(existingIndex, closingStocks) :+ appended
    val recycledRows      =
      previous.map(LedgerFinancialState.projectFirmFinancialStocks).updated(existingIndex, recycled)
    val refreshed         =
      LedgerFinancialState.refreshFirmPopulationBalances(stockRows, previous, newFirmIds = Set(FirmId(previous.length)))
    val refreshedRecycled =
      LedgerFinancialState.refreshFirmPopulationBalances(recycledRows, previous, newFirmIds = Set(FirmId(existingIndex)))

    refreshed(existingIndex) shouldBe LedgerFinancialState.firmBalances(closingStocks, corpBond = PLN(456))
    refreshed.last shouldBe LedgerFinancialState.firmBalances(appended, corpBond = PLN.Zero)
    refreshedRecycled(existingIndex) shouldBe LedgerFinancialState.firmBalances(recycled, corpBond = PLN.Zero)
  }

  "LedgerFinancialState.refreshFirmFinancialBalances" should "update operational balances while preserving corporate bonds" in {
    val init     = defaultInit
    val previous = init.ledgerFinancialState.firms.updated(
      0,
      init.ledgerFinancialState.firms.head.copy(corpBond = PLN(456)),
    )
    val balances = Vector(
      Firm.FinancialStocks(
        cash = PLN(123),
        firmLoan = PLN(88),
        equity = PLN(77),
      ),
    )

    val refreshed = LedgerFinancialState.refreshFirmFinancialBalances(balances, previous)

    refreshed.head shouldBe LedgerFinancialState.FirmBalances(
      cash = PLN(123),
      firmLoan = PLN(88),
      corpBond = PLN(456),
      equity = PLN(77),
    )
  }

  "LedgerFinancialState.bankBalances" should "write bank execution stocks directly" in {
    val stocks = Banking.BankFinancialStocks(
      totalDeposits = PLN(123),
      demandDeposit = PLN(100),
      termDeposit = PLN(23),
      firmLoan = PLN(88),
      consumerLoan = PLN(77),
      govBondAfs = PLN(66),
      govBondHtm = PLN(55),
      reserve = PLN(44),
      interbankLoan = PLN(33),
    )
    LedgerFinancialState.bankBalances(stocks, corpBond = PLN(22)) shouldBe LedgerFinancialState.BankBalances(
      totalDeposits = PLN(123),
      demandDeposit = PLN(100),
      termDeposit = PLN(23),
      firmLoan = PLN(88),
      consumerLoan = PLN(77),
      govBondAfs = PLN(66),
      govBondHtm = PLN(55),
      reserve = PLN(44),
      interbankLoan = PLN(33),
      corpBond = PLN(22),
    )
  }

  "LedgerFinancialState.nbpBalances" should "write NBP execution stocks directly" in {
    val stocks = Nbp.FinancialStocks(
      govBondHoldings = PLN(123),
      foreignAssets = PLN(456),
    )

    LedgerFinancialState.nbpBalances(stocks) shouldBe LedgerFinancialState.NbpBalances(
      govBondHoldings = PLN(123),
      foreignAssets = PLN(456),
    )
  }

  "LedgerFinancialState.quasiFiscalBalances" should "round-trip holder split through ledger-owned stock" in {
    val stock = QuasiFiscal.StockState(
      bondsOutstanding = PLN(123),
      loanPortfolio = PLN(45),
      bankHoldings = PLN(67),
      nbpHoldings = PLN(56),
    )

    val balances = LedgerFinancialState.quasiFiscalBalances(stock)

    balances shouldBe LedgerFinancialState.QuasiFiscalBalances(
      bondsOutstanding = PLN(123),
      loanPortfolio = PLN(45),
      bankHoldings = PLN(67),
      nbpHoldings = PLN(56),
    )
  }
