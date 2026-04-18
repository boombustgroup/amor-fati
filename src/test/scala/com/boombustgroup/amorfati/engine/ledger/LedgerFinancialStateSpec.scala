package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.agents.{Firm, Nbp}
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LedgerFinancialStateSpec extends AnyFlatSpec with Matchers:

  private given SimParams = SimParams.defaults

  "LedgerFinancialState.refreshHouseholdBalances" should "preserve existing ledger balances and initialize only new households" in {
    val init          = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val existing      = init.households.head
    val existingIndex = existing.id.toInt
    val previous      = init.ledgerFinancialState.households.updated(
      existingIndex,
      init.ledgerFinancialState.households(existingIndex).copy(demandDeposit = PLN(123.0)),
    )
    val mirrorChanged = existing.withFinancial(_.copy(demandDeposit = PLN(999.0)))
    val newHousehold  = init.households.last
      .copy(
        id = HhId(previous.length),
      )
      .withFinancial(_.copy(demandDeposit = PLN(777.0), mortgageLoan = PLN(55.0), consumerLoan = PLN(11.0), equity = PLN(22.0)))

    val refreshed = LedgerFinancialState.refreshHouseholdBalances(Vector(mirrorChanged, newHousehold), previous)

    refreshed.head.demandDeposit shouldBe PLN(123.0)
    refreshed.last shouldBe LedgerFinancialState.householdBalances(newHousehold.financial)
  }

  "LedgerFinancialState.refreshFirmPopulationBalances" should "preserve existing ledger balances and initialize only new firms" in {
    val init          = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val existing      = init.firms.head
    val existingIndex = existing.id.toInt
    val previous      = init.ledgerFinancialState.firms.updated(
      existingIndex,
      init.ledgerFinancialState.firms(existingIndex).copy(cash = PLN(123.0), corpBond = PLN(456.0)),
    )
    val mirrorChanged = existing.withFinancial(_.copy(cash = PLN(999.0), firmLoan = PLN(88.0), equity = PLN(77.0)))
    val appendedFirm  = init.firms.last
      .copy(
        id = FirmId(previous.length),
      )
      .withFinancial(_.copy(cash = PLN(777.0), firmLoan = PLN(55.0), equity = PLN(22.0)))
    val recycledFirm  = existing.withFinancial(_.copy(cash = PLN(333.0), firmLoan = PLN(44.0), equity = PLN(12.0)))

    val refreshed = LedgerFinancialState.refreshFirmPopulationBalances(Vector(mirrorChanged, appendedFirm), previous, newFirmIds = Set(appendedFirm.id))
    val recycled  = LedgerFinancialState.refreshFirmPopulationBalances(Vector(recycledFirm), previous, newFirmIds = Set(recycledFirm.id))

    refreshed.head shouldBe previous(existingIndex)
    refreshed.last shouldBe LedgerFinancialState.firmBalances(appendedFirm.financial, corpBond = PLN.Zero)
    recycled.head shouldBe LedgerFinancialState.firmBalances(recycledFirm.financial, corpBond = PLN.Zero)
  }

  "LedgerFinancialState.refreshFirmFinancialBalances" should "update operational balances while preserving corporate bonds" in {
    val init     = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val previous = init.ledgerFinancialState.firms.updated(
      0,
      init.ledgerFinancialState.firms.head.copy(corpBond = PLN(456.0)),
    )
    val balances = Vector(
      Firm.FinancialStocks(
        cash = PLN(123.0),
        firmLoan = PLN(88.0),
        equity = PLN(77.0),
      ),
    )

    val refreshed = LedgerFinancialState.refreshFirmFinancialBalances(balances, previous)

    refreshed.head shouldBe LedgerFinancialState.FirmBalances(
      cash = PLN(123.0),
      firmLoan = PLN(88.0),
      corpBond = PLN(456.0),
      equity = PLN(77.0),
    )
  }

  "LedgerFinancialState.bankBalances" should "write bank execution stocks directly" in {
    val init = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val bank = init.banks.head.withFinancial(
      _.copy(
        totalDeposits = PLN(123.0),
        demandDeposit = PLN(100.0),
        termDeposit = PLN(23.0),
        firmLoan = PLN(88.0),
        consumerLoan = PLN(77.0),
        govBondAfs = PLN(66.0),
        govBondHtm = PLN(55.0),
        reserve = PLN(44.0),
        interbankLoan = PLN(33.0),
      ),
    )
    LedgerFinancialState.bankBalances(bank.financial, corpBond = PLN(22.0)) shouldBe LedgerFinancialState.BankBalances(
      totalDeposits = PLN(123.0),
      demandDeposit = PLN(100.0),
      termDeposit = PLN(23.0),
      firmLoan = PLN(88.0),
      consumerLoan = PLN(77.0),
      govBondAfs = PLN(66.0),
      govBondHtm = PLN(55.0),
      reserve = PLN(44.0),
      interbankLoan = PLN(33.0),
      corpBond = PLN(22.0),
    )
  }

  "LedgerFinancialState.nbpBalances" should "write NBP execution stocks directly" in {
    val stocks = Nbp.FinancialStocks(
      govBondHoldings = PLN(123.0),
      foreignAssets = PLN(456.0),
    )

    LedgerFinancialState.nbpBalances(stocks) shouldBe LedgerFinancialState.NbpBalances(
      govBondHoldings = PLN(123.0),
      foreignAssets = PLN(456.0),
    )
  }
