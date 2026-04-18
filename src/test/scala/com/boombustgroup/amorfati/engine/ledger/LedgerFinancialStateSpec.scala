package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.agents.{Banking, Firm, Household}
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
    val mirrorChanged = existing.copy(savings = PLN(999.0))
    val newHousehold  = init.households.last.copy(
      id = HhId(previous.length),
      savings = PLN(777.0),
      debt = PLN(55.0),
      consumerDebt = PLN(11.0),
      equityWealth = PLN(22.0),
    )

    val refreshed = LedgerFinancialState.refreshHouseholdBalances(Vector(mirrorChanged, newHousehold), previous)

    refreshed.head.demandDeposit shouldBe PLN(123.0)
    refreshed.last shouldBe LedgerFinancialState.householdBalances(Household.FinancialBalances.fromState(newHousehold))
  }

  "LedgerFinancialState.refreshFirmPopulationBalances" should "preserve existing ledger balances and initialize only new firms" in {
    val init          = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val existing      = init.firms.head
    val existingIndex = existing.id.toInt
    val previous      = init.ledgerFinancialState.firms.updated(
      existingIndex,
      init.ledgerFinancialState.firms(existingIndex).copy(cash = PLN(123.0), corpBond = PLN(456.0)),
    )
    val mirrorChanged = existing.copy(cash = PLN(999.0), debt = PLN(88.0), equityRaised = PLN(77.0))
    val appendedFirm  = init.firms.last.copy(
      id = FirmId(previous.length),
      cash = PLN(777.0),
      debt = PLN(55.0),
      equityRaised = PLN(22.0),
    )
    val recycledFirm  = existing.copy(cash = PLN(333.0), debt = PLN(44.0), equityRaised = PLN(12.0))

    val refreshed = LedgerFinancialState.refreshFirmPopulationBalances(Vector(mirrorChanged, appendedFirm), previous, newFirmIds = Set(appendedFirm.id))
    val recycled  = LedgerFinancialState.refreshFirmPopulationBalances(Vector(recycledFirm), previous, newFirmIds = Set(recycledFirm.id))

    refreshed.head shouldBe previous(existingIndex)
    refreshed.last shouldBe LedgerFinancialState.firmBalances(Firm.FinancialBalances.fromState(appendedFirm), corpBond = PLN.Zero)
    recycled.head shouldBe LedgerFinancialState.firmBalances(Firm.FinancialBalances.fromState(recycledFirm), corpBond = PLN.Zero)
  }

  "LedgerFinancialState.refreshFirmFinancialBalances" should "update operational balances while preserving corporate bonds" in {
    val init     = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val previous = init.ledgerFinancialState.firms.updated(
      0,
      init.ledgerFinancialState.firms.head.copy(corpBond = PLN(456.0)),
    )
    val balances = Vector(
      Firm.FinancialBalances(
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

  "LedgerFinancialState.refreshBankFinancialBalances" should "write bank execution balances directly" in {
    val init     = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val bank     = init.banks.head.copy(
      deposits = PLN(123.0),
      demandDeposits = PLN(100.0),
      termDeposits = PLN(23.0),
      loans = PLN(88.0),
      consumerLoans = PLN(77.0),
      afsBonds = PLN(66.0),
      htmBonds = PLN(55.0),
      reservesAtNbp = PLN(44.0),
      interbankNet = PLN(33.0),
    )
    val balances = Vector(Banking.FinancialBalances.fromState(bank, corpBond = PLN(22.0)))

    val refreshed = LedgerFinancialState.refreshBankFinancialBalances(balances)

    refreshed.head shouldBe LedgerFinancialState.BankBalances(
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
