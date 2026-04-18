package com.boombustgroup.amorfati.engine.ledger

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
    refreshed.last shouldBe LedgerFinancialState.householdBalances(newHousehold)
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
    refreshed.last shouldBe LedgerFinancialState.firmBalances(appendedFirm, corpBond = PLN.Zero)
    recycled.head shouldBe LedgerFinancialState.firmBalances(recycledFirm, corpBond = PLN.Zero)
  }
