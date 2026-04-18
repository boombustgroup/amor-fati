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
