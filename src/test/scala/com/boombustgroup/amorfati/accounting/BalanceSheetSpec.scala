package com.boombustgroup.amorfati.accounting

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

class BalanceSheetSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  "BankingAggregate.nplRatio" should "equal nplAmount / totalLoans when totalLoans > 1" in {
    val b =
      Banking.Aggregate(PLN(1000000), PLN(50000), PLN(200000), PLN(500000), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    b.nplRatio.toDouble shouldBe 0.05 +- 0.001
  }

  it should "return 0.0 when totalLoans <= 1" in {
    Banking
      .Aggregate(
        PLN.Zero,
        PLN(100),
        PLN(200000),
        PLN(500000),
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
      )
      .nplRatio shouldBe Share.Zero
    Banking
      .Aggregate(
        PLN(1),
        PLN(100),
        PLN(200000),
        PLN(500000),
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
      )
      .nplRatio shouldBe Share.Zero
  }

  "BankingAggregate.car" should "equal capital / totalLoans when totalLoans > 1" in {
    val b = Banking.Aggregate(PLN(1000000), PLN.Zero, PLN(200000), PLN(500000), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    b.car.toDouble shouldBe 0.2 +- 0.001
  }

  it should "return 10.0 when totalLoans <= 1" in {
    Banking
      .Aggregate(
        PLN.Zero,
        PLN.Zero,
        PLN(200000),
        PLN(500000),
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
      )
      .car shouldBe Multiplier(10.0)
  }

  // lendingRate and canLend removed from BankingAggregate — now only on Banking.BankState
