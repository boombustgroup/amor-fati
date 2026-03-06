package sfc.accounting

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.types.*

class BalanceSheetSpec extends AnyFlatSpec with Matchers:

  "BankingAggregate.nplRatio" should "equal nplAmount / totalLoans when totalLoans > 1" in {
    val b =
      BankingAggregate(PLN(1000000), PLN(50000), PLN(200000), PLN(500000), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    b.nplRatio shouldBe 0.05 +- 0.001
  }

  it should "return 0.0 when totalLoans <= 1" in {
    BankingAggregate(
      PLN.Zero,
      PLN(100),
      PLN(200000),
      PLN(500000),
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
    ).nplRatio shouldBe 0.0
    BankingAggregate(
      PLN(1),
      PLN(100),
      PLN(200000),
      PLN(500000),
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
    ).nplRatio shouldBe 0.0
  }

  "BankingAggregate.car" should "equal capital / totalLoans when totalLoans > 1" in {
    val b = BankingAggregate(PLN(1000000), PLN.Zero, PLN(200000), PLN(500000), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    b.car shouldBe 0.2 +- 0.001
  }

  it should "return 10.0 when totalLoans <= 1" in {
    BankingAggregate(
      PLN.Zero,
      PLN.Zero,
      PLN(200000),
      PLN(500000),
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
    ).car shouldBe 10.0
  }

  // lendingRate and canLend removed from BankingAggregate — now only on Banking.BankState
