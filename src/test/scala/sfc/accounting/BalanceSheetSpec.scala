package sfc.accounting

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.Config
import sfc.types.*

class BalanceSheetSpec extends AnyFlatSpec with Matchers:

  "BankState.nplRatio" should "equal nplAmount / totalLoans when totalLoans > 1" in {
    val b = BankState(totalLoans = PLN(1000000), nplAmount = PLN(50000), capital = PLN(200000), deposits = PLN(500000))
    b.nplRatio shouldBe 0.05 +- 0.001
  }

  it should "return 0.0 when totalLoans <= 1" in {
    BankState(PLN.Zero, PLN(100), PLN(200000), PLN(500000)).nplRatio shouldBe 0.0
    BankState(PLN(1), PLN(100), PLN(200000), PLN(500000)).nplRatio shouldBe 0.0
  }

  "BankState.car" should "equal capital / totalLoans when totalLoans > 1" in {
    val b = BankState(PLN(1000000), PLN.Zero, PLN(200000), PLN(500000))
    b.car shouldBe 0.2 +- 0.001
  }

  it should "return 10.0 when totalLoans <= 1" in {
    BankState(PLN.Zero, PLN.Zero, PLN(200000), PLN(500000)).car shouldBe 10.0
  }

  "BankState.lendingRate" should "equal refRate + spread + npl component" in {
    val b = BankState(PLN(1000000), PLN.Zero, PLN(200000), PLN(500000))
    // nplRatio=0, so npl component=0
    b.lendingRate(0.0575) shouldBe (0.0575 + Config.BaseSpread) +- 0.0001
  }

  it should "include NPL spread factor" in {
    val b = BankState(PLN(1000000), PLN(20000), PLN(200000), PLN(500000)) // nplRatio=0.02
    // npl component = min(0.15, 0.02 * 5.0) = 0.10 (not capped)
    val expected = 0.0575 + Config.BaseSpread + 0.02 * Config.NplSpreadFactor
    b.lendingRate(0.0575) shouldBe expected +- 0.0001
  }

  it should "cap the NPL component at 0.15" in {
    val b = BankState(PLN(1000000), PLN(500000), PLN(200000), PLN(500000)) // nplRatio=0.5 → uncapped = 2.5
    val rate = b.lendingRate(0.0575)
    // npl component should be min(0.15, 0.5 * 5.0) = 0.15
    rate shouldBe (0.0575 + Config.BaseSpread + 0.15) +- 0.0001
  }

  "BankState.canLend" should "return true when projected CAR >= 8%" in {
    val b = BankState(totalLoans = PLN(1000000), nplAmount = PLN.Zero, capital = PLN(200000), deposits = PLN(500000))
    // projected = 200000 / (1000000 + 100000) = 0.182 > 0.08
    b.canLend(100000) shouldBe true
  }

  it should "return false when projected CAR < 8%" in {
    val b = BankState(totalLoans = PLN(1000000), nplAmount = PLN.Zero, capital = PLN(85000), deposits = PLN(500000))
    // projected = 85000 / (1000000 + 100000) = 0.077 < 0.08
    b.canLend(100000) shouldBe false
  }
