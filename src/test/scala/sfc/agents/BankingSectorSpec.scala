package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.BankState
import sfc.agents.{BankingSector, IndividualBankState}
import sfc.config.Config
import sfc.types.*

import scala.util.Random

class BankingSectorSpec extends AnyFlatSpec with Matchers:

  private val configs = BankingSector.DefaultConfigs

  // ---- initialize ----

  "BankingSector.initialize" should "create 7 banks with correct deposit/capital shares" in {
    val bs = BankingSector.initialize(1000000.0, 100000.0, configs = configs)
    bs.banks.length shouldBe 7
    bs.banks.map(_.deposits).sum shouldBe 1000000.0 +- 0.01
    bs.banks.map(_.capital).sum shouldBe 100000.0 +- 0.01
  }

  it should "set all banks as not failed initially" in {
    val bs = BankingSector.initialize(1000000.0, 100000.0, configs = configs)
    bs.banks.forall(!_.failed) shouldBe true
  }

  it should "set deposits proportional to market share" in {
    val bs = BankingSector.initialize(1000000.0, 100000.0, configs = configs)
    bs.banks(0).deposits shouldBe (1000000.0 * 0.175) +- 0.01  // PKO BP
    bs.banks(5).deposits shouldBe (1000000.0 * 0.050) +- 0.01  // BPS/Coop
  }

  // ---- assignBank ----

  "BankingSector.assignBank" should "return valid bank index" in {
    val rng = new Random(42)
    for _ <- 0 until 100 do
      val bId = BankingSector.assignBank(SectorIdx(0), configs, rng)
      bId.toInt should be >= 0
      bId.toInt should be < 7
  }

  it should "favor BPS/Coop for agriculture firms" in {
    val rng = new Random(42)
    val assignments = (0 until 10000).map(_ => BankingSector.assignBank(SectorIdx(5), configs, rng))
    val bpsCount = assignments.count(_ == BankId(5))
    // BPS has 0.65 agriculture affinity vs others ~0.15, so it should get disproportionate share
    bpsCount.toDouble / 10000 should be > 0.10
  }

  // ---- lendingRate ----

  "BankingSector.lendingRate" should "return high spread for failed bank" in {
    val bank = IndividualBankState(BankId(0), 1e6, 1e6, 1e5, 0, 0, 0, 0, failed = true, 30, 0)
    val rate = BankingSector.lendingRate(bank, configs(0), 0.05)
    rate shouldBe (0.05 + 0.50) +- 0.001
  }

  it should "increase with NPL ratio" in {
    val bankLowNpl = IndividualBankState(BankId(0), 1e6, 1e6, 2e5, 1e4, 0, 0, 0, false, 0, 0)
    val bankHighNpl = IndividualBankState(BankId(0), 1e6, 1e6, 2e5, 2e5, 0, 0, 0, false, 0, 0)
    val rateLow = BankingSector.lendingRate(bankLowNpl, configs(0), 0.05)
    val rateHigh = BankingSector.lendingRate(bankHighNpl, configs(0), 0.05)
    rateHigh should be > rateLow
  }

  it should "include bank-specific spread" in {
    val bank = IndividualBankState(BankId(0), 1e6, 1e6, 2e5, 0, 0, 0, 0, false, 0, 0)
    val ratePko = BankingSector.lendingRate(bank, configs(0), 0.05)   // spread = -0.002
    val rateBps = BankingSector.lendingRate(bank, configs(5), 0.05)   // spread = +0.003
    rateBps should be > ratePko
  }

  // ---- canLend ----

  "BankingSector.canLend" should "return false for failed bank" in {
    val bank = IndividualBankState(BankId(0), 1e6, 1e6, 1e5, 0, 0, 0, 0, failed = true, 30, 0)
    BankingSector.canLend(bank, 1000.0, new Random(42)) shouldBe false
  }

  it should "reject when projected CAR too low" in {
    // capital=8000, loans=100000, existing CAR=0.08
    // Adding 10000 loan → projected = 8000/110000 = 0.0727 < 0.08
    val bank = IndividualBankState(BankId(0), 1e6, 100000.0, 8000.0, 0, 0, 0, 0, false, 0, 0)
    // Need to test multiple times since there's a stochastic element
    val results = (0 until 100).map(_ => BankingSector.canLend(bank, 10000.0, new Random(42)))
    results.forall(_ == false) shouldBe true
  }

  // ---- interbankRate ----

  "BankingSector.interbankRate" should "return deposit rate when NPL is zero" in {
    val banks = Vector(
      IndividualBankState(BankId(0), 1e6, 1e6, 2e5, 0, 0, 0, 0, false, 0, 0),
      IndividualBankState(BankId(1), 1e6, 1e6, 2e5, 0, 0, 0, 0, false, 0, 0)
    )
    val rate = BankingSector.interbankRate(banks, 0.05)
    rate shouldBe (0.05 - 0.01) +- 0.001  // deposit rate
  }

  it should "approach lombard rate when NPL is high" in {
    val banks = Vector(
      IndividualBankState(BankId(0), 1e6, 1e6, 2e5, 1e5, 0, 0, 0, false, 0, 0),  // 10% NPL
      IndividualBankState(BankId(1), 1e6, 1e6, 2e5, 1e5, 0, 0, 0, false, 0, 0)
    )
    val rate = BankingSector.interbankRate(banks, 0.05)
    // stress = 0.10 / 0.05 = 2.0, clipped to 1.0
    rate shouldBe (0.05 + 0.01) +- 0.001  // lombard rate
  }

  // ---- clearInterbank ----

  "BankingSector.clearInterbank" should "produce interbankNet that sums to zero" in {
    val banks = Vector(
      IndividualBankState(BankId(0), 1e6, 3e5, 2e5, 0, 1e5, 0, 0, false, 0, 0),
      IndividualBankState(BankId(1), 5e5, 8e5, 1e5, 0, 0, 0, 0, false, 0, 0),
      IndividualBankState(BankId(2), 8e5, 2e5, 1.5e5, 0, 5e4, 0, 0, false, 0, 0)
    )
    val cleared = BankingSector.clearInterbank(banks, configs.take(3), 0.05)
    val netSum = cleared.map(_.interbankNet).sum
    netSum shouldBe 0.0 +- 0.01
  }

  it should "set failed banks' interbankNet to zero" in {
    val banks = Vector(
      IndividualBankState(BankId(0), 1e6, 3e5, 2e5, 0, 0, 0, 0, false, 0, 0),
      IndividualBankState(BankId(1), 5e5, 8e5, 1e5, 0, 0, 0, 0, failed = true, 30, 3)
    )
    val cleared = BankingSector.clearInterbank(banks, configs.take(2), 0.05)
    cleared(1).interbankNet shouldBe 0.0
  }

  // ---- checkFailures ----

  "BankingSector.checkFailures" should "not trigger when disabled" in {
    val banks = Vector(
      IndividualBankState(BankId(0), 1e6, 1e6, 1000.0, 0, 0, 0, 0, false, 0, 5)  // Very low CAR
    )
    val (result, anyFailed) = BankingSector.checkFailures(banks, 30, enabled = false)
    anyFailed shouldBe false
    result(0).failed shouldBe false
  }

  it should "trigger after 3 consecutive months of low CAR" in {
    val bank = IndividualBankState(BankId(0), 1e6, 1e6, 1000.0, 0, 0, 0, 0, false, 0, 2)
    val (result, anyFailed) = BankingSector.checkFailures(Vector(bank), 30, enabled = true)
    anyFailed shouldBe true
    result(0).failed shouldBe true
    result(0).capital shouldBe 0.0  // Shareholders wiped
  }

  it should "reset consecutive counter when CAR recovers" in {
    val bank = IndividualBankState(BankId(0), 1e6, 1e6, 2e5, 0, 0, 0, 0, false, 0, 2)  // CAR = 0.20 > MinCar
    val (result, anyFailed) = BankingSector.checkFailures(Vector(bank), 30, enabled = true)
    anyFailed shouldBe false
    result(0).consecutiveLowCar shouldBe 0
  }

  // ---- resolveFailures ----

  "BankingSector.resolveFailures" should "transfer deposits to healthiest bank" in {
    val banks = Vector(
      IndividualBankState(BankId(0), 500000.0, 100000.0, 50000.0, 0, 10000.0, 0, 0, false, 0, 0),
      IndividualBankState(BankId(1), 300000.0, 80000.0, 0.0, 0, 5000.0, 0, 0, failed = true, 30, 3)
    )
    val (resolved, _) = BankingSector.resolveFailures(banks)
    resolved(0).deposits shouldBe 800000.0 +- 0.01  // absorbed 300k
    resolved(1).deposits shouldBe 0.0
  }

  // ---- allocateBonds ----

  "BankingSector.allocateBonds" should "distribute proportional to deposits" in {
    val banks = Vector(
      IndividualBankState(BankId(0), 600000.0, 0, 1e5, 0, 0, 0, 0, false, 0, 0),
      IndividualBankState(BankId(1), 400000.0, 0, 1e5, 0, 0, 0, 0, false, 0, 0)
    )
    val result = BankingSector.allocateBonds(banks, 10000.0)
    result(0).govBondHoldings shouldBe 6000.0 +- 0.01
    result(1).govBondHoldings shouldBe 4000.0 +- 0.01
  }

  it should "handle negative deficit (surplus)" in {
    val banks = Vector(
      IndividualBankState(BankId(0), 500000.0, 0, 1e5, 0, 8000.0, 0, 0, false, 0, 0),
      IndividualBankState(BankId(1), 500000.0, 0, 1e5, 0, 2000.0, 0, 0, false, 0, 0)
    )
    val result = BankingSector.allocateBonds(banks, -4000.0)
    result(0).govBondHoldings shouldBe 6000.0 +- 0.01  // 8000 + (-4000 * 0.5)
    result(1).govBondHoldings shouldBe 0.0 +- 0.01     // 2000 + (-4000 * 0.5)
  }

  it should "have per-bank deltas summing to exactly deficit (residual-based)" in {
    // 7 banks with irrational deposit ratios → FP rounding inevitable without residual
    val banks = (0 until 7).map(i =>
      IndividualBankState(BankId(i), 1e6 / 7.0 * (i + 1), 0, 1e5, 0, 1000.0 * (i + 1), 0, 0, false, 0, 0)
    ).toVector
    val deficit = 123456.789
    val result = BankingSector.allocateBonds(banks, deficit)
    val deltas = result.zip(banks).map((a, b) => a.govBondHoldings - b.govBondHoldings)
    deltas.sum shouldBe deficit +- 1e-6
  }

  it should "keep aggregate within tight tolerance with large deficit (1e13)" in {
    val banks = BankingSector.initialize(1e9, 1e8, configs = configs).banks
    val deficit = 1e13
    val before = banks.map(_.govBondHoldings).sum
    val result = BankingSector.allocateBonds(banks, deficit)
    val after = result.map(_.govBondHoldings).sum
    (after - before) shouldBe deficit +- 0.01  // well within SFC tolerance of 1.0
  }

  // ---- allocateQePurchases ----

  "BankingSector.allocateQePurchases" should "sell proportional to bond holdings" in {
    val banks = Vector(
      IndividualBankState(BankId(0), 1e6, 0, 1e5, 0, 6000.0, 0, 0, false, 0, 0),
      IndividualBankState(BankId(1), 1e6, 0, 1e5, 0, 4000.0, 0, 0, false, 0, 0)
    )
    val result = BankingSector.allocateQePurchases(banks, 5000.0)
    result(0).govBondHoldings shouldBe 3000.0 +- 0.01  // 6000 - 5000*0.6
    result(1).govBondHoldings shouldBe 2000.0 +- 0.01  // 4000 - 5000*0.4
  }

  it should "not change banks when qeTotal is zero" in {
    val banks = Vector(
      IndividualBankState(BankId(0), 1e6, 0, 1e5, 0, 5000.0, 0, 0, false, 0, 0)
    )
    val result = BankingSector.allocateQePurchases(banks, 0.0)
    result(0).govBondHoldings shouldBe 5000.0
  }

  // ---- reassignBankId ----

  "BankingSector.reassignBankId" should "keep valid bank unchanged" in {
    val banks = Vector(
      IndividualBankState(BankId(0), 1e6, 1e6, 2e5, 0, 0, 0, 0, false, 0, 0),
      IndividualBankState(BankId(1), 1e6, 1e6, 1e5, 0, 0, 0, 0, false, 0, 0)
    )
    BankingSector.reassignBankId(BankId(0), banks) shouldBe BankId(0)
  }

  it should "route to healthiest bank when current bank failed" in {
    val banks = Vector(
      IndividualBankState(BankId(0), 1e6, 1e6, 2e5, 0, 0, 0, 0, false, 0, 0),
      IndividualBankState(BankId(1), 1e6, 1e6, 1e5, 0, 0, 0, 0, failed = true, 30, 3)
    )
    BankingSector.reassignBankId(BankId(1), banks) shouldBe BankId(0)
  }

  // ---- aggregate ----

  "BankingSectorState.aggregate" should "sum all individual bank values" in {
    val bs = BankingSector.initialize(1000000.0, 100000.0, configs = configs)
    val agg = bs.aggregate
    agg.deposits shouldBe 1000000.0 +- 0.01
    agg.capital shouldBe 100000.0 +- 0.01
    agg.totalLoans shouldBe 0.0
  }
