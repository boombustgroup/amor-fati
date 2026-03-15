package com.boombustgroup.amorfati.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.agents.Banking.BankStatus
import com.boombustgroup.amorfati.Generators
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

import scala.util.Random

class BankingSectorSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val configs = Banking.DefaultConfigs

  private def mkBank(
      id: Int = 0,
      deposits: PLN = PLN(1e6),
      loans: PLN = PLN(1e6),
      capital: PLN = PLN(2e5),
      nplAmount: PLN = PLN.Zero,
      govBondHoldings: PLN = PLN.Zero,
      reservesAtNbp: PLN = PLN.Zero,
      interbankNet: PLN = PLN.Zero,
      status: BankStatus = BankStatus.Active(0),
      demandDeposits: PLN = PLN.Zero,
      termDeposits: PLN = PLN.Zero,
      loansShort: PLN = PLN.Zero,
      loansMedium: PLN = PLN.Zero,
      loansLong: PLN = PLN.Zero,
      consumerLoans: PLN = PLN.Zero,
      consumerNpl: PLN = PLN.Zero,
      corpBondHoldings: PLN = PLN.Zero,
  ): Banking.BankState = Banking.BankState(
    id = BankId(id),
    deposits = deposits,
    loans = loans,
    capital = capital,
    nplAmount = nplAmount,
    govBondHoldings = govBondHoldings,
    reservesAtNbp = reservesAtNbp,
    interbankNet = interbankNet,
    status = status,
    demandDeposits = demandDeposits,
    termDeposits = termDeposits,
    loansShort = loansShort,
    loansMedium = loansMedium,
    loansLong = loansLong,
    consumerLoans = consumerLoans,
    consumerNpl = consumerNpl,
    corpBondHoldings = corpBondHoldings,
  )

  // ---- initialize ----

  "Banking.initialize" should "create 7 banks with correct deposit/capital shares" in {
    val bs = Generators.testBankingSector(totalDeposits = PLN(1000000.0), totalCapital = PLN(100000.0), totalLoans = PLN.Zero, configs = configs)
    bs.banks.length shouldBe 7
    bs.banks.map(_.deposits.toDouble).sum shouldBe 1000000.0 +- 0.01
    bs.banks.map(_.capital.toDouble).sum shouldBe 100000.0 +- 0.01
  }

  it should "set all banks as not failed initially" in {
    val bs = Generators.testBankingSector(totalDeposits = PLN(1000000.0), totalCapital = PLN(100000.0), totalLoans = PLN.Zero, configs = configs)
    bs.banks.forall(!_.failed) shouldBe true
  }

  it should "set deposits proportional to market share" in {
    val bs = Generators.testBankingSector(totalDeposits = PLN(1000000.0), totalCapital = PLN(100000.0), totalLoans = PLN.Zero, configs = configs)
    bs.banks(0).deposits.toDouble shouldBe (1000000.0 * 0.175) +- 0.01 // PKO BP
    bs.banks(5).deposits.toDouble shouldBe (1000000.0 * 0.050) +- 0.01 // BPS/Coop
  }

  // ---- assignBank ----

  "Banking.assignBank" should "return valid bank index" in {
    val rng = new Random(42)
    for _ <- 0 until 100 do
      val bId = Banking.assignBank(SectorIdx(0), configs, rng)
      bId.toInt should be >= 0
      bId.toInt should be < 7
  }

  it should "favor BPS/Coop for agriculture firms" in {
    val rng         = new Random(42)
    val assignments = (0 until 10000).map(_ => Banking.assignBank(SectorIdx(5), configs, rng))
    val bpsCount    = assignments.count(_ == BankId(5))
    // BPS has 0.65 agriculture affinity vs others ~0.15, so it should get disproportionate share
    bpsCount.toDouble / 10000 should be > 0.10
  }

  // ---- lendingRate ----

  "Banking.lendingRate" should "return high spread for failed bank" in {
    val bank = mkBank(status = BankStatus.Failed(30))
    val rate = Banking.lendingRate(bank, configs(0), Rate(0.05), Rate.Zero)
    rate.toDouble shouldBe (0.05 + 0.50) +- 0.001
  }

  it should "increase with NPL ratio" in {
    val bankLowNpl  = mkBank(nplAmount = PLN(1e4))
    val bankHighNpl = mkBank(nplAmount = PLN(2e5))
    val rateLow     = Banking.lendingRate(bankLowNpl, configs(0), Rate(0.05), Rate.Zero)
    val rateHigh    = Banking.lendingRate(bankHighNpl, configs(0), Rate(0.05), Rate.Zero)
    rateHigh should be > rateLow
  }

  it should "include bank-specific spread" in {
    val bank    = mkBank(nplAmount = PLN.Zero)
    val ratePko = Banking.lendingRate(bank, configs(0), Rate(0.05), Rate.Zero) // spread = -0.002
    val rateBps = Banking.lendingRate(bank, configs(5), Rate(0.05), Rate.Zero) // spread = +0.003
    rateBps should be > ratePko
  }

  // ---- canLend ----

  "Banking.canLend" should "return false for failed bank" in {
    val bank = mkBank(capital = PLN(1e5), status = BankStatus.Failed(30))
    Banking.canLend(bank, PLN(1000.0), new Random(42), Rate.Zero) shouldBe false
  }

  it should "reject when projected CAR too low" in {
    // capital=8000, loans=100000, existing CAR=0.08
    // Adding 10000 loan -> projected = 8000/110000 = 0.0727 < 0.08
    val bank    = mkBank(loans = PLN(100000.0), capital = PLN(8000.0))
    // Need to test multiple times since there's a stochastic element
    val results = (0 until 100).map(_ => Banking.canLend(bank, PLN(10000.0), new Random(42), Rate.Zero))
    results.forall(_ == false) shouldBe true
  }

  // ---- interbankRate ----

  "Banking.interbankRate" should "return deposit rate when NPL is zero" in {
    val banks = Vector(
      mkBank(id = 0),
      mkBank(id = 1),
    )
    val rate  = Banking.interbankRate(banks, Rate(0.05))
    rate.toDouble shouldBe (0.05 - 0.01) +- 0.001 // deposit rate
  }

  it should "approach lombard rate when NPL is high" in {
    val banks = Vector(
      mkBank(id = 0, nplAmount = PLN(1e5)), // 10% NPL
      mkBank(id = 1, nplAmount = PLN(1e5)),
    )
    val rate  = Banking.interbankRate(banks, Rate(0.05))
    // stress = 0.10 / 0.05 = 2.0, clipped to 1.0
    rate.toDouble shouldBe (0.05 + 0.01) +- 0.001 // lombard rate
  }

  // ---- clearInterbank ----

  "Banking.clearInterbank" should "produce interbankNet that sums to zero" in {
    val banks   = Vector(
      mkBank(id = 0, loans = PLN(3e5), govBondHoldings = PLN(1e5)),
      mkBank(id = 1, deposits = PLN(5e5), loans = PLN(8e5), capital = PLN(1e5)),
      mkBank(id = 2, deposits = PLN(8e5), loans = PLN(2e5), capital = PLN(1.5e5), govBondHoldings = PLN(5e4)),
    )
    val cleared = Banking.clearInterbank(banks, configs.take(3))
    val netSum  = cleared.map(_.interbankNet.toDouble).sum
    netSum shouldBe 0.0 +- 0.01
  }

  it should "set failed banks' interbankNet to zero" in {
    val banks   = Vector(
      mkBank(id = 0, loans = PLN(3e5)),
      mkBank(id = 1, deposits = PLN(5e5), loans = PLN(8e5), capital = PLN(1e5), status = BankStatus.Failed(30)),
    )
    val cleared = Banking.clearInterbank(banks, configs.take(2))
    cleared(1).interbankNet shouldBe PLN.Zero
  }

  // ---- checkFailures ----

  "Banking.checkFailures" should "not trigger when disabled" in {
    val banks = Vector(
      mkBank(capital = PLN(1000.0), status = BankStatus.Active(5)), // Very low CAR
    )
    val result = Banking.checkFailures(banks, 30, enabled = false, Rate.Zero)
    result.anyFailed shouldBe false
    result.banks(0).failed shouldBe false
  }

  it should "trigger after 3 consecutive months of low CAR" in {
    val bank   = mkBank(capital = PLN(1000.0), status = BankStatus.Active(2))
    val result = Banking.checkFailures(Vector(bank), 30, enabled = true, Rate.Zero)
    result.anyFailed shouldBe true
    result.banks(0).failed shouldBe true
    result.banks(0).capital shouldBe PLN.Zero // Shareholders wiped
  }

  it should "reset consecutive counter when CAR recovers" in {
    val bank   = mkBank(status = BankStatus.Active(2)) // CAR = 0.20 > MinCar
    val result = Banking.checkFailures(Vector(bank), 30, enabled = true, Rate.Zero)
    result.anyFailed shouldBe false
    result.banks(0).consecutiveLowCar shouldBe 0
  }

  // ---- resolveFailures ----

  "Banking.resolveFailures" should "transfer deposits to healthiest bank" in {
    val banks  = Vector(
      mkBank(id = 0, deposits = PLN(500000.0), loans = PLN(100000.0), capital = PLN(50000.0), govBondHoldings = PLN(10000.0)),
      mkBank(id = 1, deposits = PLN(300000.0), loans = PLN(80000.0), capital = PLN(0.0), govBondHoldings = PLN(5000.0), status = BankStatus.Failed(30)),
    )
    val result = Banking.resolveFailures(banks)
    result.banks(0).deposits.toDouble shouldBe 800000.0 +- 0.01 // absorbed 300k
    result.banks(1).deposits shouldBe PLN.Zero
  }

  // ---- allocateBonds ----

  "Banking.allocateBonds" should "distribute proportional to deposits" in {
    val banks  = Vector(
      mkBank(id = 0, deposits = PLN(600000.0), loans = PLN.Zero, capital = PLN(1e5)),
      mkBank(id = 1, deposits = PLN(400000.0), loans = PLN.Zero, capital = PLN(1e5)),
    )
    val result = Banking.allocateBonds(banks, PLN(10000.0))
    result(0).govBondHoldings.toDouble shouldBe 6000.0 +- 0.01
    result(1).govBondHoldings.toDouble shouldBe 4000.0 +- 0.01
  }

  it should "handle negative deficit (surplus)" in {
    val banks  = Vector(
      mkBank(id = 0, deposits = PLN(500000.0), loans = PLN.Zero, capital = PLN(1e5), govBondHoldings = PLN(8000.0)),
      mkBank(id = 1, deposits = PLN(500000.0), loans = PLN.Zero, capital = PLN(1e5), govBondHoldings = PLN(2000.0)),
    )
    val result = Banking.allocateBonds(banks, PLN(-4000.0))
    result(0).govBondHoldings.toDouble shouldBe 6000.0 +- 0.01 // 8000 + (-4000 * 0.5)
    result(1).govBondHoldings.toDouble shouldBe 0.0 +- 0.01    // 2000 + (-4000 * 0.5)
  }

  it should "have per-bank deltas summing to exactly deficit (residual-based)" in {
    // 7 banks with irrational deposit ratios -> FP rounding inevitable without residual
    val banks   = (0 until 7)
      .map(i =>
        mkBank(
          id = i,
          deposits = PLN(1e6 / 7.0 * (i + 1)),
          loans = PLN.Zero,
          capital = PLN(1e5),
          govBondHoldings = PLN(1000.0 * (i + 1)),
        ),
      )
      .toVector
    val deficit = PLN(123456.789)
    val result  = Banking.allocateBonds(banks, deficit)
    val deltas  = result.zip(banks).map((a, b) => a.govBondHoldings.toDouble - b.govBondHoldings.toDouble)
    deltas.sum shouldBe deficit.toDouble +- 1e-6
  }

  it should "keep aggregate within tight tolerance with large deficit (1e13)" in {
    val banks   = Generators.testBankingSector(totalDeposits = PLN(1e9), totalCapital = PLN(1e8), totalLoans = PLN.Zero, configs = configs).banks
    val deficit = PLN(1e13)
    val before  = banks.map(_.govBondHoldings.toDouble).sum
    val result  = Banking.allocateBonds(banks, deficit)
    val after   = result.map(_.govBondHoldings.toDouble).sum
    (after - before) shouldBe deficit.toDouble +- 0.01 // well within SFC tolerance of 1.0
  }

  // ---- allocateQePurchases ----

  "Banking.allocateQePurchases" should "sell proportional to bond holdings" in {
    val banks  = Vector(
      mkBank(id = 0, loans = PLN.Zero, capital = PLN(1e5), govBondHoldings = PLN(6000.0)),
      mkBank(id = 1, loans = PLN.Zero, capital = PLN(1e5), govBondHoldings = PLN(4000.0)),
    )
    val result = Banking.allocateQePurchases(banks, PLN(5000.0))
    result(0).govBondHoldings.toDouble shouldBe 3000.0 +- 0.01 // 6000 - 5000*0.6
    result(1).govBondHoldings.toDouble shouldBe 2000.0 +- 0.01 // 4000 - 5000*0.4
  }

  it should "not change banks when qeTotal is zero" in {
    val banks  = Vector(
      mkBank(loans = PLN.Zero, capital = PLN(1e5), govBondHoldings = PLN(5000.0)),
    )
    val result = Banking.allocateQePurchases(banks, PLN.Zero)
    result(0).govBondHoldings shouldBe PLN(5000.0)
  }

  // ---- reassignBankId ----

  "Banking.reassignBankId" should "keep valid bank unchanged" in {
    val banks = Vector(mkBank(id = 0), mkBank(id = 1, capital = PLN(1e5)))
    Banking.reassignBankId(BankId(0), banks) shouldBe BankId(0)
  }

  it should "route to healthiest bank when current bank failed" in {
    val banks = Vector(
      mkBank(id = 0),
      mkBank(id = 1, capital = PLN(1e5), status = BankStatus.Failed(30)),
    )
    Banking.reassignBankId(BankId(1), banks) shouldBe BankId(0)
  }

  // ---- aggregate ----

  "Banking.State.aggregate" should "sum all individual bank values" in {
    val bs  = Generators.testBankingSector(totalDeposits = PLN(1000000.0), totalCapital = PLN(100000.0), totalLoans = PLN.Zero, configs = configs)
    val agg = bs.aggregate
    agg.deposits.toDouble shouldBe 1000000.0 +- 0.01
    agg.capital.toDouble shouldBe 100000.0 +- 0.01
    agg.totalLoans shouldBe PLN.Zero
  }
