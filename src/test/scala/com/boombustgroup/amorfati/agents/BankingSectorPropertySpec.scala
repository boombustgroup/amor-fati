package com.boombustgroup.amorfati.agents

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.agents.Banking.BankStatus
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

import scala.util.Random

class BankingSectorPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams = SimParams.defaults

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  private val configs = Banking.DefaultConfigs

  @annotation.nowarn("msg=unused private member") // defaults used by callers
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
    demandDeposits = PLN.Zero,
    termDeposits = PLN.Zero,
    loansShort = PLN.Zero,
    loansMedium = PLN.Zero,
    loansLong = PLN.Zero,
    consumerLoans = PLN.Zero,
    consumerNpl = PLN.Zero,
    corpBondHoldings = PLN.Zero,
  )

  // ---- Interbank netting invariant ----

  "clearInterbank" should "always produce interbankNet that sums to zero" in
    forAll(genBanking.State) { (bs: Banking.State) =>
      val cleared = Banking.clearInterbank(bs.banks, bs.configs)
      val netSum  = cleared.map(_.interbankNet.toDouble).sum
      netSum shouldBe 0.0 +- 1.0 // tolerance for floating-point
    }

  // ---- allocateBonds increment sums to deficit ----

  "allocateBonds" should "have individual deltas summing to exactly deficit (residual)" in
    forAll(genBanking.State, Gen.choose(-1e8, 1e8)) { (bs: Banking.State, deficit: Double) =>
      val aliveDep = bs.banks.filterNot(_.failed).map(_.deposits.toDouble).sum
      whenever(aliveDep > 0 && deficit != 0.0) {
        val after  = Banking.allocateBonds(bs.banks, PLN(deficit))
        // Per-bank deltas sum to exactly deficit (residual-based allocation)
        val deltas = after.zip(bs.banks).map((a, b) => a.govBondHoldings.toDouble - b.govBondHoldings.toDouble)
        deltas.sum shouldBe deficit +- 1e-6
      }
    }

  it should "keep aggregate bond change within tight tolerance for large deficits" in
    forAll(genBanking.State, Gen.choose(1e10, 1e14)) { (bs: Banking.State, deficit: Double) =>
      val alive = bs.banks.filterNot(_.failed)
      whenever(alive.nonEmpty) {
        val before   = bs.banks.map(_.govBondHoldings.toDouble).sum
        val after    = Banking.allocateBonds(bs.banks, PLN(deficit))
        val afterSum = after.map(_.govBondHoldings.toDouble).sum
        (afterSum - before) shouldBe deficit +- 1.0 // well within SFC tolerance
      }
    }

  // ---- lendingRate monotonic in NPL ----

  "lendingRate" should "be monotonically non-decreasing with NPL ratio" in
    forAll(genRate, Gen.choose(0.0, 0.15), Gen.choose(0.0, 0.15)) { (refRate: Double, npl1Frac: Double, npl2Frac: Double) =>
      val loans    = 1e6
      val (lo, hi) = if npl1Frac <= npl2Frac then (npl1Frac, npl2Frac) else (npl2Frac, npl1Frac)
      val bankLo   = mkBank(nplAmount = PLN(loans * lo))
      val bankHi   = mkBank(nplAmount = PLN(loans * hi))
      val rateLo   = Banking.lendingRate(bankLo, configs(0), Rate(refRate))
      val rateHi   = Banking.lendingRate(bankHi, configs(0), Rate(refRate))
      rateHi should be >= rateLo
    }

  // ---- assignBank returns valid index ----

  "assignBank" should "always return valid index in [0, nBanks)" in
    forAll(Gen.choose(0, 5)) { (sector: Int) =>
      val rng = new Random()
      for _ <- 0 until 50 do
        val bId = Banking.assignBank(SectorIdx(sector), configs, rng)
        bId.toInt should be >= 0
        bId.toInt should be < configs.length
    }

  // ---- aggregate.deposits == sum of individual deposits ----

  "aggregate" should "have deposits equal to sum of individual deposits" in
    forAll(genBanking.State) { (bs: Banking.State) =>
      val agg         = bs.aggregate
      val expectedDep = bs.banks.map(_.deposits.toDouble).sum
      agg.deposits.toDouble shouldBe expectedDep +- 1.0
    }

  it should "have capital equal to sum of individual capital" in
    forAll(genBanking.State) { (bs: Banking.State) =>
      val agg         = bs.aggregate
      val expectedCap = bs.banks.map(_.capital.toDouble).sum
      agg.capital.toDouble shouldBe expectedCap +- 1.0
    }

  // ---- Failed banks stay failed ----

  "checkFailures" should "never un-fail a bank" in
    forAll(genBanking.State) { (bs: Banking.State) =>
      val failedBefore = bs.banks.filter(_.failed).map(_.id).toSet
      val afterCheck   = Banking.checkFailures(bs.banks, 50, enabled = true, Rate.Zero)
      val failedAfter  = afterCheck.banks.filter(_.failed).map(_.id).toSet
      failedBefore.subsetOf(failedAfter) shouldBe true
    }

  // ---- Reserves non-negative after clearing ----

  "clearInterbank" should "keep reserves non-negative" in
    forAll(genBanking.State) { (bs: Banking.State) =>
      val cleared = Banking.clearInterbank(bs.banks, bs.configs)
      cleared.foreach(_.reservesAtNbp should be >= PLN.Zero)
    }

  // ---- Initialize preserves totals ----

  "initialize" should "preserve total deposits and capital" in
    forAll(Gen.choose(1e5, 1e10), Gen.choose(1e4, 1e9)) { (totalDep: Double, totalCap: Double) =>
      val bs = testBankingSector(totalDeposits = PLN(totalDep), totalCapital = PLN(totalCap), totalLoans = PLN.Zero, configs = configs)
      bs.banks.map(_.deposits.toDouble).sum shouldBe totalDep +- 1.0
      bs.banks.map(_.capital.toDouble).sum shouldBe totalCap +- 1.0
    }

  // ---- QE purchases don't exceed bond holdings ----

  "allocateQePurchases" should "not make any bank's bond holdings negative" in
    forAll(genBanking.State, Gen.choose(0.0, 1e9)) { (bs: Banking.State, qe: Double) =>
      val result = Banking.allocateQePurchases(bs.banks, PLN(qe))
      result.foreach(_.govBondHoldings should be >= PLN.Zero)
    }
