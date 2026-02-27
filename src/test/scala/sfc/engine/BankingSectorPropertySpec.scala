package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import sfc.testutil.Generators.*
import sfc.config.Config

import scala.util.Random

class BankingSectorPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  private val configs = BankingSector.DefaultConfigs

  // ---- Interbank netting invariant ----

  "clearInterbank" should "always produce interbankNet that sums to zero" in {
    forAll(genBankingSectorState) { (bs: BankingSectorState) =>
      val cleared = BankingSector.clearInterbank(bs.banks, bs.configs, bs.interbankRate)
      val netSum = cleared.map(_.interbankNet).sum
      netSum shouldBe 0.0 +- 1.0  // tolerance for floating-point
    }
  }

  // ---- allocateBonds increment sums to deficit ----

  "allocateBonds" should "change total bond holdings by exactly deficit" in {
    forAll(genBankingSectorState, Gen.choose(-1e8, 1e8)) { (bs: BankingSectorState, deficit: Double) =>
      val before = bs.banks.map(_.govBondHoldings).sum
      val after = BankingSector.allocateBonds(bs.banks, deficit)
      val afterSum = after.map(_.govBondHoldings).sum
      (afterSum - before) shouldBe deficit +- 1.0
    }
  }

  // ---- lendingRate monotonic in NPL ----

  "lendingRate" should "be monotonically non-decreasing with NPL ratio" in {
    forAll(genRate, Gen.choose(0.0, 0.15), Gen.choose(0.0, 0.15)) {
      (refRate: Double, npl1Frac: Double, npl2Frac: Double) =>
        val loans = 1e6
        val (lo, hi) = if npl1Frac <= npl2Frac then (npl1Frac, npl2Frac) else (npl2Frac, npl1Frac)
        val bankLo = IndividualBankState(0, 1e6, loans, 2e5, loans * lo, 0, 0, 0, false, 0, 0)
        val bankHi = IndividualBankState(0, 1e6, loans, 2e5, loans * hi, 0, 0, 0, false, 0, 0)
        val rateLo = BankingSector.lendingRate(bankLo, configs(0), refRate)
        val rateHi = BankingSector.lendingRate(bankHi, configs(0), refRate)
        rateHi should be >= rateLo
    }
  }

  // ---- assignBank returns valid index ----

  "assignBank" should "always return valid index in [0, nBanks)" in {
    forAll(Gen.choose(0, 5)) { (sector: Int) =>
      val rng = new Random()
      for _ <- 0 until 50 do
        val bId = BankingSector.assignBank(sector, configs, rng)
        bId should be >= 0
        bId should be < configs.length
    }
  }

  // ---- aggregate.deposits == sum of individual deposits ----

  "aggregate" should "have deposits equal to sum of individual deposits" in {
    forAll(genBankingSectorState) { (bs: BankingSectorState) =>
      val agg = bs.aggregate
      val expectedDep = bs.banks.map(_.deposits).sum
      agg.deposits shouldBe expectedDep +- 1.0
    }
  }

  it should "have capital equal to sum of individual capital" in {
    forAll(genBankingSectorState) { (bs: BankingSectorState) =>
      val agg = bs.aggregate
      val expectedCap = bs.banks.map(_.capital).sum
      agg.capital shouldBe expectedCap +- 1.0
    }
  }

  // ---- Failed banks stay failed ----

  "checkFailures" should "never un-fail a bank" in {
    forAll(genBankingSectorState) { (bs: BankingSectorState) =>
      val failedBefore = bs.banks.filter(_.failed).map(_.id).toSet
      val (afterCheck, _) = BankingSector.checkFailures(bs.banks, 50, enabled = true)
      val failedAfter = afterCheck.filter(_.failed).map(_.id).toSet
      failedBefore.subsetOf(failedAfter) shouldBe true
    }
  }

  // ---- Reserves non-negative after clearing ----

  "clearInterbank" should "keep reserves non-negative" in {
    forAll(genBankingSectorState) { (bs: BankingSectorState) =>
      val cleared = BankingSector.clearInterbank(bs.banks, bs.configs, bs.interbankRate)
      cleared.foreach(_.reservesAtNbp should be >= 0.0)
    }
  }

  // ---- Initialize preserves totals ----

  "initialize" should "preserve total deposits and capital" in {
    forAll(Gen.choose(1e5, 1e10), Gen.choose(1e4, 1e9)) { (totalDep: Double, totalCap: Double) =>
      val bs = BankingSector.initialize(totalDep, totalCap, configs)
      bs.banks.map(_.deposits).sum shouldBe totalDep +- 1.0
      bs.banks.map(_.capital).sum shouldBe totalCap +- 1.0
    }
  }

  // ---- QE purchases don't exceed bond holdings ----

  "allocateQePurchases" should "not make any bank's bond holdings negative" in {
    forAll(genBankingSectorState, Gen.choose(0.0, 1e9)) { (bs: BankingSectorState, qe: Double) =>
      val result = BankingSector.allocateQePurchases(bs.banks, qe)
      result.foreach(_.govBondHoldings should be >= 0.0)
    }
  }
