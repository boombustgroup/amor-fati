package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import sfc.testutil.Generators.*
import sfc.config.Config

class HouseholdPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // --- giniSorted properties ---

  "giniSorted" should "be in [0, 1] for any non-negative sorted array" in {
    forAll(genSortedArrayWithSize) { (arr: Array[Double]) =>
      val g = HouseholdLogic.giniSorted(arr)
      g should be >= 0.0
      g should be <= (1.0 + 1e-10)
    }
  }

  it should "be 0 for uniform arrays" in {
    forAll(Gen.choose(2, 100), Gen.choose(1.0, 10000.0)) { (n: Int, v: Double) =>
      val arr = Array.fill(n)(v)
      HouseholdLogic.giniSorted(arr) shouldBe (0.0 +- 1e-10)
    }
  }

  it should "be 0 for single-element arrays" in {
    forAll(Gen.choose(0.0, 10000.0)) { (v: Double) =>
      HouseholdLogic.giniSorted(Array(v)) shouldBe 0.0
    }
  }

  it should "be 0 for empty-like arrays (n <= 1)" in {
    HouseholdLogic.giniSorted(Array.empty[Double]) shouldBe 0.0
    forAll(Gen.choose(0.0, 10000.0)) { (v: Double) =>
      HouseholdLogic.giniSorted(Array(v)) shouldBe 0.0
    }
  }

  it should "increase when adding an outlier (monotonic with inequality)" in {
    forAll(Gen.choose(5, 50), Gen.choose(100.0, 1000.0)) { (n: Int, v: Double) =>
      val uniform = Array.fill(n)(v).sorted
      val withOutlier = (Array.fill(n)(v) :+ (v * 100)).sorted
      HouseholdLogic.giniSorted(withOutlier) should be > HouseholdLogic.giniSorted(uniform)
    }
  }

  it should "handle negatives via shift and still be in [0, 1]" in {
    forAll(Gen.choose(2, 50)) { (n: Int) =>
      forAll(Gen.listOfN(n, Gen.choose(-10000.0, 10000.0))) { (list: List[Double]) =>
        val arr = list.toArray.sorted
        val g = HouseholdLogic.giniSorted(arr)
        g should be >= 0.0
        g should be <= (1.0 + 1e-10)
      }
    }
  }

  // --- computeBenefit properties ---

  "computeBenefit" should "be >= 0" in {
    forAll(Gen.choose(0, 24)) { (months: Int) =>
      HouseholdLogic.computeBenefit(months) should be >= 0.0
    }
  }

  it should "be weakly decreasing in months" in {
    forAll(Gen.choose(0, 23)) { (months: Int) =>
      HouseholdLogic.computeBenefit(months) should be >= HouseholdLogic.computeBenefit(months + 1)
    }
  }

  it should "be 0 after GovBenefitDuration" in {
    forAll(Gen.choose(Config.GovBenefitDuration + 1, 100)) { (months: Int) =>
      HouseholdLogic.computeBenefit(months) shouldBe 0.0
    }
  }

  // --- computeAggregates properties ---

  "computeAggregates" should "have employed + unemployed + retraining + bankrupt = n" in {
    forAll(Gen.choose(5, 50)) { (n: Int) =>
      forAll(Gen.listOfN(n, genHousehold)) { (hhList: List[Household]) =>
        val hhs = hhList.toVector
        val agg = HouseholdLogic.computeAggregates(hhs, 8266.0, 4666.0, 0.40, 0, 0)
        (agg.employed + agg.unemployed + agg.retraining + agg.bankrupt) shouldBe n
      }
    }
  }

  it should "have consumptionP10 <= P50 <= P90" in {
    forAll(Gen.choose(10, 50)) { (n: Int) =>
      forAll(Gen.listOfN(n, genHousehold)) { (hhList: List[Household]) =>
        val hhs = hhList.toVector
        val agg = HouseholdLogic.computeAggregates(hhs, 8266.0, 4666.0, 0.40, 0, 0)
        agg.consumptionP10 should be <= agg.consumptionP50
        agg.consumptionP50 should be <= agg.consumptionP90
      }
    }
  }

  it should "have povertyRate30 and povertyRate50 in [0, 1]" in {
    forAll(Gen.choose(5, 50)) { (n: Int) =>
      forAll(Gen.listOfN(n, genHousehold)) { (hhList: List[Household]) =>
        val hhs = hhList.toVector
        val agg = HouseholdLogic.computeAggregates(hhs, 8266.0, 4666.0, 0.40, 0, 0)
        agg.povertyRate30 should be >= 0.0
        agg.povertyRate30 should be <= 1.0
        agg.povertyRate50 should be >= 0.0
        agg.povertyRate50 should be <= 1.0
      }
    }
  }

  it should "have poverty30 <= poverty50" in {
    forAll(Gen.choose(5, 50)) { (n: Int) =>
      forAll(Gen.listOfN(n, genHousehold)) { (hhList: List[Household]) =>
        val hhs = hhList.toVector
        val agg = HouseholdLogic.computeAggregates(hhs, 8266.0, 4666.0, 0.40, 0, 0)
        agg.povertyRate30 should be <= (agg.povertyRate50 + 1e-10)
      }
    }
  }

  it should "have bankruptcyRate in [0, 1]" in {
    forAll(Gen.choose(5, 50)) { (n: Int) =>
      forAll(Gen.listOfN(n, genHousehold)) { (hhList: List[Household]) =>
        val hhs = hhList.toVector
        val agg = HouseholdLogic.computeAggregates(hhs, 8266.0, 4666.0, 0.40, 0, 0)
        agg.bankruptcyRate should be >= 0.0
        agg.bankruptcyRate should be <= 1.0
      }
    }
  }

  it should "have positive meanSavings when all savings are positive" in {
    forAll(Gen.choose(5, 30)) { (n: Int) =>
      val positiveHhGen = genHousehold.map(h => h.copy(savings = Math.abs(h.savings) + 1.0))
      forAll(Gen.listOfN(n, positiveHhGen)) { (hhList: List[Household]) =>
        val hhs = hhList.toVector
        val agg = HouseholdLogic.computeAggregates(hhs, 8266.0, 4666.0, 0.40, 0, 0)
        agg.meanSavings should be > 0.0
      }
    }
  }

  // --- Bankrupt is absorbing barrier ---

  "Bankrupt" should "be an absorbing barrier" in {
    forAll(Gen.choose(1, 20)) { (n: Int) =>
      val bankruptHhs = (0 until n).map { i =>
        Household(i, -10000.0, 5000.0, 1800.0, 0.5, 0.3, 0.8, HhStatus.Bankrupt, Array.empty)
      }.toVector
      val agg = HouseholdLogic.computeAggregates(bankruptHhs, 8266.0, 4666.0, 0.40, 0, 0)
      agg.bankrupt shouldBe n
      agg.employed shouldBe 0
      agg.unemployed shouldBe 0
      agg.retraining shouldBe 0
    }
  }
