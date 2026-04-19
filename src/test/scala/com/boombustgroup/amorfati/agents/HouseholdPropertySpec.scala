package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.TestHouseholdState

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.types.*

class HouseholdPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private def computeAggregates(
      households: Vector[Household.State],
      financialStocks: Vector[Household.FinancialStocks] = Vector.empty,
  ): Household.Aggregates =
    Household.computeAggregates(
      households,
      if financialStocks.nonEmpty then financialStocks else Vector.fill(households.length)(TestHouseholdState.financial(savings = PLN(10000.0))),
      PLN(8266.0),
      PLN(4666.0),
      Share(0.40),
      0,
      0,
    )

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // --- giniSorted properties ---

  "giniSorted" should "be in [0, 1] for any non-negative sorted array" in
    forAll(genSortedArrayWithSize.map(_.map(v => Math.round(v)))) { (arr: Array[Long]) =>
      val g = Household.giniSorted(arr)
      g should be >= Share.Zero
      g should be <= Share.One
    }

  it should "be 0 for uniform arrays" in
    forAll(Gen.choose(2, 100), Gen.choose(1.0, 10000.0)) { (n: Int, v: Double) =>
      val arr = Array.fill(n)(Math.round(v))
      Household.giniSorted(arr) shouldBe Share.Zero
    }

  it should "be 0 for single-element arrays" in
    forAll(Gen.choose(0.0, 10000.0)) { (v: Double) =>
      Household.giniSorted(Array(Math.round(v))) shouldBe Share.Zero
    }

  it should "be 0 for empty-like arrays (n <= 1)" in {
    Household.giniSorted(Array.emptyLongArray) shouldBe Share.Zero
    forAll(Gen.choose(0.0, 10000.0)) { (v: Double) =>
      Household.giniSorted(Array(Math.round(v))) shouldBe Share.Zero
    }
  }

  it should "increase when adding an outlier (monotonic with inequality)" in
    forAll(Gen.choose(5, 50), Gen.choose(100.0, 1000.0)) { (n: Int, v: Double) =>
      val uniform     = Array.fill(n)(Math.round(v)).sorted
      val withOutlier = (Array.fill(n)(Math.round(v)) :+ Math.round(v * 100)).sorted
      Household.giniSorted(withOutlier) should be > Household.giniSorted(uniform)
    }

  it should "handle negatives via shift and still be in [0, 1]" in
    forAll(Gen.choose(2, 50)) { (n: Int) =>
      forAll(Gen.listOfN(n, Gen.choose(-10000.0, 10000.0))) { (list: List[Double]) =>
        val arr = list.map(Math.round).toArray.sorted
        val g   = Household.giniSorted(arr)
        g should be >= Share.Zero
        g should be <= Share.One
      }
    }

  // --- computeBenefit properties ---

  "computeBenefit" should "be >= 0" in
    forAll(Gen.choose(0, 24)) { (months: Int) =>
      Household.computeBenefit(months) should be >= PLN.Zero
    }

  it should "be weakly decreasing in months" in
    forAll(Gen.choose(0, 23)) { (months: Int) =>
      Household.computeBenefit(months) should be >= Household.computeBenefit(months + 1)
    }

  it should "be 0 after GovBenefitDuration" in
    forAll(Gen.choose(p.fiscal.govBenefitDuration + 1, 100)) { (months: Int) =>
      Household.computeBenefit(months) shouldBe PLN.Zero
    }

  // --- computeAggregates properties ---

  "computeAggregates" should "have employed + unemployed + retraining + bankrupt = n" in
    forAll(Gen.choose(5, 50)) { (n: Int) =>
      forAll(Gen.listOfN(n, genHousehold)) { (hhList: List[Household.State]) =>
        val hhs = hhList.toVector
        val agg = computeAggregates(hhs)
        (agg.employed + agg.unemployed + agg.retraining + agg.bankrupt) shouldBe n
      }
    }

  it should "have consumptionP10 <= P50 <= P90" in
    forAll(Gen.choose(10, 50)) { (n: Int) =>
      forAll(Gen.listOfN(n, genHousehold)) { (hhList: List[Household.State]) =>
        val hhs = hhList.toVector
        val agg = computeAggregates(hhs)
        agg.consumptionP10 should be <= agg.consumptionP50
        agg.consumptionP50 should be <= agg.consumptionP90
      }
    }

  it should "have povertyRate30 and povertyRate50 in [0, 1]" in
    forAll(Gen.choose(5, 50)) { (n: Int) =>
      forAll(Gen.listOfN(n, genHousehold)) { (hhList: List[Household.State]) =>
        val hhs = hhList.toVector
        val agg = computeAggregates(hhs)
        agg.povertyRate30 should be >= Share.Zero
        agg.povertyRate30 should be <= Share.One
        agg.povertyRate50 should be >= Share.Zero
        agg.povertyRate50 should be <= Share.One
      }
    }

  it should "have poverty30 <= poverty50" in
    forAll(Gen.choose(5, 50)) { (n: Int) =>
      forAll(Gen.listOfN(n, genHousehold)) { (hhList: List[Household.State]) =>
        val hhs = hhList.toVector
        val agg = computeAggregates(hhs)
        agg.povertyRate30 should be <= agg.povertyRate50
      }
    }

  it should "have bankruptcyRate in [0, 1]" in
    forAll(Gen.choose(5, 50)) { (n: Int) =>
      forAll(Gen.listOfN(n, genHousehold)) { (hhList: List[Household.State]) =>
        val hhs = hhList.toVector
        val agg = computeAggregates(hhs)
        agg.bankruptcyRate should be >= Share.Zero
        agg.bankruptcyRate should be <= Share.One
      }
    }

  it should "have positive meanSavings when all savings are positive" in
    forAll(Gen.choose(5, 30)) { (n: Int) =>
      forAll(Gen.listOfN(n, genHousehold)) { (hhList: List[Household.State]) =>
        val hhs    = hhList.toVector
        val stocks = hhs.map(h => TestHouseholdState.financial(savings = PLN(Math.abs(h.id.toInt.toDouble) + 1.0)))
        val agg    = computeAggregates(hhs, stocks)
        agg.meanSavings should be > PLN.Zero
      }
    }

  // --- Bankrupt is absorbing barrier ---

  "Bankrupt" should "be an absorbing barrier" in
    forAll(Gen.choose(1, 20)) { (n: Int) =>
      val bankruptHhs = (0 until n).map { i =>
        TestHouseholdState(
          HhId(i),
          PLN(-10000.0),
          PLN(5000.0),
          PLN(1800.0),
          Share(0.5),
          Share(0.3),
          Share(0.8),
          HhStatus.Bankrupt,
          Array.empty[HhId],
          bankId = BankId(0),
          equityWealth = PLN.Zero,
          lastSectorIdx = SectorIdx(-1),
          isImmigrant = false,
          numDependentChildren = 0,
          consumerDebt = PLN.Zero,
          education = 2,
          taskRoutineness = Share(0.5),
          wageScar = Share.Zero,
        )
      }.toVector
      val agg         = computeAggregates(bankruptHhs)
      agg.bankrupt shouldBe n
      agg.employed shouldBe 0
      agg.unemployed shouldBe 0
      agg.retraining shouldBe 0
    }
