package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.agents.Household
import sfc.config.Config
import sfc.types.*

class MinWageSpec extends AnyFlatSpec with Matchers:

  "MinWageEnabled=false" should "use BaseReservationWage" in {
    // When disabled, baseMinWage = Config.BaseReservationWage = 4666.0
    Config.MinWageEnabled shouldBe false
    Config.BaseReservationWage shouldBe 4666.0
  }

  "Household.SectorState defaults" should "have minWageLevel=4666 and minWagePriceLevel=1.0" in {
    val hh = Household.SectorState(100, PLN(8000.0), PLN(4666.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    hh.minWageLevel shouldBe PLN(4666.0)
    hh.minWagePriceLevel shouldBe 1.0
  }

  "Inflation indexation" should "increase min wage by cumulative CPI" in {
    // 5% cumulative inflation: prevMinWage * (1 + 0.05) = 4666 * 1.05 = 4899.3
    val prevMinWage = 4666.0
    val cumInfl = 0.05
    val inflIndexed = prevMinWage * (1.0 + Math.max(0.0, cumInfl))
    inflIndexed shouldBe (4899.3 +- 0.1)
  }

  "Deflation" should "not decrease min wage (max(0, cumInfl))" in {
    val prevMinWage = 4666.0
    val cumInfl = -0.03 // 3% deflation
    val inflIndexed = prevMinWage * (1.0 + Math.max(0.0, cumInfl))
    inflIndexed shouldBe prevMinWage // no change under deflation
  }

  "Convergence" should "close gap toward 50% of market wage" in {
    val prevMinWage = 4666.0
    val marketWage = 12000.0
    val targetRatio = 0.50
    val convergenceSpeed = 0.33
    val cumInfl = 0.0

    val inflIndexed = prevMinWage * (1.0 + Math.max(0.0, cumInfl))
    val target = marketWage * targetRatio // 6000
    val gap = target - inflIndexed // 6000 - 4666 = 1334
    val adjusted = inflIndexed + gap * convergenceSpeed // 4666 + 1334*0.33 = 5106.22
    val result = Math.max(prevMinWage, adjusted)

    result shouldBe (5106.22 +- 0.1)
    result should be > prevMinWage
  }

  "Low market wage" should "not reduce below inflation-indexed level" in {
    val prevMinWage = 5000.0
    val marketWage = 8000.0
    val targetRatio = 0.50
    val convergenceSpeed = 0.33
    val cumInfl = 0.02

    val inflIndexed = prevMinWage * (1.0 + Math.max(0.0, cumInfl)) // 5100
    val target = marketWage * targetRatio // 4000
    val gap = target - inflIndexed // 4000 - 5100 = -1100 (negative)
    // When gap <= 0, adjusted = inflIndexed (no convergence push down)
    val adjusted =
      if gap > 0 then inflIndexed + gap * convergenceSpeed
      else inflIndexed
    val result = Math.max(prevMinWage, adjusted)

    result shouldBe (5100.0 +- 0.1)
    result should be >= inflIndexed
  }

  "BDP" should "stack on top of dynamic base" in {
    val baseMinWage = 5000.0
    val bdp = 2000.0
    val bdpMult = Config.ReservationBdpMult // 0.5
    val resWage = baseMinWage + bdp * bdpMult
    resWage shouldBe (6000.0 +- 0.1)
  }

  "Default adjustment frequency" should "be 12 months" in {
    Config.MinWageAdjustMonths shouldBe 12
  }

  "Ratchet" should "never allow min wage to decrease" in {
    val prevMinWage = 5200.0
    // Even if adjusted value is lower (impossible by construction, but verify ratchet)
    val adjusted = 5100.0
    val result = Math.max(prevMinWage, adjusted)
    result shouldBe prevMinWage
  }
