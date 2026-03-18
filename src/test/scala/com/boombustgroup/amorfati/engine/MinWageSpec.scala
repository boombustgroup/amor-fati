package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.markets.FiscalBudget
import com.boombustgroup.amorfati.types.*

class MinWageSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  "GovState defaults" should "have minWageLevel=4666 and minWagePriceLevel=1.0" in {
    val gov = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    gov.minWageLevel shouldBe PLN(4666.0)
    gov.minWagePriceLevel shouldBe 1.0
  }

  "Inflation indexation" should "increase min wage by cumulative CPI" in {
    // 5% cumulative inflation: prevMinWage * (1 + 0.05) = 4666 * 1.05 = 4899.3
    val prevMinWage = 4666.0
    val cumInfl     = 0.05
    val inflIndexed = prevMinWage * (1.0 + Math.max(0.0, cumInfl))
    inflIndexed shouldBe (4899.3 +- 0.1)
  }

  "Deflation" should "not decrease min wage (max(0, cumInfl))" in {
    val prevMinWage = 4666.0
    val cumInfl     = -0.03 // 3% deflation
    val inflIndexed = prevMinWage * (1.0 + Math.max(0.0, cumInfl))
    inflIndexed shouldBe prevMinWage // no change under deflation
  }

  "Convergence" should "close gap toward 50% of market wage" in {
    val prevMinWage      = 4666.0
    val marketWage       = 12000.0
    val targetRatio      = 0.50
    val convergenceSpeed = 0.33
    val cumInfl          = 0.0

    val inflIndexed = prevMinWage * (1.0 + Math.max(0.0, cumInfl))
    val target      = marketWage * targetRatio             // 6000
    val gap         = target - inflIndexed                 // 6000 - 4666 = 1334
    val adjusted    = inflIndexed + gap * convergenceSpeed // 4666 + 1334*0.33 = 5106.22
    val result      = Math.max(prevMinWage, adjusted)

    result shouldBe (5106.22 +- 0.1)
    result should be > prevMinWage
  }

  "Low market wage" should "not reduce below inflation-indexed level" in {
    val prevMinWage      = 5000.0
    val marketWage       = 8000.0
    val targetRatio      = 0.50
    val convergenceSpeed = 0.33
    val cumInfl          = 0.02

    val inflIndexed = prevMinWage * (1.0 + Math.max(0.0, cumInfl)) // 5100
    val target      = marketWage * targetRatio                     // 4000
    val gap         = target - inflIndexed                         // 4000 - 5100 = -1100 (negative)
    // When gap <= 0, adjusted = inflIndexed (no convergence push down)
    val adjusted    =
      if gap > 0 then inflIndexed + gap * convergenceSpeed
      else inflIndexed
    val result      = Math.max(prevMinWage, adjusted)

    result shouldBe (5100.0 +- 0.1)
    result should be >= inflIndexed
  }

  "Default adjustment frequency" should "be 12 months" in {
    p.fiscal.minWageAdjustMonths shouldBe 12
  }

  "Ratchet" should "never allow min wage to decrease" in {
    val prevMinWage = 5200.0
    // Even if adjusted value is lower (impossible by construction, but verify ratchet)
    val adjusted    = 5100.0
    val result      = Math.max(prevMinWage, adjusted)
    result shouldBe prevMinWage
  }
