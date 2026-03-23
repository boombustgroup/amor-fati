package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.mechanisms.Macroprudential
import com.boombustgroup.amorfati.types.*

/** Unit tests for macroprudential instruments. */
class MacroprudentialSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  // ==========================================================================
  // Macroprudential.State
  // ==========================================================================

  "Macroprudential.State.zero" should "have all zeros" in {
    val s = Macroprudential.State.zero
    s.ccyb shouldBe Multiplier.Zero
    s.creditToGdpGap shouldBe Coefficient.Zero
    s.creditToGdpTrend shouldBe Multiplier.Zero
  }

  // ==========================================================================
  // OSII buffer (internal — bypasses Config guard)
  // ==========================================================================

  "osiiBufferImpl" should "return 1.0% for PKO BP (id=0)" in {
    td.toDouble(Macroprudential.osiiBufferImpl(0)) shouldBe 0.01 +- 1e-10
  }

  it should "return 0.5% for Pekao (id=1)" in {
    td.toDouble(Macroprudential.osiiBufferImpl(1)) shouldBe 0.005 +- 1e-10
  }

  it should "return 0% for other banks (id>=2)" in {
    for id <- 2 to 6 do Macroprudential.osiiBufferImpl(id) shouldBe Multiplier.Zero
  }

  // ==========================================================================
  // effectiveMinCar (internal)
  // ==========================================================================

  "effectiveMinCarImpl" should "equal MinCar + P2R when ccyb=0 and OSII=0" in {
    // Bank id=3 has no OSII buffer, but has P2R
    val eff = Macroprudential.effectiveMinCarImpl(3, Multiplier.Zero)
    td.toDouble(eff) shouldBe (td.toDouble(p.banking.minCar) + td.toDouble(p.banking.p2rAddons(3))) +- 1e-10
  }

  it should "add CCyB to MinCar + P2R" in {
    val ccyb = Multiplier(0.015)
    val eff  = Macroprudential.effectiveMinCarImpl(3, ccyb)
    td.toDouble(eff) shouldBe (td.toDouble(p.banking.minCar) + 0.015 + td.toDouble(p.banking.p2rAddons(3))) +- 1e-10
  }

  it should "add both CCyB and OSII and P2R for PKO BP" in {
    val ccyb = Multiplier(0.01)
    val eff  = Macroprudential.effectiveMinCarImpl(0, ccyb)
    td.toDouble(eff) shouldBe (td.toDouble(p.banking.minCar) + 0.01 + 0.01 + td.toDouble(p.banking.p2rAddons(0))) +- 1e-10
  }

  it should "add CCyB and OSII and P2R for Pekao" in {
    val ccyb = Multiplier(0.02)
    val eff  = Macroprudential.effectiveMinCarImpl(1, ccyb)
    td.toDouble(eff) shouldBe (td.toDouble(p.banking.minCar) + 0.02 + 0.005 + td.toDouble(p.banking.p2rAddons(1))) +- 1e-10
  }

  // ==========================================================================
  // stepImpl — CCyB computation
  // ==========================================================================

  "stepImpl" should "initialize trend on first call" in {
    val prev   = Macroprudential.State.zero
    val result = Macroprudential.stepImpl(prev, PLN(1000.0), PLN(100.0))
    // creditToGdp = 1000 / (100*12) = 0.8333
    // First call: trend = creditToGdp (since prev trend = 0)
    td.toDouble(result.creditToGdpTrend) shouldBe (1000.0 / 1200.0) +- 0.01
    // Gap = actual - trend = 0 on first call
    td.toDouble(result.creditToGdpGap) shouldBe 0.0 +- 0.01
  }

  it should "maintain ccyb=0 when gap is within neutral zone" in {
    val prev       = Macroprudential.State(Multiplier.Zero, Coefficient.Zero, Multiplier(0.5)) // trend already established
    // creditToGdp ≈ trend → gap ≈ 0 → within neutral zone
    val totalLoans = PLN(0.5 * 100.0 * 12.0)                                                   // exactly at trend
    val result     = Macroprudential.stepImpl(prev, totalLoans, PLN(100.0))
    td.toDouble(result.ccyb) shouldBe 0.0
  }

  it should "build CCyB gradually when gap exceeds activation threshold" in {
    // Set up so credit-to-GDP ratio is much higher than trend
    // trend = 0.30, make creditToGdp ≈ 0.40 → gap ≈ 0.10 > 0.02
    val prev   = Macroprudential.State(Multiplier.Zero, Coefficient.Zero, Multiplier(0.30))
    // creditToGdp = totalLoans / (gdp*12) = X / 1200
    // We want gap = creditToGdp - newTrend > 0.02
    // newTrend = 0.30*0.95 + creditToGdp*0.05
    // gap = creditToGdp - (0.285 + 0.05*creditToGdp) = 0.95*creditToGdp - 0.285
    // gap > 0.02 → creditToGdp > 0.305/0.95 ≈ 0.321
    // totalLoans = 0.40 * 1200 = 480
    val result = Macroprudential.stepImpl(prev, PLN(480.0), PLN(100.0))
    td.toDouble(result.ccyb) should be > 0.0
    td.toDouble(result.ccyb) should be <= td.toDouble(p.banking.ccybMax)
  }

  it should "release CCyB immediately when gap falls below release threshold" in {
    // trend = 0.50, make creditToGdp very low → gap < -0.02
    val prev   = Macroprudential.State(Multiplier(0.02), Coefficient.Zero, Multiplier(0.50))
    // creditToGdp = 10 / 1200 ≈ 0.0083
    // newTrend = 0.50*0.95 + 0.0083*0.05 ≈ 0.4754
    // gap = 0.0083 - 0.4754 ≈ -0.467 < -0.02
    val result = Macroprudential.stepImpl(prev, PLN(10.0), PLN(100.0))
    td.toDouble(result.ccyb) shouldBe 0.0 // immediate release
  }

  it should "cap CCyB at CcybMax" in {
    // Start near max and build further
    val prev   = Macroprudential.State(Multiplier(0.024), Coefficient.Zero, Multiplier(0.30))
    val result = Macroprudential.stepImpl(prev, PLN(480.0), PLN(100.0))
    td.toDouble(result.ccyb) should be <= td.toDouble(p.banking.ccybMax)
  }

  // ==========================================================================
  // withinConcentrationLimitImpl
  // ==========================================================================

  "withinConcentrationLimitImpl" should "return true when loan share is below limit" in {
    // 100/1000 = 10% < 25%
    Macroprudential.withinConcentrationLimitImpl(100.0, 500.0, 1000.0) shouldBe true
  }

  it should "return false when loan share exceeds limit" in {
    // 300/1000 = 30% > 25% concentration limit
    Macroprudential.withinConcentrationLimitImpl(300.0, 500.0, 1000.0) shouldBe false
  }

  it should "return true when total system loans is zero" in {
    Macroprudential.withinConcentrationLimitImpl(100.0, 500.0, 0.0) shouldBe true
  }
