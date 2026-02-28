package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Unit tests for macroprudential instruments. */
class MacroprudentialSpec extends AnyFlatSpec with Matchers:

  // ==========================================================================
  // MacropruState
  // ==========================================================================

  "MacropruState.zero" should "have all zeros" in {
    val s = MacropruState.zero
    s.ccyb shouldBe 0.0
    s.creditToGdpGap shouldBe 0.0
    s.creditToGdpTrend shouldBe 0.0
  }

  // ==========================================================================
  // Disabled state (default: MACROPRU_ENABLED=false)
  // ==========================================================================

  "osiiBuffer (disabled)" should "return 0 for all banks" in {
    for id <- 0 to 6 do
      Macroprudential.osiiBuffer(id) shouldBe 0.0
  }

  "effectiveMinCar (disabled)" should "return base MinCar for all banks" in {
    for id <- 0 to 6 do
      Macroprudential.effectiveMinCar(id, 0.02) shouldBe sfc.config.Config.MinCar
  }

  "step (disabled)" should "return prev unchanged" in {
    val prev = MacropruState(0.01, 0.05, 0.40)
    val result = Macroprudential.step(prev, 1000.0, 100.0)
    result shouldBe prev
  }

  "withinConcentrationLimit (disabled)" should "always return true" in {
    Macroprudential.withinConcentrationLimit(900.0, 100.0, 1000.0) shouldBe true
  }

  // ==========================================================================
  // OSII buffer (internal — bypasses Config guard)
  // ==========================================================================

  "osiiBufferInternal" should "return 1.0% for PKO BP (id=0)" in {
    Macroprudential.osiiBufferInternal(0) shouldBe 0.01 +- 1e-10
  }

  it should "return 0.5% for Pekao (id=1)" in {
    Macroprudential.osiiBufferInternal(1) shouldBe 0.005 +- 1e-10
  }

  it should "return 0% for other banks (id>=2)" in {
    for id <- 2 to 6 do
      Macroprudential.osiiBufferInternal(id) shouldBe 0.0
  }

  // ==========================================================================
  // effectiveMinCar (internal)
  // ==========================================================================

  "effectiveMinCarInternal" should "equal MinCar when ccyb=0 and OSII=0" in {
    // Bank id=3 has no OSII buffer
    val eff = Macroprudential.effectiveMinCarInternal(3, 0.0)
    eff shouldBe sfc.config.Config.MinCar +- 1e-10
  }

  it should "add CCyB to MinCar" in {
    val ccyb = 0.015
    val eff = Macroprudential.effectiveMinCarInternal(3, ccyb)
    eff shouldBe (sfc.config.Config.MinCar + ccyb) +- 1e-10
  }

  it should "add both CCyB and OSII for PKO BP" in {
    val ccyb = 0.01
    val eff = Macroprudential.effectiveMinCarInternal(0, ccyb)
    eff shouldBe (sfc.config.Config.MinCar + ccyb + 0.01) +- 1e-10
  }

  it should "add CCyB and OSII for Pekao" in {
    val ccyb = 0.02
    val eff = Macroprudential.effectiveMinCarInternal(1, ccyb)
    eff shouldBe (sfc.config.Config.MinCar + ccyb + 0.005) +- 1e-10
  }

  // ==========================================================================
  // stepInternal — CCyB computation
  // ==========================================================================

  "stepInternal" should "initialize trend on first call" in {
    val prev = MacropruState.zero
    val result = Macroprudential.stepInternal(prev, 1000.0, 100.0)
    // creditToGdp = 1000 / (100*12) = 0.8333
    // First call: trend = creditToGdp (since prev trend = 0)
    result.creditToGdpTrend shouldBe (1000.0 / 1200.0) +- 0.01
    // Gap = actual - trend = 0 on first call
    result.creditToGdpGap shouldBe 0.0 +- 0.01
  }

  it should "maintain ccyb=0 when gap is within neutral zone" in {
    val prev = MacropruState(0.0, 0.0, 0.5)  // trend already established
    // creditToGdp ≈ trend → gap ≈ 0 → within neutral zone
    val totalLoans = 0.5 * 100.0 * 12.0  // exactly at trend
    val result = Macroprudential.stepInternal(prev, totalLoans, 100.0)
    result.ccyb shouldBe 0.0
  }

  it should "build CCyB gradually when gap exceeds activation threshold" in {
    // Set up so credit-to-GDP ratio is much higher than trend
    // trend = 0.30, make creditToGdp ≈ 0.40 → gap ≈ 0.10 > 0.02
    val prev = MacropruState(0.0, 0.0, 0.30)
    // creditToGdp = totalLoans / (gdp*12) = X / 1200
    // We want gap = creditToGdp - newTrend > 0.02
    // newTrend = 0.30*0.95 + creditToGdp*0.05
    // gap = creditToGdp - (0.285 + 0.05*creditToGdp) = 0.95*creditToGdp - 0.285
    // gap > 0.02 → creditToGdp > 0.305/0.95 ≈ 0.321
    // totalLoans = 0.40 * 1200 = 480
    val result = Macroprudential.stepInternal(prev, 480.0, 100.0)
    result.ccyb should be > 0.0
    result.ccyb should be <= sfc.config.Config.CcybMax
  }

  it should "release CCyB immediately when gap falls below release threshold" in {
    // trend = 0.50, make creditToGdp very low → gap < -0.02
    val prev = MacropruState(0.02, 0.0, 0.50)
    // creditToGdp = 10 / 1200 ≈ 0.0083
    // newTrend = 0.50*0.95 + 0.0083*0.05 ≈ 0.4754
    // gap = 0.0083 - 0.4754 ≈ -0.467 < -0.02
    val result = Macroprudential.stepInternal(prev, 10.0, 100.0)
    result.ccyb shouldBe 0.0  // immediate release
  }

  it should "cap CCyB at CcybMax" in {
    // Start near max and build further
    val prev = MacropruState(0.024, 0.0, 0.30)
    val result = Macroprudential.stepInternal(prev, 480.0, 100.0)
    result.ccyb should be <= sfc.config.Config.CcybMax
  }

  // ==========================================================================
  // withinConcentrationLimitInternal
  // ==========================================================================

  "withinConcentrationLimitInternal" should "return true when loan share is below limit" in {
    // 100/1000 = 10% < 25%
    Macroprudential.withinConcentrationLimitInternal(100.0, 500.0, 1000.0) shouldBe true
  }

  it should "return false when loan share exceeds limit" in {
    // 300/1000 = 30% > 25% concentration limit
    Macroprudential.withinConcentrationLimitInternal(300.0, 500.0, 1000.0) shouldBe false
  }

  it should "return true when total system loans is zero" in {
    Macroprudential.withinConcentrationLimitInternal(100.0, 500.0, 0.0) shouldBe true
  }
