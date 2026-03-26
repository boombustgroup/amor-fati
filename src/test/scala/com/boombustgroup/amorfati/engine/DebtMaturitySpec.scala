package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.engine.economics.OpenEconEconomics
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tests for the rolling-portfolio WAM (Weighted Average Maturity) model.
  *
  * The WAM model governs how yield shocks propagate to government debt service.
  * Each month, 1/avgMaturity of the bond portfolio matures and is refinanced at
  * the current market yield. The weighted coupon converges gradually — not
  * instantly — to market yield.
  */
class DebtMaturitySpec extends AnyFlatSpec with Matchers:

  private val td     = ComputationBoundary
  private val update = OpenEconEconomics.updateWeightedCouponPublic
  private val bonds  = PLN(100e9)

  "updateWeightedCoupon" should "not reprice instantly after a yield shock" in {
    val after = update(Rate(0.05), Rate(0.10), bonds, PLN.Zero, 54)
    td.toDouble(after) shouldBe 0.05 +- 0.002
    td.toDouble(after) should be > 0.05
    td.toDouble(after) should be < 0.06
  }

  it should "close 63% of gap after avgMaturity months (1 - 1/e)" in {
    // Geometric convergence: after M months, remaining gap ~= gap0 * (1-1/M)^M ~= gap0/e
    var coupon     = Rate(0.05)
    for _ <- 1 to 54 do coupon = update(coupon, Rate(0.10), bonds, PLN.Zero, 54)
    val closedFrac = (td.toDouble(coupon) - 0.05) / 0.05
    closedFrac shouldBe 0.63 +- 0.05
  }

  it should "close 86% of gap after 2x avgMaturity months" in {
    var coupon     = Rate(0.03)
    for _ <- 1 to 108 do coupon = update(coupon, Rate(0.08), bonds, PLN.Zero, 54)
    val closedFrac = (td.toDouble(coupon) - 0.03) / 0.05
    closedFrac shouldBe 0.86 +- 0.05
  }

  it should "stay stable when market yield equals coupon" in {
    val stable = Rate(0.055)
    td.toDouble(update(stable, stable, bonds, PLN.Zero, 54)) shouldBe 0.055 +- 1e-4
  }

  it should "accelerate convergence with large deficit" in {
    val noDeficit    = update(Rate(0.05), Rate(0.10), bonds, PLN.Zero, 54)
    val largeDeficit = update(Rate(0.05), Rate(0.10), bonds, PLN(20e9), 54)
    td.toDouble(largeDeficit) should be > td.toDouble(noDeficit)
  }

  it should "handle zero bonds outstanding gracefully" in {
    td.toDouble(update(Rate(0.05), Rate(0.10), PLN.Zero, PLN.Zero, 54)) shouldBe 0.05 +- 0.01
  }

  it should "reprice fully with maturity = 1" in {
    td.toDouble(update(Rate(0.05), Rate(0.10), bonds, PLN.Zero, 1)) shouldBe 0.10 +- 1e-4
  }

  it should "converge monotonically upward (no overshooting)" in {
    var coupon = Rate(0.03)
    for _ <- 1 to 120 do
      val next = update(coupon, Rate(0.08), bonds, PLN.Zero, 54)
      td.toDouble(next) should be >= td.toDouble(coupon)
      td.toDouble(next) should be <= 0.08
      coupon = next
  }

  it should "converge monotonically downward for yield decrease" in {
    var coupon     = Rate(0.10)
    for _ <- 1 to 54 do
      val next = update(coupon, Rate(0.03), bonds, PLN.Zero, 54)
      td.toDouble(next) should be <= td.toDouble(coupon)
      td.toDouble(next) should be >= 0.03
      coupon = next
    // After 54 months: 63% of 7pp gap closed
    val closedFrac = (0.10 - td.toDouble(coupon)) / 0.07
    closedFrac shouldBe 0.63 +- 0.05
  }

  it should "have half-life approximately avgMaturity * ln(2) months" in {
    var coupon       = Rate(0.05)
    for _ <- 1 to 37 do coupon = update(coupon, Rate(0.10), bonds, PLN.Zero, 54)
    val remainingGap = 0.10 - td.toDouble(coupon)
    (remainingGap / 0.05) shouldBe 0.50 +- 0.15
  }
