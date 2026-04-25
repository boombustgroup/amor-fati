package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
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

  private val update = OpenEconEconomics.updateWeightedCouponPublic
  private val bonds  = PLN("100e9")

  "updateWeightedCoupon" should "not reprice instantly after a yield shock" in {
    val after = update(Rate("0.05"), Rate("0.10"), bonds, PLN.Zero, 54)
    decimal(after) shouldBe BigDecimal("0.05") +- BigDecimal("0.002")
    decimal(after) should be > BigDecimal("0.05")
    decimal(after) should be < BigDecimal("0.06")
  }

  it should "close 63% of gap after avgMaturity months (1 - 1/e)" in {
    // Geometric convergence: after M months, remaining gap ~= gap0 * (1-1/M)^M ~= gap0/e
    var coupon     = Rate("0.05")
    for _ <- 1 to 54 do coupon = update(coupon, Rate("0.10"), bonds, PLN.Zero, 54)
    val closedFrac = (decimal(coupon) - BigDecimal("0.05")) / BigDecimal("0.05")
    closedFrac shouldBe BigDecimal("0.63") +- BigDecimal("0.05")
  }

  it should "close 86% of gap after 2x avgMaturity months" in {
    var coupon     = Rate("0.03")
    for _ <- 1 to 108 do coupon = update(coupon, Rate("0.08"), bonds, PLN.Zero, 54)
    val closedFrac = (decimal(coupon) - BigDecimal("0.03")) / BigDecimal("0.05")
    closedFrac shouldBe BigDecimal("0.86") +- BigDecimal("0.05")
  }

  it should "stay stable when market yield equals coupon" in {
    val stable = Rate("0.055")
    decimal(update(stable, stable, bonds, PLN.Zero, 54)) shouldBe BigDecimal("0.055") +- BigDecimal("1e-4")
  }

  it should "accelerate convergence with large deficit" in {
    val noDeficit    = update(Rate("0.05"), Rate("0.10"), bonds, PLN.Zero, 54)
    val largeDeficit = update(Rate("0.05"), Rate("0.10"), bonds, PLN("20e9"), 54)
    decimal(largeDeficit) should be > decimal(noDeficit)
  }

  it should "handle zero bonds outstanding gracefully" in {
    decimal(update(Rate("0.05"), Rate("0.10"), PLN.Zero, PLN.Zero, 54)) shouldBe BigDecimal("0.05") +- BigDecimal("0.01")
  }

  it should "reprice fully with maturity = 1" in {
    decimal(update(Rate("0.05"), Rate("0.10"), bonds, PLN.Zero, 1)) shouldBe BigDecimal("0.10") +- BigDecimal("1e-4")
  }

  it should "converge monotonically upward (no overshooting)" in {
    var coupon = Rate("0.03")
    for _ <- 1 to 120 do
      val next = update(coupon, Rate("0.08"), bonds, PLN.Zero, 54)
      decimal(next) should be >= decimal(coupon)
      decimal(next) should be <= BigDecimal("0.08")
      coupon = next
  }

  it should "converge monotonically downward for yield decrease" in {
    var coupon     = Rate("0.10")
    for _ <- 1 to 54 do
      val next = update(coupon, Rate("0.03"), bonds, PLN.Zero, 54)
      decimal(next) should be <= decimal(coupon)
      decimal(next) should be >= BigDecimal("0.03")
      coupon = next
    // After 54 months: 63% of 7pp gap closed
    val closedFrac = (BigDecimal("0.10") - decimal(coupon)) / BigDecimal("0.07")
    closedFrac shouldBe BigDecimal("0.63") +- BigDecimal("0.05")
  }

  it should "have half-life approximately avgMaturity * ln(2) months" in {
    var coupon       = Rate("0.05")
    for _ <- 1 to 37 do coupon = update(coupon, Rate("0.10"), bonds, PLN.Zero, 54)
    val remainingGap = BigDecimal("0.10") - decimal(coupon)
    (remainingGap / BigDecimal("0.05")) shouldBe BigDecimal("0.50") +- BigDecimal("0.15")
  }
