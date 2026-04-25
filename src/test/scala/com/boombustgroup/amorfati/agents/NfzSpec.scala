package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NfzSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private val wage     = PLN("8000.0")
  private val employed = 80000

  "nfzStep" should "compute contributions as wage × employed × 9%" in {
    val result   = SocialSecurity.nfzStep(employed, wage, 100000, 10000)
    val expected = employed * (wage * p.social.nfzContribRate)
    result.contributions shouldBe expected
  }

  it should "increase spending with more retirees (aging pressure)" in {
    val fewRetirees  = SocialSecurity.nfzStep(employed, wage, 100000, 5000)
    val manyRetirees = SocialSecurity.nfzStep(employed, wage, 100000, 30000)
    manyRetirees.spending should be > fewRetirees.spending
  }

  it should "produce govSubvention when spending exceeds contributions" in {
    // Many retirees + low employment → deficit
    val result = SocialSecurity.nfzStep(10000, wage, 50000, 50000)
    result.spending should be > result.contributions
    result.govSubvention should be > PLN.Zero
  }

  it should "produce zero govSubvention when in surplus" in {
    // High employment + high wage + few retirees → surplus
    val result = SocialSecurity.nfzStep(100000, PLN("20000.0"), 100000, 100)
    result.contributions should be > result.spending
    result.govSubvention shouldBe PLN.Zero
  }

  it should "accumulate balance across months" in {
    val m1            = SocialSecurity.nfzStep(employed, wage, 100000, 10000)
    val m1Balance     = SocialSecurity.nfzCashAfter(PLN.Zero, m1)
    val m2            = SocialSecurity.nfzStep(employed, wage, 100000, 10000)
    val m2Balance     = SocialSecurity.nfzCashAfter(m1Balance, m2)
    val expectedDelta = m1.contributions - m1.spending
    (m2Balance - m1Balance) shouldBe expectedDelta
  }

  it should "have same contributions regardless of retiree count" in {
    val r1 = SocialSecurity.nfzStep(employed, wage, 100000, 5000)
    val r2 = SocialSecurity.nfzStep(employed, wage, 100000, 50000)
    r1.contributions shouldBe r2.contributions
  }

  it should "scale spending by aging elasticity (retirees cost 2.5×)" in {
    // 10000 working-age + 0 retirees vs 0 working-age + 10000 retirees
    val workingOnly = SocialSecurity.nfzStep(employed, wage, 10000, 0)
    val retiredOnly = SocialSecurity.nfzStep(employed, wage, 0, 10000)
    val ratio       = retiredOnly.spending / workingOnly.spending
    decimal(ratio) shouldBe decimal(p.social.nfzAgingElasticity) +- BigDecimal("0.01")
  }
