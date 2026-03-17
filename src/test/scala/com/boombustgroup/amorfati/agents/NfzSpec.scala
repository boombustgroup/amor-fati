package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NfzSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private val wage     = PLN(8000.0)
  private val employed = 80000

  "nfzStep" should "compute contributions as wage × employed × 9%" in {
    assume(p.flags.nfz, "nfz=true required")
    val result   = SocialSecurity.nfzStep(PLN.Zero, employed, wage, 100000, 10000)
    val expected = wage * p.social.nfzContribRate * employed.toDouble
    result.contributions shouldBe expected
  }

  it should "increase spending with more retirees (aging pressure)" in {
    assume(p.flags.nfz, "nfz=true required")
    val fewRetirees  = SocialSecurity.nfzStep(PLN.Zero, employed, wage, 100000, 5000)
    val manyRetirees = SocialSecurity.nfzStep(PLN.Zero, employed, wage, 100000, 30000)
    manyRetirees.spending should be > fewRetirees.spending
  }

  it should "produce govSubvention when spending exceeds contributions" in {
    assume(p.flags.nfz, "nfz=true required")
    // Many retirees + low employment → deficit
    val result = SocialSecurity.nfzStep(PLN.Zero, 10000, wage, 50000, 50000)
    result.spending should be > result.contributions
    result.govSubvention should be > PLN.Zero
  }

  it should "produce zero govSubvention when in surplus" in {
    assume(p.flags.nfz, "nfz=true required")
    // High employment + high wage + few retirees → surplus
    val result = SocialSecurity.nfzStep(PLN.Zero, 100000, PLN(20000.0), 100000, 100)
    result.contributions should be > result.spending
    result.govSubvention shouldBe PLN.Zero
  }

  it should "accumulate balance across months" in {
    assume(p.flags.nfz, "nfz=true required")
    val m1            = SocialSecurity.nfzStep(PLN.Zero, employed, wage, 100000, 10000)
    val m2            = SocialSecurity.nfzStep(m1.balance, employed, wage, 100000, 10000)
    val expectedDelta = m1.contributions - m1.spending
    (m2.balance - m1.balance) shouldBe expectedDelta
  }

  it should "have same contributions regardless of retiree count" in {
    assume(p.flags.nfz, "nfz=true required")
    val r1 = SocialSecurity.nfzStep(PLN.Zero, employed, wage, 100000, 5000)
    val r2 = SocialSecurity.nfzStep(PLN.Zero, employed, wage, 100000, 50000)
    r1.contributions shouldBe r2.contributions
  }

  it should "scale spending by aging elasticity (retirees cost 2.5×)" in {
    assume(p.flags.nfz, "nfz=true required")
    // 10000 working-age + 0 retirees vs 0 working-age + 10000 retirees
    val workingOnly = SocialSecurity.nfzStep(PLN.Zero, employed, wage, 10000, 0)
    val retiredOnly = SocialSecurity.nfzStep(PLN.Zero, employed, wage, 0, 10000)
    val ratio       = retiredOnly.spending / workingOnly.spending
    ratio shouldBe p.social.nfzAgingElasticity +- 0.01
  }
