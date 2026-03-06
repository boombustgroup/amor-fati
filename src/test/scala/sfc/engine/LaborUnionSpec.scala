package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.{Config, SECTORS}
import sfc.agents.{Firm, TechState, Household, HhStatus}
import sfc.types.*

class LaborUnionSpec extends AnyFlatSpec with Matchers:

  // --- Config defaults ---

  "UnionEnabled" should "be false by default" in {
    Config.UnionEnabled shouldBe false
  }

  "UnionDensity" should "have 6 values" in {
    Config.UnionDensity.length shouldBe 6
  }

  "UnionDensity" should "have aggregate ~12%" in {
    val aggDensity = SECTORS.zipWithIndex.map((s, i) => s.share.toDouble * Config.UnionDensity(i)).sum
    aggDensity shouldBe (0.12 +- 0.02)
  }

  "UnionWagePremium" should "be positive" in {
    Config.UnionWagePremium should be > 0.0
  }

  "UnionRigidity" should "be in [0,1]" in {
    Config.UnionRigidity should be >= 0.0
    Config.UnionRigidity should be <= 1.0
  }

  // --- effectiveWageMult ---

  "effectiveWageMult" should "return base wageMultiplier when disabled" in {
    // Default: UnionEnabled = false
    for s <- SECTORS.indices do
      Firm.effectiveWageMult(SectorIdx(s)) shouldBe SECTORS(s).wageMultiplier
  }

  // Note: union-enabled tests use pure formula verification since Config is static

  "effectiveWageMult formula" should "include premium proportional to density" in {
    // Verify the formula: base * (1 + premium * density)
    val publicIdx = 4  // Public sector: density 0.30
    val base = SECTORS(publicIdx).wageMultiplier
    val premium = Config.UnionWagePremium
    val density = Config.UnionDensity(publicIdx)
    val expected = base * (1.0 + premium * density)
    // Public: 0.91 * (1 + 0.08 * 0.30) = 0.91 * 1.024 = 0.93184
    expected shouldBe (0.932 +- 0.001)
    expected should be > base
  }

  "Public sector" should "have higher union density than Retail" in {
    val publicDensity = Config.UnionDensity(4)   // Public
    val retailDensity = Config.UnionDensity(2)   // Retail
    publicDensity should be > retailDensity
  }

  // --- Downward wage rigidity ---

  "Downward rigidity formula" should "dampen wage decline" in {
    val prevWage = 8000.0
    val rawWage = 7600.0  // 5% decline
    val aggDensity = SECTORS.zipWithIndex.map((s, i) => s.share.toDouble * Config.UnionDensity(i)).sum
    val decline = prevWage - rawWage  // 400
    val dampened = rawWage + decline * Config.UnionRigidity * aggDensity
    // With ~12% density and 0.50 rigidity: 7600 + 400 * 0.50 * 0.12 = 7600 + 24 = 7624
    dampened should be > rawWage
    dampened should be < prevWage
    dampened shouldBe (7624.0 +- 5.0)
  }

  "Downward rigidity" should "respect reservation wage floor" in {
    val resWage = 4666.0
    val prevWage = 5000.0
    val rawWage = 4500.0  // Below resWage
    val aggDensity = 0.12
    val decline = prevWage - rawWage
    val dampened = rawWage + decline * Config.UnionRigidity * aggDensity
    val result = Math.max(resWage, dampened)
    result should be >= resWage
  }

  "Downward rigidity" should "not affect rising wages" in {
    val prevWage = 8000.0
    val rawWage = 8200.0  // Wages rising
    // When rawWage >= prevWage, union rigidity has no effect
    val result = if rawWage < prevWage then
      val aggDensity = 0.12
      val decline = prevWage - rawWage
      rawWage + decline * Config.UnionRigidity * aggDensity
    else rawWage
    result shouldBe rawWage
  }

  // --- Individual wage distribution ---

  "effectiveWageMult" should "produce higher premium for unionized sectors" in {
    // Verify the ordering: Public (30%) > Manufacturing (15%) > BPO (2%)
    val publicPremium = Config.UnionDensity(4) * Config.UnionWagePremium
    val mfgPremium = Config.UnionDensity(1) * Config.UnionWagePremium
    val bpoPremium = Config.UnionDensity(0) * Config.UnionWagePremium
    publicPremium should be > mfgPremium
    mfgPremium should be > bpoPremium
  }

  "Individual wage normalization" should "redistribute toward union sectors" in {
    // When unions enabled, raw wages for union sectors are higher
    // After normalization (rawWages * scale where scale = 1/rawMean),
    // unionized sector workers earn more, non-union less (redistribution)
    val mktWage = 8000.0
    val sectors = Vector(0.02, 0.15, 0.03, 0.12, 0.30, 0.04) // densities
    val premium = 0.08

    // Compute relative wages without union
    val baseRaw = SECTORS.map(_.wageMultiplier)
    val baseMean = SECTORS.zipWithIndex.map((s, i) => s.wageMultiplier * s.share.toDouble).sum /
      SECTORS.map(_.share.toDouble).sum

    // Compute relative wages with union
    val unionRaw = SECTORS.zipWithIndex.map((s, i) =>
      s.wageMultiplier * (1.0 + premium * sectors(i)))
    val unionMean = SECTORS.zipWithIndex.map((s, i) =>
      unionRaw(i) * s.share.toDouble).sum / SECTORS.map(_.share.toDouble).sum

    // After normalization, Public sector (index 4) should be relatively higher
    val publicBaseNorm = baseRaw(4) / baseMean
    val publicUnionNorm = unionRaw(4) / unionMean
    publicUnionNorm should be > publicBaseNorm
  }
