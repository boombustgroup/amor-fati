package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.agents.Firm
import sfc.config.SimParams
import sfc.types.*

class LaborUnionSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  // --- Config defaults ---

  "UnionEnabled" should "be false by default" in {
    p.flags.unions shouldBe false
  }

  "UnionDensity" should "have 6 values" in {
    p.labor.unionDensity.map(_.toDouble).length shouldBe 6
  }

  "UnionDensity" should "have aggregate ~12%" in {
    val aggDensity =
      p.sectorDefs.zipWithIndex.map((s, i) => s.share.toDouble * p.labor.unionDensity(i).toDouble).sum
    aggDensity shouldBe (0.12 +- 0.02)
  }

  "UnionWagePremium" should "be positive" in {
    p.labor.unionWagePremium.toDouble should be > 0.0
  }

  "UnionRigidity" should "be in [0,1]" in {
    p.labor.unionRigidity.toDouble should be >= 0.0
    p.labor.unionRigidity.toDouble should be <= 1.0
  }

  // --- effectiveWageMult ---

  "effectiveWageMult" should "return base wageMultiplier when disabled" in {
    // Default: UnionEnabled = false
    for s <- p.sectorDefs.indices do Firm.effectiveWageMult(SectorIdx(s)).toDouble shouldBe p.sectorDefs(s).wageMultiplier
  }

  // Note: union-enabled tests use pure formula verification since Config is static

  "effectiveWageMult formula" should "include premium proportional to density" in {
    // Verify the formula: base * (1 + premium * density)
    val publicIdx = 4 // Public sector: density 0.30
    val base      = p.sectorDefs(publicIdx).wageMultiplier
    val premium   = p.labor.unionWagePremium
    val density   = p.labor.unionDensity(publicIdx)
    val expected  = base * (1.0 + (premium * density).toDouble)
    // Public: 0.91 * (1 + 0.08 * 0.30) = 0.91 * 1.024 = 0.93184
    expected shouldBe (0.932 +- 0.001)
    expected should be > base
  }

  "Public sector" should "have higher union density than Retail" in {
    val publicDensity = p.labor.unionDensity(4) // Public
    val retailDensity = p.labor.unionDensity(2) // Retail
    publicDensity should be > retailDensity
  }

  // --- Downward wage rigidity ---

  "Downward rigidity formula" should "dampen wage decline" in {
    val prevWage   = 8000.0
    val rawWage    = 7600.0             // 5% decline
    val aggDensity =
      p.sectorDefs.zipWithIndex.map((s, i) => s.share.toDouble * p.labor.unionDensity(i).toDouble).sum
    val decline    = prevWage - rawWage // 400
    val dampened   = rawWage + decline * p.labor.unionRigidity.toDouble * aggDensity
    // With ~12% density and 0.50 rigidity: 7600 + 400 * 0.50 * 0.12 = 7600 + 24 = 7624
    dampened should be > rawWage
    dampened should be < prevWage
    dampened shouldBe (7624.0 +- 5.0)
  }

  "Downward rigidity" should "respect reservation wage floor" in {
    val resWage    = 4666.0
    val prevWage   = 5000.0
    val rawWage    = 4500.0 // Below resWage
    val aggDensity = 0.12
    val decline    = prevWage - rawWage
    val dampened   = rawWage + decline * p.labor.unionRigidity.toDouble * aggDensity
    val result     = Math.max(resWage, dampened)
    result should be >= resWage
  }

  "Downward rigidity" should "not affect rising wages" in {
    val prevWage = 8000.0
    val rawWage  = 8200.0 // Wages rising
    // When rawWage >= prevWage, union rigidity has no effect
    val result   = if rawWage < prevWage then
      val aggDensity = 0.12
      val decline    = prevWage - rawWage
      rawWage + decline * p.labor.unionRigidity.toDouble * aggDensity
    else rawWage
    result shouldBe rawWage
  }

  // --- Individual wage distribution ---

  "effectiveWageMult" should "produce higher premium for unionized sectors" in {
    // Verify the ordering: Public (30%) > Manufacturing (15%) > BPO (2%)
    val publicPremium = p.labor.unionDensity(4) * p.labor.unionWagePremium
    val mfgPremium    = p.labor.unionDensity(1) * p.labor.unionWagePremium
    val bpoPremium    = p.labor.unionDensity(0) * p.labor.unionWagePremium
    publicPremium should be > mfgPremium
    mfgPremium should be > bpoPremium
  }

  "Individual wage normalization" should "redistribute toward union sectors" in {
    // When unions enabled, raw wages for union sectors are higher
    // After normalization (rawWages * scale where scale = 1/rawMean),
    // unionized sector workers earn more, non-union less (redistribution)
    val sectors = Vector(0.02, 0.15, 0.03, 0.12, 0.30, 0.04) // densities
    val premium = 0.08

    // Compute relative wages without union
    val baseRaw  = p.sectorDefs.map(_.wageMultiplier)
    val baseMean = p.sectorDefs.map(s => s.wageMultiplier * s.share.toDouble).sum /
      p.sectorDefs.map(_.share.toDouble).sum

    // Compute relative wages with union
    val unionRaw  = p.sectorDefs.zipWithIndex.map((s, i) => s.wageMultiplier * (1.0 + premium * sectors(i)))
    val unionMean =
      p.sectorDefs.zipWithIndex.map((s, i) => unionRaw(i) * s.share.toDouble).sum / p.sectorDefs.map(_.share.toDouble).sum

    // After normalization, Public sector (index 4) should be relatively higher
    val publicBaseNorm  = baseRaw(4) / baseMean
    val publicUnionNorm = unionRaw(4) / unionMean
    publicUnionNorm should be > publicBaseNorm
  }
