package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.BondAuction
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tests for the government bond auction mechanism.
  *
  * Key properties:
  *   - Higher yield spread vs Bund attracts more foreign demand (stabilizer)
  *   - PLN depreciation reduces foreign demand (risk-off)
  *   - Bid-to-cover >= 1 when demand sufficient
  *   - Foreign share clamped to [0, maxForeignShare]
  *   - Zero issuance produces no auction activity
  */
class BondAuctionSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val p         = summon[SimParams]
  private val issuance  = PLN("10e9")
  private val bankCap   = PLN("50e9")
  private val baseYield = Rate("0.055")

  "foreignDemandShare" should "increase with higher yield spread vs Bund" in {
    val lowSpread  = BondAuction.foreignDemandShare(Rate("0.03"), Coefficient.Zero)
    val highSpread = BondAuction.foreignDemandShare(Rate("0.08"), Coefficient.Zero)
    decimal(highSpread) should be > decimal(lowSpread)
  }

  it should "decrease with PLN depreciation (risk-off)" in {
    val stable      = BondAuction.foreignDemandShare(baseYield, Coefficient.Zero)
    val depreciated = BondAuction.foreignDemandShare(baseYield, Coefficient("0.05"))
    decimal(depreciated) should be < decimal(stable)
  }

  it should "increase with PLN appreciation" in {
    val stable      = BondAuction.foreignDemandShare(baseYield, Coefficient.Zero)
    val appreciated = BondAuction.foreignDemandShare(baseYield, Coefficient("-0.05"))
    decimal(appreciated) should be > decimal(stable)
  }

  it should "not exceed maxForeignShare" in {
    val extreme = BondAuction.foreignDemandShare(Rate("0.20"), Coefficient("-0.10"))
    decimal(extreme) should be <= decimal(p.fiscal.maxForeignShare)
  }

  it should "not go below zero" in {
    val extreme = BondAuction.foreignDemandShare(Rate("0.001"), Coefficient("0.50"))
    decimal(extreme) should be >= BigDecimal("0.0")
  }

  it should "equal baseForeignShare when spread = 0 and ER stable" in {
    val share = BondAuction.foreignDemandShare(p.fiscal.bundYield, Coefficient.Zero)
    decimal(share) shouldBe decimal(p.fiscal.baseForeignShare) +- BigDecimal("0.01")
  }

  "auction" should "produce bid-to-cover >= 1 under normal conditions" in {
    val result = BondAuction.auction(issuance, bankCap, baseYield, Coefficient.Zero)
    decimal(result.bidToCover) should be >= BigDecimal("1.0")
  }

  it should "split issuance between domestic and foreign" in {
    val result = BondAuction.auction(issuance, bankCap, baseYield, Coefficient.Zero)
    decimal(result.domesticAbsorbed + result.foreignAbsorbed) shouldBe decimal(issuance) +- BigDecimal("1.0")
  }

  it should "allocate ~35% to foreign holders at baseline" in {
    val result     = BondAuction.auction(issuance, bankCap, baseYield, Coefficient.Zero)
    val foreignPct = decimal(result.foreignAbsorbed) / decimal(issuance)
    foreignPct shouldBe decimal(p.fiscal.baseForeignShare) +- BigDecimal("0.15")
  }

  it should "return zero activity for zero issuance" in {
    val result = BondAuction.auction(PLN.Zero, bankCap, baseYield, Coefficient.Zero)
    result.domesticAbsorbed shouldBe PLN.Zero
    result.foreignAbsorbed shouldBe PLN.Zero
  }

  it should "increase foreign absorption when yield rises" in {
    val lowYield  = BondAuction.auction(issuance, bankCap, Rate("0.03"), Coefficient.Zero)
    val highYield = BondAuction.auction(issuance, bankCap, Rate("0.08"), Coefficient.Zero)
    decimal(highYield.foreignAbsorbed) should be > decimal(lowYield.foreignAbsorbed)
  }

  it should "reduce foreign absorption during PLN depreciation" in {
    val stable = BondAuction.auction(issuance, bankCap, baseYield, Coefficient.Zero)
    val crisis = BondAuction.auction(issuance, bankCap, baseYield, Coefficient("0.10"))
    decimal(crisis.foreignAbsorbed) should be < decimal(stable.foreignAbsorbed)
  }
