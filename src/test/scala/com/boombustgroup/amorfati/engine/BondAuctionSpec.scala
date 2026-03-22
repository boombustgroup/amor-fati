package com.boombustgroup.amorfati.engine

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
  private val issuance  = PLN(10e9)
  private val bankCap   = PLN(50e9)
  private val baseYield = Rate(0.055)

  "foreignDemandShare" should "increase with higher yield spread vs Bund" in {
    val lowSpread  = BondAuction.foreignDemandShare(Rate(0.03), Coefficient.Zero)
    val highSpread = BondAuction.foreignDemandShare(Rate(0.08), Coefficient.Zero)
    highSpread.toDouble should be > lowSpread.toDouble
  }

  it should "decrease with PLN depreciation (risk-off)" in {
    val stable      = BondAuction.foreignDemandShare(baseYield, Coefficient.Zero)
    val depreciated = BondAuction.foreignDemandShare(baseYield, Coefficient(0.05))
    depreciated.toDouble should be < stable.toDouble
  }

  it should "increase with PLN appreciation" in {
    val stable      = BondAuction.foreignDemandShare(baseYield, Coefficient.Zero)
    val appreciated = BondAuction.foreignDemandShare(baseYield, Coefficient(-0.05))
    appreciated.toDouble should be > stable.toDouble
  }

  it should "not exceed maxForeignShare" in {
    val extreme = BondAuction.foreignDemandShare(Rate(0.20), Coefficient(-0.10))
    extreme.toDouble should be <= p.fiscal.maxForeignShare.toDouble
  }

  it should "not go below zero" in {
    val extreme = BondAuction.foreignDemandShare(Rate(0.001), Coefficient(0.50))
    extreme.toDouble should be >= 0.0
  }

  it should "equal baseForeignShare when spread = 0 and ER stable" in {
    val share = BondAuction.foreignDemandShare(p.fiscal.bundYield, Coefficient.Zero)
    share.toDouble shouldBe p.fiscal.baseForeignShare.toDouble +- 0.01
  }

  "auction" should "produce bid-to-cover >= 1 under normal conditions" in {
    val result = BondAuction.auction(issuance, bankCap, baseYield, Coefficient.Zero)
    result.bidToCover.toDouble should be >= 1.0
  }

  it should "split issuance between domestic and foreign" in {
    val result = BondAuction.auction(issuance, bankCap, baseYield, Coefficient.Zero)
    (result.domesticAbsorbed + result.foreignAbsorbed).toDouble shouldBe issuance.toDouble +- 1.0
  }

  it should "allocate ~35% to foreign holders at baseline" in {
    val result     = BondAuction.auction(issuance, bankCap, baseYield, Coefficient.Zero)
    val foreignPct = result.foreignAbsorbed.toDouble / issuance.toDouble
    foreignPct shouldBe p.fiscal.baseForeignShare.toDouble +- 0.15
  }

  it should "return zero activity for zero issuance" in {
    val result = BondAuction.auction(PLN.Zero, bankCap, baseYield, Coefficient.Zero)
    result.domesticAbsorbed shouldBe PLN.Zero
    result.foreignAbsorbed shouldBe PLN.Zero
  }

  it should "increase foreign absorption when yield rises" in {
    val lowYield  = BondAuction.auction(issuance, bankCap, Rate(0.03), Coefficient.Zero)
    val highYield = BondAuction.auction(issuance, bankCap, Rate(0.08), Coefficient.Zero)
    highYield.foreignAbsorbed.toDouble should be > lowYield.foreignAbsorbed.toDouble
  }

  it should "reduce foreign absorption during PLN depreciation" in {
    val stable = BondAuction.auction(issuance, bankCap, baseYield, Coefficient.Zero)
    val crisis = BondAuction.auction(issuance, bankCap, baseYield, Coefficient(0.10))
    crisis.foreignAbsorbed.toDouble should be < stable.foreignAbsorbed.toDouble
  }
