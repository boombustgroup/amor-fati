package com.boombustgroup.amorfati.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.types.*

class CentralBankSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  // --- bondYield ---

  "Nbp.bondYield" should "include capped fiscal risk premium" in {
    // When bond market off, yield = refRate (no risk premium, no QE)
    // This test depends on true which is true by default.
    // We test the positive path instead.
    val y = Nbp.bondYield(Rate(0.05), Share(0.50), Share.Zero, PLN.Zero, Rate.Zero)
    // debtToGdp=0.50 > 0.40 -> raw fiscalRisk = 2.0 * 0.10 = 0.20, capped at 0.10
    // yield = 0.05 + 0.005 + 0.10 - 0 - 0 = 0.155
    td.toDouble(y) shouldBe 0.155 +- 0.001
  }

  it should "increase with debtToGdp (fiscal risk premium)" in {
    val low  = Nbp.bondYield(Rate(0.05), Share(0.30), Share.Zero, PLN.Zero, Rate.Zero)
    val high = Nbp.bondYield(Rate(0.05), Share(0.70), Share.Zero, PLN.Zero, Rate.Zero)
    td.toDouble(high) should be > td.toDouble(low)
  }

  it should "decrease with nbpBondGdpShare (QE compression)" in {
    val noQe   = Nbp.bondYield(Rate(0.05), Share(0.50), Share.Zero, PLN.Zero, Rate.Zero)
    val withQe = Nbp.bondYield(Rate(0.05), Share(0.50), Share(0.20), PLN.Zero, Rate.Zero)
    td.toDouble(withQe) should be < td.toDouble(noQe)
  }

  it should "apply foreign demand discount when NFA > 0" in {
    val nfaNeg = Nbp.bondYield(Rate(0.05), Share(0.50), Share.Zero, PLN(-1000.0), Rate.Zero)
    val nfaPos = Nbp.bondYield(Rate(0.05), Share(0.50), Share.Zero, PLN(1000.0), Rate.Zero)
    td.toDouble(nfaPos) should be < td.toDouble(nfaNeg)
  }

  it should "have a floor at 0" in {
    // Very high QE compression -> yield should not go negative
    val y = Nbp.bondYield(Rate(0.01), Share(0.30), Share(0.50), PLN(1000.0), Rate.Zero)
    td.toDouble(y) should be >= 0.0
  }

  it should "have zero fiscal risk when debtToGdp <= 0.40" in {
    val y1 = Nbp.bondYield(Rate(0.05), Share(0.30), Share.Zero, PLN.Zero, Rate.Zero)
    val y2 = Nbp.bondYield(Rate(0.05), Share(0.40), Share.Zero, PLN.Zero, Rate.Zero)
    // Both below threshold -> same yield (only termPremium differs)
    y1 shouldBe y2
  }

  // --- shouldActivateQe ---

  "Nbp.shouldActivateQe" should "be true at ZLB with deflation" in {
    // true defaults to true (NBP March 2020 precedent)
    Nbp.shouldActivateQe(p.monetary.rateFloor, Rate(-0.05), Rate(-0.02)) shouldBe true
  }

  it should "be true at ZLB when expectations remain deeply below target" in {
    Nbp.shouldActivateQe(p.monetary.rateFloor, Rate(0.015), Rate(-0.02)) shouldBe true
  }

  // --- shouldTaperQe ---

  "Nbp.shouldTaperQe" should "be true when inflation exceeds target" in {
    Nbp.shouldTaperQe(Rate(td.toDouble(p.monetary.targetInfl) + 0.01), Rate(td.toDouble(p.monetary.targetInfl) + 0.005)) shouldBe true
  }

  it should "be false when expectations remain below target" in {
    Nbp.shouldTaperQe(Rate(td.toDouble(p.monetary.targetInfl) + 0.01), Rate(td.toDouble(p.monetary.targetInfl) - 0.01)) shouldBe false
  }

  // --- executeQe ---

  "Nbp.executeQe" should "return 0 purchase when not active" in {
    val nbp      = Nbp.State(Rate(0.05), PLN(1000.0), false, PLN.Zero, PLN.Zero, PLN.Zero)
    val qeResult = Nbp.executeQe(nbp, PLN(5000.0), PLN(1e10), Rate(-0.05), Rate(-0.02))
    qeResult.requestedPurchase shouldBe PLN.Zero
    qeResult.nbpState.govBondHoldings shouldBe nbp.govBondHoldings
  }

  it should "not exceed available bank bond holdings" in {
    val nbp       = Nbp.State(Rate(0.05), PLN.Zero, true, PLN.Zero, PLN.Zero, PLN.Zero)
    val bankBonds = PLN(100.0)
    val qeResult  = Nbp.executeQe(nbp, bankBonds, PLN(1e12), Rate(-0.05), Rate(-0.02))
    td.toDouble(qeResult.requestedPurchase) should be <= td.toDouble(bankBonds)
  }

  it should "not exceed max GDP share" in {
    val nbp       = Nbp.State(Rate(0.05), PLN.Zero, true, PLN.Zero, PLN.Zero, PLN.Zero)
    val annualGdp = PLN(1000.0)
    val maxByGdp  = td.toDouble(p.monetary.qeMaxGdpShare) * td.toDouble(annualGdp)
    val qeResult  = Nbp.executeQe(nbp, PLN(1e12), annualGdp, Rate(-0.05), Rate(-0.02))
    td.toDouble(qeResult.requestedPurchase) should be <= maxByGdp
  }

  it should "return positive purchase when active with available bonds" in {
    val nbp      = Nbp.State(Rate(0.05), PLN.Zero, true, PLN.Zero, PLN.Zero, PLN.Zero)
    val qeResult = Nbp.executeQe(nbp, PLN(1e12), PLN(1e12), Rate(-0.05), Rate(-0.02))
    td.toDouble(qeResult.requestedPurchase) should be > 0.0
    // executeQe no longer updates cumulative; that happens in BankingEconomics
    qeResult.nbpState.qeCumulative shouldBe nbp.qeCumulative
  }

  it should "scale QE more aggressively in a lower-bound regime" in {
    val zlbNbp    = Nbp.State(p.monetary.rateFloor, PLN.Zero, true, PLN.Zero, PLN.Zero, PLN.Zero)
    val normalNbp = Nbp.State(Rate(0.03), PLN.Zero, true, PLN.Zero, PLN.Zero, PLN.Zero)
    val annualGdp = PLN(1e12)
    val zlbQe     = Nbp.executeQe(zlbNbp, PLN(1e12), annualGdp, Rate(-0.05), Rate(-0.02))
    val normalQe  = Nbp.executeQe(normalNbp, PLN(1e12), annualGdp, Rate(0.015), Rate(0.02))
    td.toDouble(zlbQe.requestedPurchase) should be > td.toDouble(normalQe.requestedPurchase)
  }

  // --- NbpState defaults ---

  "Nbp.State" should "have backward-compatible constructor" in {
    val nbp = Nbp.State(Rate(0.0575), PLN.Zero, false, PLN.Zero, PLN.Zero, PLN.Zero)
    nbp.referenceRate shouldBe Rate(0.0575)
    nbp.govBondHoldings shouldBe PLN.Zero
    nbp.qeActive shouldBe false
    nbp.qeCumulative shouldBe PLN.Zero
  }
