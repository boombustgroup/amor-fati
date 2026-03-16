package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CesProductionSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private val alpha = Ratio(0.30)
  private val tol   = 1e-6

  // --- cesOutput unit tests ---

  "cesOutput" should "return 1.0 when K=1 and L=1 for any sigma" in {
    for sigma <- Seq(1.0, 2.0, 5.0, 50.0, 1000.0) do
      withClue(s"sigma=$sigma: ") {
        Firm.cesOutput(alpha, Ratio.One, Ratio.One, sigma).toDouble shouldBe 1.0 +- tol
      }
  }

  it should "approximate Cobb-Douglas when sigma <= 1.001" in {
    val k        = Ratio(1.5)
    val l        = Ratio(0.8)
    val expected = Math.pow(k.toDouble, alpha.toDouble) * Math.pow(l.toDouble, 1.0 - alpha.toDouble)
    Firm.cesOutput(alpha, k, l, 1.0).toDouble shouldBe expected +- tol
  }

  it should "approach linear when sigma is very large" in {
    val k      = Ratio(2.0)
    val l      = Ratio(0.5)
    val linear = alpha.toDouble * k.toDouble + (1.0 - alpha.toDouble) * l.toDouble
    val result = Firm.cesOutput(alpha, k, l, 10000.0).toDouble
    result shouldBe linear +- 0.01
  }

  it should "produce higher output for high-sigma with K>L (easier substitution)" in {
    val k         = Ratio(2.0)
    val l         = Ratio(0.5)
    val lowSigma  = Firm.cesOutput(alpha, k, l, 2.0).toDouble
    val highSigma = Firm.cesOutput(alpha, k, l, 50.0).toDouble
    highSigma should be > lowSigma
  }

  it should "be symmetric: swapping K/L with alpha/(1-alpha) gives same result" in {
    val k       = Ratio(1.5)
    val l       = Ratio(0.8)
    val normal  = Firm.cesOutput(alpha, k, l, 5.0).toDouble
    val swapped = Firm.cesOutput(Ratio(1.0 - alpha.toDouble), l, k, 5.0).toDouble
    normal shouldBe swapped +- tol
  }

  it should "be monotonic in K" in {
    val l     = Ratio(1.0)
    val kLow  = Firm.cesOutput(alpha, Ratio(0.5), l, 5.0).toDouble
    val kMid  = Firm.cesOutput(alpha, Ratio(1.0), l, 5.0).toDouble
    val kHigh = Firm.cesOutput(alpha, Ratio(1.5), l, 5.0).toDouble
    kLow should be < kMid
    kMid should be < kHigh
  }

  // --- computeCapacity integration ---

  private def mkFirm(
      tech: TechState = TechState.Traditional(10),
      sector: Int = 0,
      capitalStock: PLN = PLN(1_200_000.0),
  ): Firm.State =
    Firm.State(
      FirmId(0),
      PLN(50000.0),
      PLN.Zero,
      tech,
      Ratio(0.5),
      1.0,
      Ratio(0.3),
      SectorIdx(sector),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = capitalStock,
      bondDebt = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
      accumulatedLoss = PLN.Zero,
    )

  "computeCapacity" should "produce non-zero output for Traditional firm" in {
    Firm.computeCapacity(mkFirm()) should be > PLN.Zero
  }

  it should "produce higher capacity for high-sigma sector (BPO) than low-sigma (Public) at same K/L" in {
    val bpo    = Firm.computeCapacity(mkFirm(sector = 0)) // sigma=50
    val public = Firm.computeCapacity(mkFirm(sector = 4)) // sigma=1
    // BPO has higher revenueMultiplier too, but the CES effect amplifies the difference
    bpo should be > public
  }

  it should "produce zero for bankrupt firm" in {
    Firm.computeCapacity(mkFirm(tech = TechState.Bankrupt(BankruptReason.AiDebtTrap))) shouldBe PLN.Zero
  }

  it should "increase capacity with more capital" in {
    assume(p.flags.physCap, "physCap=true required")
    val poor = Firm.computeCapacity(mkFirm(capitalStock = PLN(100_000.0)))
    val rich = Firm.computeCapacity(mkFirm(capitalStock = PLN(5_000_000.0)))
    rich should be > poor
  }
