package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CesProductionSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults
  private val td  = ComputationBoundary

  private val alpha = Share(0.30)
  private val tol   = 1e-3

  // --- cesOutput unit tests ---

  "cesOutput" should "return 1.0 when K=1 and L=1 for any sigma" in {
    for sigma <- Seq(1.0, 2.0, 5.0, 50.0, 1000.0) do
      withClue(s"sigma=$sigma: ") {
        td.toDouble(Firm.cesOutput(alpha, Multiplier.One, Multiplier.One, Sigma(sigma))).shouldBe(1.0 +- tol)
      }
  }

  it should "approximate Cobb-Douglas when sigma <= 1.001" in {
    val k        = Multiplier(1.5)
    val l        = Multiplier(0.8)
    val expected = Math.pow(td.toDouble(k), td.toDouble(alpha)) * Math.pow(td.toDouble(l), 1.0 - td.toDouble(alpha))
    td.toDouble(Firm.cesOutput(alpha, k, l, Sigma(1.0))).shouldBe(expected +- tol)
  }

  it should "approach linear when sigma is very large" in {
    val k      = Multiplier(2.0)
    val l      = Multiplier(0.5)
    val linear = td.toDouble(alpha) * td.toDouble(k) + (1.0 - td.toDouble(alpha)) * td.toDouble(l)
    val result = td.toDouble(Firm.cesOutput(alpha, k, l, Sigma(10000.0)))
    result.shouldBe(linear +- 0.01)
  }

  it should "produce higher output for high-sigma with K>L (easier substitution)" in {
    val k         = Multiplier(2.0)
    val l         = Multiplier(0.5)
    val lowSigma  = td.toDouble(Firm.cesOutput(alpha, k, l, Sigma(2.0)))
    val highSigma = td.toDouble(Firm.cesOutput(alpha, k, l, Sigma(50.0)))
    highSigma.should(be > lowSigma)
  }

  it should "be symmetric: swapping K/L with alpha/(1-alpha) gives same result" in {
    val k       = Multiplier(1.5)
    val l       = Multiplier(0.8)
    val normal  = td.toDouble(Firm.cesOutput(alpha, k, l, Sigma(5.0)))
    val swapped = td.toDouble(Firm.cesOutput(Share(1.0 - td.toDouble(alpha)), l, k, Sigma(5.0)))
    normal.shouldBe(swapped +- tol)
  }

  it should "be monotonic in K" in {
    val l     = Multiplier(1.0)
    val kLow  = td.toDouble(Firm.cesOutput(alpha, Multiplier(0.5), l, Sigma(5.0)))
    val kMid  = td.toDouble(Firm.cesOutput(alpha, Multiplier(1.0), l, Sigma(5.0)))
    val kHigh = td.toDouble(Firm.cesOutput(alpha, Multiplier(1.5), l, Sigma(5.0)))
    kLow.should(be < kMid)
    kMid.should(be < kHigh)
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
      Share(0.5),
      Multiplier.One,
      Share(0.3),
      SectorIdx(sector),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = capitalStock,
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
    val poor = Firm.computeCapacity(mkFirm(capitalStock = PLN(100_000.0)))
    val rich = Firm.computeCapacity(mkFirm(capitalStock = PLN(5_000_000.0)))
    rich should be > poor
  }
