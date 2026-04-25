package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.TestFirmState

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CesProductionSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val alpha = Share.decimal(30, 2)
  private val tol   = BigDecimal("1e-3")

  // --- cesOutput unit tests ---

  "cesOutput" should "return 1.0 when K=1 and L=1 for any sigma" in {
    for sigma <- Seq(BigDecimal("1.0"), BigDecimal("2.0"), BigDecimal("5.0"), BigDecimal("50.0"), BigDecimal("1000.0")) do
      withClue(s"sigma=$sigma: ") {
        decimal(Firm.cesOutput(alpha, Multiplier.One, Multiplier.One, sigmaBD(sigma))).shouldBe(BigDecimal("1.0") +- tol)
      }
  }

  it should "approximate Cobb-Douglas when sigma <= 1.001" in {
    val k        = Multiplier.decimal(15, 1)
    val l        = Multiplier.decimal(8, 1)
    val expected = DecimalMath.pow(decimal(k), decimal(alpha)) * DecimalMath.pow(decimal(l), BigDecimal("1.0") - decimal(alpha))
    decimal(Firm.cesOutput(alpha, k, l, Sigma(1))).shouldBe(expected +- tol)
  }

  it should "approach linear when sigma is very large" in {
    val k      = Multiplier(2)
    val l      = Multiplier.decimal(5, 1)
    val linear = decimal(alpha) * decimal(k) + (BigDecimal("1.0") - decimal(alpha)) * decimal(l)
    val result = decimal(Firm.cesOutput(alpha, k, l, Sigma(10000)))
    result.shouldBe(linear +- BigDecimal("0.01"))
  }

  it should "produce higher output for high-sigma with K>L (easier substitution)" in {
    val k         = Multiplier(2)
    val l         = Multiplier.decimal(5, 1)
    val lowSigma  = decimal(Firm.cesOutput(alpha, k, l, Sigma(2)))
    val highSigma = decimal(Firm.cesOutput(alpha, k, l, Sigma(50)))
    highSigma.should(be > lowSigma)
  }

  it should "be symmetric: swapping K/L with alpha/(1-alpha) gives same result" in {
    val k       = Multiplier.decimal(15, 1)
    val l       = Multiplier.decimal(8, 1)
    val normal  = decimal(Firm.cesOutput(alpha, k, l, Sigma(5)))
    val swapped = decimal(Firm.cesOutput(Share.One - alpha, l, k, Sigma(5)))
    normal.shouldBe(swapped +- tol)
  }

  it should "be monotonic in K" in {
    val l     = Multiplier(1)
    val kLow  = decimal(Firm.cesOutput(alpha, Multiplier.decimal(5, 1), l, Sigma(5)))
    val kMid  = decimal(Firm.cesOutput(alpha, Multiplier(1), l, Sigma(5)))
    val kHigh = decimal(Firm.cesOutput(alpha, Multiplier.decimal(15, 1), l, Sigma(5)))
    kLow.should(be < kMid)
    kMid.should(be < kHigh)
  }

  // --- computeCapacity integration ---

  private def mkFirm(
      tech: TechState = TechState.Traditional(10),
      sector: Int = 0,
      capitalStock: PLN = PLN(1200000),
  ): Firm.State =
    TestFirmState(
      FirmId(0),
      PLN(50000),
      PLN.Zero,
      tech,
      Share.decimal(5, 1),
      Multiplier.One,
      Share.decimal(3, 1),
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
    val poor = Firm.computeCapacity(mkFirm(capitalStock = PLN(100000)))
    val rich = Firm.computeCapacity(mkFirm(capitalStock = PLN(5000000)))
    rich should be > poor
  }
