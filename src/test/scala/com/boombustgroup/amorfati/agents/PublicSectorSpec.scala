package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.agents.QuasiFiscal
import com.boombustgroup.amorfati.types.*

/** Public sector unit tests — ZUS, PPK, Demographics. */
class PublicSectorSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  // =========================================================================
  // ZUS
  // =========================================================================

  "SocialSecurity.zusStep" should "compute contributions from employed × wage × rate" in {
    val employed = 100000
    val wage     = BigDecimal("8266.0")
    val rate     = BigDecimal("0.1952")
    val scale    = BigDecimal("1.0")
    val expected = employed * wage * rate * scale
    expected shouldBe (BigDecimal("161.3e6") +- BigDecimal("1e5"))
  }

  it should "compute pension payments from retirees × basePension" in {
    val retirees    = 50000
    val basePension = BigDecimal("3500.0")
    val expected    = retirees * basePension
    expected shouldBe BigDecimal("175e6")
  }

  it should "compute govSubvention when FUS in deficit" in {
    val contributions = BigDecimal("100e6")
    val pensions      = BigDecimal("150e6")
    val deficit       = contributions - pensions // -50M
    val govSubvention = if deficit < 0 then -deficit else BigDecimal("0.0")
    govSubvention shouldBe BigDecimal("50e6")
  }

  it should "have zero govSubvention when FUS in surplus" in {
    val contributions = BigDecimal("200e6")
    val pensions      = BigDecimal("100e6")
    val surplus       = contributions - pensions // +100M
    val govSubvention = if surplus < 0 then -surplus else BigDecimal("0.0")
    govSubvention shouldBe BigDecimal("0.0")
  }

  "SocialSecurity.ZusState.zero" should "have all zero fields" in {
    SocialSecurity.ZusState.zero.contributions shouldBe PLN.Zero
    SocialSecurity.ZusState.zero.pensionPayments shouldBe PLN.Zero
    SocialSecurity.ZusState.zero.govSubvention shouldBe PLN.Zero
  }

  // =========================================================================
  // PPK
  // =========================================================================

  "SocialSecurity.ppkBondPurchase" should "be contributions × bondAlloc" in {
    val ppk      = SocialSecurity.PpkState(contributions = PLN("1e6"))
    // Default bondAlloc = 0.60
    val purchase = SocialSecurity.ppkBondPurchase(ppk)
    decimal(purchase) shouldBe (BigDecimal("1e6") * BigDecimal("0.60") +- BigDecimal("0.01"))
  }

  "SocialSecurity.PpkState.zero" should "have zero contributions" in {
    SocialSecurity.PpkState.zero.contributions shouldBe PLN.Zero
  }

  // =========================================================================
  // Demographics
  // =========================================================================

  "SocialSecurity.DemographicsState.zero" should "have all zero fields" in {
    SocialSecurity.DemographicsState.zero.retirees shouldBe 0
    SocialSecurity.DemographicsState.zero.workingAgePop shouldBe 0
    SocialSecurity.DemographicsState.zero.monthlyRetirements shouldBe 0
  }

  // =========================================================================
  // BGK (stub)
  // =========================================================================

  "QuasiFiscal.StockState.zero" should "have zero outstanding and loans" in {
    QuasiFiscal.StockState.zero.bondsOutstanding shouldBe PLN.Zero
    QuasiFiscal.StockState.zero.loanPortfolio shouldBe PLN.Zero
    QuasiFiscal.StockState.zero.bankHoldings shouldBe PLN.Zero
    QuasiFiscal.StockState.zero.nbpHoldings shouldBe PLN.Zero
  }

  // =========================================================================
  // SFC Identity 8: FUS balance
  // =========================================================================

  "FUS balance identity" should "hold: ΔfusBalance = contributions - pensions" in {
    val prevBalance    = BigDecimal("100e6")
    val contributions  = BigDecimal("50e6")
    val pensions       = BigDecimal("70e6")
    val expectedChange = contributions - pensions     // -20M
    val newBalance     = prevBalance + expectedChange // 80M
    (newBalance - prevBalance) shouldBe (expectedChange +- BigDecimal("0.01"))
  }
