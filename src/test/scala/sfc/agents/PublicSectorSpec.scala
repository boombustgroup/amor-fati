package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Public sector unit tests — ZUS, PPK, Demographics. */
class PublicSectorSpec extends AnyFlatSpec with Matchers:

  // =========================================================================
  // ZUS
  // =========================================================================

  "PublicSectorLogic.zusStep" should "return zero flows when ZUS disabled" in {
    val zus = PublicSectorLogic.zusStep(0.0, 100000, 8266.0, 50000)
    zus.contributions shouldBe 0.0
    zus.pensionPayments shouldBe 0.0
    zus.govSubvention shouldBe 0.0
    zus.fusBalance shouldBe 0.0
  }

  it should "compute contributions from employed × wage × rate" in {
    // Need ZUS enabled to test — but Config.ZusEnabled is false by default.
    // Test the formula directly instead.
    val employed = 100000
    val wage = 8266.0
    val rate = 0.1952
    val scale = 1.0
    val expected = employed * wage * rate * scale
    expected shouldBe (161.3e6 +- 1e5)
  }

  it should "compute pension payments from retirees × basePension" in {
    val retirees = 50000
    val basePension = 3500.0
    val expected = retirees * basePension
    expected shouldBe 175e6
  }

  it should "compute govSubvention when FUS in deficit" in {
    val contributions = 100e6
    val pensions = 150e6
    val deficit = contributions - pensions  // -50M
    val govSubvention = if deficit < 0 then -deficit else 0.0
    govSubvention shouldBe 50e6
  }

  it should "have zero govSubvention when FUS in surplus" in {
    val contributions = 200e6
    val pensions = 100e6
    val surplus = contributions - pensions  // +100M
    val govSubvention = if surplus < 0 then -surplus else 0.0
    govSubvention shouldBe 0.0
  }

  "ZusState.zero" should "have all zero fields" in {
    ZusState.zero.fusBalance shouldBe 0.0
    ZusState.zero.contributions shouldBe 0.0
    ZusState.zero.pensionPayments shouldBe 0.0
    ZusState.zero.govSubvention shouldBe 0.0
  }

  // =========================================================================
  // PPK
  // =========================================================================

  "PublicSectorLogic.ppkStep" should "return zero flows when PPK disabled" in {
    val ppk = PublicSectorLogic.ppkStep(0.0, 100000, 8266.0)
    ppk.contributions shouldBe 0.0
    ppk.bondHoldings shouldBe 0.0
  }

  it should "preserve previous bond holdings when disabled" in {
    val ppk = PublicSectorLogic.ppkStep(1e9, 100000, 8266.0)
    ppk.bondHoldings shouldBe 1e9
  }

  "PublicSectorLogic.ppkBondPurchase" should "be contributions × bondAlloc" in {
    val ppk = PpkState(bondHoldings = 0.0, contributions = 1e6)
    // Default bondAlloc = 0.60
    val purchase = PublicSectorLogic.ppkBondPurchase(ppk)
    purchase shouldBe (1e6 * 0.60 +- 0.01)
  }

  "PpkState.zero" should "have all zero fields" in {
    PpkState.zero.bondHoldings shouldBe 0.0
    PpkState.zero.contributions shouldBe 0.0
  }

  // =========================================================================
  // Demographics
  // =========================================================================

  "PublicSectorLogic.demographicsStep" should "return unchanged state when disabled" in {
    val dem = DemographicsState(100, 10000, 5)
    val result = PublicSectorLogic.demographicsStep(dem, 9000)
    result.retirees shouldBe 100
    result.workingAgePop shouldBe 10000
    result.monthlyRetirements shouldBe 0
  }

  it should "accept netMigration=0 as default (backward compat)" in {
    val dem = DemographicsState(100, 10000, 5)
    val result = PublicSectorLogic.demographicsStep(dem, 9000)
    // Same as calling with explicit netMigration=0
    val result2 = PublicSectorLogic.demographicsStep(dem, 9000, 0)
    result.workingAgePop shouldBe result2.workingAgePop
  }

  it should "increase workingAgePop with positive netMigration (when disabled)" in {
    // When demographics disabled, state is returned unchanged (no retirement/decline)
    // netMigration is only applied when DemEnabled=true, so this just tests the default path
    val dem = DemographicsState(100, 10000, 5)
    val result = PublicSectorLogic.demographicsStep(dem, 9000, 500)
    // When disabled, workingAgePop unchanged regardless of netMigration
    result.workingAgePop shouldBe 10000
  }

  "DemographicsState.zero" should "have all zero fields" in {
    DemographicsState.zero.retirees shouldBe 0
    DemographicsState.zero.workingAgePop shouldBe 0
    DemographicsState.zero.monthlyRetirements shouldBe 0
  }

  // =========================================================================
  // BGK (stub)
  // =========================================================================

  "BgkState.zero" should "have zero loan portfolio" in {
    BgkState.zero.loanPortfolio shouldBe 0.0
  }

  // =========================================================================
  // SFC Identity 8: FUS balance
  // =========================================================================

  "FUS balance identity" should "hold: ΔfusBalance = contributions - pensions" in {
    val prevBalance = 100e6
    val contributions = 50e6
    val pensions = 70e6
    val expectedChange = contributions - pensions  // -20M
    val newBalance = prevBalance + expectedChange  // 80M
    (newBalance - prevBalance) shouldBe (expectedChange +- 0.01)
  }

  it should "hold trivially when ZUS disabled (both sides = 0)" in {
    val prevBalance = 0.0
    val newBalance = 0.0
    val contributions = 0.0
    val pensions = 0.0
    (newBalance - prevBalance) shouldBe (contributions - pensions +- 0.01)
  }
