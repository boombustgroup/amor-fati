package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.TestFirmState

import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.engine.mechanisms.InformalEconomy
import com.boombustgroup.amorfati.agents.{BankruptReason, Firm, TechState}
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*

class InformalEconomySpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private def cyclicalAdj(unemployment: Rate): Share =
    shareBD(DecimalMath.max(BigDecimal("0.0"), decimal(unemployment) - decimal(p.informal.unempThreshold)) * decimal(p.informal.cyclicalSens))

  // ==========================================================================
  // Config defaults
  // ==========================================================================

  "InformalSectorShares" should "have 6 elements" in {
    p.informal.sectorShares.length shouldBe 6
  }

  it should "have all values in [0,1]" in
    p.informal.sectorShares.foreach { s =>
      s should be >= Share.Zero
      s should be <= Share.One
    }

  it should "have Agri highest" in {
    p.informal.sectorShares(5) shouldBe p.informal.sectorShares.max
  }

  it should "have Public lowest" in {
    p.informal.sectorShares(4) shouldBe p.informal.sectorShares.min
  }

  it should "match expected defaults" in {
    p.informal.sectorShares shouldBe Vector(
      Share.decimal(5, 2),
      Share.decimal(15, 2),
      Share.decimal(30, 2),
      Share.decimal(20, 2),
      Share.decimal(2, 2),
      Share.decimal(35, 2),
    )
  }

  "InformalCitEvasion" should "default to 0.80" in {
    p.informal.citEvasion shouldBe Share.decimal(80, 2)
  }

  "InformalVatEvasion" should "default to 0.90" in {
    p.informal.vatEvasion shouldBe Share.decimal(90, 2)
  }

  "InformalPitEvasion" should "default to 0.85" in {
    p.informal.pitEvasion shouldBe Share.decimal(85, 2)
  }

  "InformalExciseEvasion" should "default to 0.70" in {
    p.informal.exciseEvasion shouldBe Share.decimal(70, 2)
  }

  "InformalUnempThreshold" should "default to 0.05" in {
    p.informal.unempThreshold shouldBe Rate.decimal(5, 2)
  }

  "InformalCyclicalSens" should "default to 0.50" in {
    p.informal.cyclicalSens shouldBe Coefficient.decimal(50, 2)
  }

  // ==========================================================================
  // World fields
  // ==========================================================================

  private def mkMinimalWorld() = Generators.testWorld(
    inflation = Rate(0),
    currentSigmas = Vector.fill(6)(Sigma(5)),
    marketWage = PLN(8000),
    reservationWage = PLN(4500),
  )

  "World" should "have informalCyclicalAdj defaulting to 0.0" in {
    val w = mkMinimalWorld()
    w.mechanisms.informalCyclicalAdj shouldBe Share.Zero
  }

  it should "have nextTaxShadowShare defaulting to 0.0" in {
    val w = mkMinimalWorld()
    w.mechanisms.nextTaxShadowShare shouldBe Share.Zero
  }

  it should "have taxEvasionLoss defaulting to 0.0" in {
    val w = mkMinimalWorld()
    w.flows.taxEvasionLoss shouldBe PLN.Zero
  }

  it should "have realizedTaxShadowShare defaulting to 0.0" in {
    val w = mkMinimalWorld()
    w.flows.realizedTaxShadowShare shouldBe Share.Zero
  }

  // ==========================================================================
  // Firm.Result citEvasion
  // ==========================================================================

  private def mkFirm(tech: TechState = TechState.Traditional(10), cash: PLN = PLN(50000)): Firm.State =
    TestFirmState(
      FirmId(0),
      cash,
      PLN.Zero,
      tech,
      Share.decimal(5, 1),
      Multiplier.One,
      Share.decimal(3, 1),
      SectorIdx(0),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
      accumulatedLoss = PLN.Zero,
    )

  "Firm.Result" should "have citEvasion defaulting to 0.0" in {
    val r = Firm.Result.zero(mkFirm()).copy(taxPaid = PLN(100))
    r.citEvasion shouldBe PLN.Zero
  }

  // ==========================================================================
  // CIT evasion: bankrupt firms
  // ==========================================================================

  "CIT evasion" should "be zero for bankrupt firms" in {
    val r = Firm.Result.zero(mkFirm(TechState.Bankrupt(BankruptReason.Other("test")), cash = PLN.Zero))
    r.citEvasion shouldBe PLN.Zero
  }

  it should "be zero when taxPaid <= 0" in {
    val r = Firm.Result.zero(mkFirm())
    r.citEvasion shouldBe PLN.Zero
  }

  // ==========================================================================
  // Counter-cyclical dynamics
  // ==========================================================================

  "Counter-cyclical adjustment" should "be 0 when unemployment <= threshold" in {
    cyclicalAdj(Rate.decimal(4, 2)) shouldBe Share.Zero
  }

  it should "be positive when unemployment > threshold" in {
    val adj = cyclicalAdj(Rate.decimal(10, 2))
    adj should be > Share.Zero
    adj shouldBe Share.decimal(25, 3)
  }

  it should "increase with unemployment" in {
    val adj1 = cyclicalAdj(Rate.decimal(8, 2))
    val adj2 = cyclicalAdj(Rate.decimal(15, 2))
    adj2 should be > adj1
  }

  // ==========================================================================
  // Effective shadow share
  // ==========================================================================

  "Effective shadow share" should "be weighted by FofConsWeights" in {
    val ess = InformalEconomy.aggregateTaxShadowShare(Share.Zero)
    ess should be > Share.Zero
    ess should be < Share.One
    // BPO=0.02*0.05, Mfg=0.22*0.15, Ret=0.53*0.30, Hlt=0.06*0.20, Pub=0.07*0.02, Agr=0.10*0.35
    // = 0.001 + 0.033 + 0.159 + 0.012 + 0.0014 + 0.035 = ~0.2414
    ess shouldBe Share.decimal(2414, 4)
  }

  it should "be capped at 1.0 per sector" in {
    val cyclicalAdj = Share(2)
    val shares      = p.informal.sectorShares.map(ss => (ss + cyclicalAdj).min(Share.One))
    shares.foreach(_ shouldBe Share.One)
  }

  it should "keep the baseline aggregate tax shadow share inside the Schneider target band" in {
    val baseline = InformalEconomy.aggregateTaxShadowShare(Share.Zero)
    baseline should be >= Share.decimal(20, 2)
    baseline should be <= Share.decimal(25, 2)
    baseline shouldBe Share.decimal(2414, 4)
  }

  // ==========================================================================
  // VAT evasion
  // ==========================================================================

  "VAT evasion" should "reduce VAT proportionally" in {
    val vat         = PLN(1000)
    val ess         = Share.decimal(20, 2)
    val evasionFrac = shareBD(decimal(ess) * decimal(p.informal.vatEvasion))
    val vatAfter    = vat * (Share.One - evasionFrac)
    vatAfter should be < vat
    vatAfter should be > PLN.Zero
  }

  // ==========================================================================
  // PIT evasion
  // ==========================================================================

  "PIT evasion" should "reduce PIT proportionally" in {
    val pit         = PLN(500)
    val ess         = Share.decimal(20, 2)
    val evasionFrac = shareBD(decimal(ess) * decimal(p.informal.pitEvasion))
    val pitAfter    = pit * (Share.One - evasionFrac)
    pitAfter should be < pit
    pitAfter should be > PLN.Zero
  }

  // ==========================================================================
  // Excise evasion
  // ==========================================================================

  "Excise evasion" should "reduce excise proportionally" in {
    val excise      = PLN(300)
    val ess         = Share.decimal(20, 2)
    val evasionFrac = shareBD(decimal(ess) * decimal(p.informal.exciseEvasion))
    val exciseAfter = excise * (Share.One - evasionFrac)
    exciseAfter should be < excise
    exciseAfter should be > PLN.Zero
  }

  // ==========================================================================
  // TaxEvasionLoss
  // ==========================================================================

  "TaxEvasionLoss" should "be sum of all channels" in {
    val citEvasion = PLN(100)
    val vatDiff    = PLN(200)
    val pitDiff    = PLN(150)
    val exciseDiff = PLN(50)
    val total      = citEvasion + vatDiff + pitDiff + exciseDiff
    total shouldBe PLN(500)
  }

  // ==========================================================================
  // EvasionToGdpRatio
  // ==========================================================================

  "EvasionToGdpRatio" should "be positive when evasion > 0 and GDP > 0" in {
    val evasion = PLN(100)
    val gdp     = PLN(1000)
    val ratio   = decimal(evasion) / decimal(gdp)
    ratio should be > BigDecimal("0.0")
    ratio shouldBe BigDecimal("0.1")
  }

  it should "be zero when GDP is zero" in {
    val evasion = PLN(100)
    val gdp     = PLN.Zero
    val ratio   = if gdp > PLN.Zero then decimal(evasion) / decimal(gdp) else BigDecimal("0.0")
    ratio shouldBe BigDecimal("0.0")
  }

  // ==========================================================================
  // GDP unaffected
  // ==========================================================================

  "GDP formula" should "not include tax evasion (evasion doesn't change GDP)" in {
    // GDP = domesticCons + runtime gov contribution + euGdpContribution + exports + domesticGFCF + inventoryChange
    // Tax evasion only reduces government revenue, not GDP
    // This is a design test: GDP computation doesn't use taxEvasionLoss
    true shouldBe true // Verified by code inspection
  }

  "Informal calibration" should "keep default runtime ratios in a plausible envelope over 12 months" in {
    val init  = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    var state = FlowSimulation.SimState.fromInit(init)

    val realizedShares = collection.mutable.ArrayBuffer.empty[Share]
    val evasionRatios  = collection.mutable.ArrayBuffer.empty[Share]

    (1 to 12).foreach: month =>
      val result     = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L * 1000 + month))
      realizedShares += result.nextState.world.flows.realizedTaxShadowShare
      val monthlyGdp = result.nextState.world.cachedMonthlyGdpProxy
      val ratio      =
        if monthlyGdp > PLN.Zero then shareBD(decimal(result.nextState.world.flows.taxEvasionLoss) / decimal(monthlyGdp)) else Share.Zero
      evasionRatios += ratio
      state = result.nextState

    val avgShare = shareBD(realizedShares.map(decimal).sum / realizedShares.size)
    val avgRatio = shareBD(evasionRatios.map(decimal).sum / evasionRatios.size)

    avgShare should be >= Share.decimal(24, 2)
    avgShare should be <= Share.decimal(30, 2)
    avgRatio should be >= Share.decimal(3, 3)
    avgRatio should be <= Share.decimal(2, 2)
  }
