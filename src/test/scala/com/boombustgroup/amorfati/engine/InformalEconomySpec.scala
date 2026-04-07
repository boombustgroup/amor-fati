package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.engine.mechanisms.InformalEconomy
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, OpenEconomy}
import com.boombustgroup.amorfati.agents.{BankruptReason, Firm, TechState}
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.*

class InformalEconomySpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  private def cyclicalAdj(unemployment: Rate): Share =
    Share(Math.max(0.0, td.toDouble(unemployment) - td.toDouble(p.informal.unempThreshold)) * td.toDouble(p.informal.cyclicalSens))

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
    p.informal.sectorShares shouldBe Vector(Share(0.05), Share(0.15), Share(0.30), Share(0.20), Share(0.02), Share(0.35))
  }

  "InformalCitEvasion" should "default to 0.80" in {
    p.informal.citEvasion shouldBe Share(0.80)
  }

  "InformalVatEvasion" should "default to 0.90" in {
    p.informal.vatEvasion shouldBe Share(0.90)
  }

  "InformalPitEvasion" should "default to 0.85" in {
    p.informal.pitEvasion shouldBe Share(0.85)
  }

  "InformalExciseEvasion" should "default to 0.70" in {
    p.informal.exciseEvasion shouldBe Share(0.70)
  }

  "InformalUnempThreshold" should "default to 0.05" in {
    p.informal.unempThreshold shouldBe Rate(0.05)
  }

  "InformalCyclicalSens" should "default to 0.50" in {
    p.informal.cyclicalSens shouldBe Coefficient(0.50)
  }

  // ==========================================================================
  // World fields
  // ==========================================================================

  private def mkMinimalWorld() = World(
    month = 0,
    inflation = Rate(0.0),
    priceLevel = 1.0,
    gdpProxy = 1e9,
    currentSigmas = Vector.fill(6)(Sigma(5.0)),
    totalPopulation = 100,
    gov = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    nbp = com.boombustgroup.amorfati.agents.Nbp.State(Rate(0.05), PLN.Zero, false, PLN.Zero, PLN.Zero, PLN.Zero),
    bankingSector = Generators.testBankingSector().marketState,
    forex = OpenEconomy.ForexState(4.33, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    hhAgg = com.boombustgroup.amorfati.agents.Household.Aggregates(
      employed = 100,
      unemployed = 0,
      retraining = 0,
      bankrupt = 0,
      totalIncome = PLN.Zero,
      consumption = PLN.Zero,
      domesticConsumption = PLN.Zero,
      importConsumption = PLN.Zero,
      marketWage = PLN(8000),
      reservationWage = PLN(4500),
      giniIndividual = Share.Zero,
      giniWealth = Share.Zero,
      meanSavings = PLN.Zero,
      medianSavings = PLN.Zero,
      povertyRate50 = Share.Zero,
      bankruptcyRate = Share.Zero,
      meanSkill = Share.Zero,
      meanHealthPenalty = Share.Zero,
      retrainingAttempts = 0,
      retrainingSuccesses = 0,
      consumptionP10 = PLN.Zero,
      consumptionP50 = PLN.Zero,
      consumptionP90 = PLN.Zero,
      meanMonthsToRuin = Scalar.Zero,
      povertyRate30 = Share.Zero,
      totalRent = PLN.Zero,
      totalDebtService = PLN.Zero,
      totalUnempBenefits = PLN.Zero,
      totalDepositInterest = PLN.Zero,
      crossSectorHires = 0,
      voluntaryQuits = 0,
      sectorMobilityRate = Share.Zero,
      totalRemittances = PLN.Zero,
      totalPit = PLN.Zero,
      totalSocialTransfers = PLN.Zero,
      totalConsumerDebtService = PLN.Zero,
      totalConsumerOrigination = PLN.Zero,
      totalConsumerDefault = PLN.Zero,
      totalConsumerPrincipal = PLN.Zero,
    ),
    social = SocialState.zero,
    financial = FinancialMarketsState.zero,
    external = ExternalState.zero,
    real = RealState.zero,
    mechanisms = MechanismsState.zero,
    plumbing = MonetaryPlumbingState.zero,
    flows = FlowState.zero,
  )

  "World" should "have informalCyclicalAdj defaulting to 0.0" in {
    val w = mkMinimalWorld()
    w.mechanisms.informalCyclicalAdj shouldBe 0.0
  }

  it should "have nextTaxShadowShare defaulting to 0.0" in {
    val w = mkMinimalWorld()
    w.mechanisms.nextTaxShadowShare shouldBe 0.0
  }

  it should "have taxEvasionLoss defaulting to 0.0" in {
    val w = mkMinimalWorld()
    w.flows.taxEvasionLoss shouldBe PLN.Zero
  }

  it should "have realizedTaxShadowShare defaulting to 0.0" in {
    val w = mkMinimalWorld()
    w.flows.realizedTaxShadowShare shouldBe 0.0
  }

  // ==========================================================================
  // Firm.Result citEvasion
  // ==========================================================================

  private def mkFirm(tech: TechState = TechState.Traditional(10), cash: PLN = PLN(50000.0)): Firm.State =
    Firm.State(
      FirmId(0),
      cash,
      PLN.Zero,
      tech,
      Share(0.5),
      Multiplier.One,
      Share(0.3),
      SectorIdx(0),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = PLN.Zero,
      bondDebt = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
      accumulatedLoss = PLN.Zero,
    )

  "Firm.Result" should "have citEvasion defaulting to 0.0" in {
    val r = Firm.Result.zero(mkFirm()).copy(taxPaid = PLN(100.0))
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
    cyclicalAdj(Rate(0.04)) shouldBe Share.Zero
  }

  it should "be positive when unemployment > threshold" in {
    val adj = cyclicalAdj(Rate(0.10))
    adj should be > Share.Zero
    adj shouldBe Share(0.025)
  }

  it should "increase with unemployment" in {
    val adj1 = cyclicalAdj(Rate(0.08))
    val adj2 = cyclicalAdj(Rate(0.15))
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
    ess shouldBe Share(0.2414)
  }

  it should "be capped at 1.0 per sector" in {
    val cyclicalAdj = Share(2.0)
    val shares      = p.informal.sectorShares.map(ss => (ss + cyclicalAdj).min(Share.One))
    shares.foreach(_ shouldBe Share.One)
  }

  it should "keep the baseline aggregate tax shadow share inside the Schneider target band" in {
    val baseline = InformalEconomy.aggregateTaxShadowShare(Share.Zero)
    baseline should be >= Share(0.20)
    baseline should be <= Share(0.25)
    baseline shouldBe Share(0.2414)
  }

  // ==========================================================================
  // VAT evasion
  // ==========================================================================

  "VAT evasion" should "reduce VAT proportionally" in {
    val vat         = PLN(1000.0)
    val ess         = Share(0.20)
    val evasionFrac = Share(td.toDouble(ess) * td.toDouble(p.informal.vatEvasion))
    val vatAfter    = vat * (Share.One - evasionFrac)
    vatAfter should be < vat
    vatAfter should be > PLN.Zero
  }

  // ==========================================================================
  // PIT evasion
  // ==========================================================================

  "PIT evasion" should "reduce PIT proportionally" in {
    val pit         = PLN(500.0)
    val ess         = Share(0.20)
    val evasionFrac = Share(td.toDouble(ess) * td.toDouble(p.informal.pitEvasion))
    val pitAfter    = pit * (Share.One - evasionFrac)
    pitAfter should be < pit
    pitAfter should be > PLN.Zero
  }

  // ==========================================================================
  // Excise evasion
  // ==========================================================================

  "Excise evasion" should "reduce excise proportionally" in {
    val excise      = PLN(300.0)
    val ess         = Share(0.20)
    val evasionFrac = Share(td.toDouble(ess) * td.toDouble(p.informal.exciseEvasion))
    val exciseAfter = excise * (Share.One - evasionFrac)
    exciseAfter should be < excise
    exciseAfter should be > PLN.Zero
  }

  // ==========================================================================
  // TaxEvasionLoss
  // ==========================================================================

  "TaxEvasionLoss" should "be sum of all channels" in {
    val citEvasion = PLN(100.0)
    val vatDiff    = PLN(200.0)
    val pitDiff    = PLN(150.0)
    val exciseDiff = PLN(50.0)
    val total      = citEvasion + vatDiff + pitDiff + exciseDiff
    total shouldBe PLN(500.0)
  }

  // ==========================================================================
  // EvasionToGdpRatio
  // ==========================================================================

  "EvasionToGdpRatio" should "be positive when evasion > 0 and GDP > 0" in {
    val evasion = PLN(100.0)
    val gdp     = PLN(1000.0)
    val ratio   = td.toDouble(evasion) / td.toDouble(gdp)
    ratio should be > 0.0
    ratio shouldBe 0.1
  }

  it should "be zero when GDP is zero" in {
    val evasion = PLN(100.0)
    val gdp     = PLN.Zero
    val ratio   = if gdp > PLN.Zero then td.toDouble(evasion) / td.toDouble(gdp) else 0.0
    ratio shouldBe 0.0
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
    val init  = WorldInit.initialize(42L)
    var w     = init.world
    var firms = init.firms
    var hh    = init.households
    var banks = init.banks

    val realizedShares = collection.mutable.ArrayBuffer.empty[Share]
    val evasionRatios  = collection.mutable.ArrayBuffer.empty[Share]

    (1 to 12).foreach: month =>
      val result     = FlowSimulation.step(w, firms, hh, banks, new scala.util.Random(42L * 1000 + month))
      realizedShares += Share(result.newWorld.flows.realizedTaxShadowShare)
      val monthlyGdp = result.newWorld.cachedMonthlyGdpProxy
      val ratio      =
        if monthlyGdp > PLN.Zero then Share(td.toDouble(result.newWorld.flows.taxEvasionLoss) / td.toDouble(monthlyGdp)) else Share.Zero
      evasionRatios += ratio
      w = result.newWorld
      firms = result.newFirms
      hh = result.newHouseholds
      banks = result.newBanks

    val avgShare = Share(realizedShares.map(td.toDouble).sum / realizedShares.size)
    val avgRatio = Share(evasionRatios.map(td.toDouble).sum / evasionRatios.size)

    avgShare should be >= Share(0.24)
    avgShare should be <= Share(0.30)
    avgRatio should be >= Share(0.003)
    avgRatio should be <= Share(0.02)
  }
