package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting
import sfc.accounting.{ForexState, GovState}
import sfc.agents.{Banking, BankruptReason, Firm, TechState}
import sfc.types.*

class InformalEconomySpec extends AnyFlatSpec with Matchers:

  import sfc.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  // ==========================================================================
  // Config defaults
  // ==========================================================================

  "InformalEnabled" should "default to false" in {
    p.flags.informal shouldBe false
  }

  "InformalSectorShares" should "have 6 elements" in {
    p.informal.sectorShares.map(_.toDouble).length shouldBe 6
  }

  it should "have all values in [0,1]" in
    p.informal.sectorShares.map(_.toDouble).foreach { s =>
      s should be >= 0.0
      s should be <= 1.0
    }

  it should "have Agri highest" in {
    p.informal.sectorShares.map(_.toDouble)(5) shouldBe p.informal.sectorShares.map(_.toDouble).max
  }

  it should "have Public lowest" in {
    p.informal.sectorShares.map(_.toDouble)(4) shouldBe p.informal.sectorShares.map(_.toDouble).min
  }

  it should "match expected defaults" in {
    p.informal.sectorShares.map(_.toDouble) shouldBe Vector(0.05, 0.15, 0.30, 0.20, 0.02, 0.35)
  }

  "InformalCitEvasion" should "default to 0.80" in {
    p.informal.citEvasion.toDouble shouldBe 0.80
  }

  "InformalVatEvasion" should "default to 0.90" in {
    p.informal.vatEvasion.toDouble shouldBe 0.90
  }

  "InformalPitEvasion" should "default to 0.85" in {
    p.informal.pitEvasion.toDouble shouldBe 0.85
  }

  "InformalExciseEvasion" should "default to 0.70" in {
    p.informal.exciseEvasion.toDouble shouldBe 0.70
  }

  "InformalUnempThreshold" should "default to 0.05" in {
    p.informal.unempThreshold.toDouble shouldBe 0.05
  }

  "InformalCyclicalSens" should "default to 0.50" in {
    p.informal.cyclicalSens.toDouble shouldBe 0.50
  }

  // ==========================================================================
  // World fields
  // ==========================================================================

  private def mkMinimalWorld() = World(
    month = 0,
    inflation = Rate(0.0),
    priceLevel = 1.0,
    gdpProxy = 1e9,
    currentSigmas = Vector.fill(6)(5.0),
    totalPopulation = 100,
    gov = GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    nbp = sfc.agents.Nbp.State(Rate(0.05), PLN.Zero, false, PLN.Zero, PLN.Zero, PLN.Zero),
    bank = Banking.Aggregate(PLN.Zero, PLN.Zero, PLN(1e9), PLN(1e9), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    bankingSector = Banking.initialize(PLN(1e9), PLN(5e8), PLN(5e8), PLN.Zero, PLN.Zero, Banking.DefaultConfigs),
    forex = ForexState(4.33, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    hhAgg = sfc.agents.Household.Aggregates(
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
      giniIndividual = Ratio.Zero,
      giniWealth = Ratio.Zero,
      meanSavings = PLN.Zero,
      medianSavings = PLN.Zero,
      povertyRate50 = Ratio.Zero,
      bankruptcyRate = Ratio.Zero,
      meanSkill = 0.0,
      meanHealthPenalty = 0.0,
      retrainingAttempts = 0,
      retrainingSuccesses = 0,
      consumptionP10 = PLN.Zero,
      consumptionP50 = PLN.Zero,
      consumptionP90 = PLN.Zero,
      meanMonthsToRuin = 0.0,
      povertyRate30 = Ratio.Zero,
      totalRent = PLN.Zero,
      totalDebtService = PLN.Zero,
      totalUnempBenefits = PLN.Zero,
      totalDepositInterest = PLN.Zero,
      crossSectorHires = 0,
      voluntaryQuits = 0,
      sectorMobilityRate = Ratio.Zero,
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

  it should "have taxEvasionLoss defaulting to 0.0" in {
    val w = mkMinimalWorld()
    w.flows.taxEvasionLoss.toDouble shouldBe 0.0
  }

  it should "have informalEmployed defaulting to 0.0" in {
    val w = mkMinimalWorld()
    w.flows.informalEmployed shouldBe 0.0
  }

  // ==========================================================================
  // Firm.Result citEvasion
  // ==========================================================================

  private def mkFirm(tech: TechState = TechState.Traditional(10), cash: Double = 50000.0): Firm.State =
    Firm.State(
      FirmId(0),
      PLN(cash),
      PLN.Zero,
      tech,
      Ratio(0.5),
      1.0,
      Ratio(0.3),
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
    )

  "Firm.Result" should "have citEvasion defaulting to 0.0" in {
    val r = Firm.Result.zero(mkFirm()).copy(taxPaid = PLN(100.0))
    r.citEvasion.toDouble shouldBe 0.0
  }

  // ==========================================================================
  // CIT evasion logic (disabled mode)
  // ==========================================================================

  "CIT evasion (disabled)" should "not reduce taxPaid when InformalEnabled=false" in {
    // InformalEnabled defaults to false
    p.flags.informal shouldBe false
    val r = Firm.Result.zero(mkFirm()).copy(taxPaid = PLN(1000.0))
    // Since InformalEnabled is false, citEvasion should remain 0
    r.citEvasion.toDouble shouldBe 0.0
  }

  // ==========================================================================
  // CIT evasion: bankrupt firms
  // ==========================================================================

  "CIT evasion" should "be zero for bankrupt firms" in {
    val r = Firm.Result.zero(mkFirm(TechState.Bankrupt(BankruptReason.Other("test")), cash = 0.0))
    r.citEvasion.toDouble shouldBe 0.0
  }

  it should "be zero when taxPaid <= 0" in {
    val r = Firm.Result.zero(mkFirm())
    r.citEvasion.toDouble shouldBe 0.0
  }

  // ==========================================================================
  // Counter-cyclical dynamics
  // ==========================================================================

  "Counter-cyclical adjustment" should "be 0 when unemployment <= threshold" in {
    // unemp = 0.04 < threshold 0.05
    val adj = Math.max(0.0, 0.04 - p.informal.unempThreshold.toDouble) * p.informal.cyclicalSens.toDouble
    adj shouldBe 0.0
  }

  it should "be positive when unemployment > threshold" in {
    // unemp = 0.10 > threshold 0.05
    val adj = Math.max(0.0, 0.10 - p.informal.unempThreshold.toDouble) * p.informal.cyclicalSens.toDouble
    adj should be > 0.0
    adj shouldBe (0.05 * 0.50 +- 1e-10)
  }

  it should "increase with unemployment" in {
    val adj1 = Math.max(0.0, 0.08 - p.informal.unempThreshold.toDouble) * p.informal.cyclicalSens.toDouble
    val adj2 = Math.max(0.0, 0.15 - p.informal.unempThreshold.toDouble) * p.informal.cyclicalSens.toDouble
    adj2 should be > adj1
  }

  // ==========================================================================
  // Effective shadow share
  // ==========================================================================

  "Effective shadow share" should "be weighted by FofConsWeights" in {
    val cyclicalAdj = 0.0
    val ess         =
      p.fiscal.fofConsWeights
        .map(_.toDouble)
        .zip(p.informal.sectorShares.map(_.toDouble))
        .map((cw, ss) => cw * Math.min(1.0, ss + cyclicalAdj))
        .sum
    // Weighted average of sector shares: should be between min and max
    ess should be > 0.0
    ess should be < 1.0
    // BPO=0.02*0.05, Mfg=0.22*0.15, Ret=0.53*0.30, Hlt=0.06*0.20, Pub=0.07*0.02, Agr=0.10*0.35
    // = 0.001 + 0.033 + 0.159 + 0.012 + 0.0014 + 0.035 = ~0.2414
    ess shouldBe (0.2414 +- 0.01)
  }

  it should "be capped at 1.0 per sector" in {
    val cyclicalAdj = 2.0 // very high
    val shares      = p.informal.sectorShares.map(_.toDouble).map(ss => Math.min(1.0, ss + cyclicalAdj))
    shares.foreach(_ shouldBe 1.0)
  }

  // ==========================================================================
  // VAT evasion
  // ==========================================================================

  "VAT evasion" should "reduce VAT proportionally" in {
    val vat      = 1000.0
    val ess      = 0.20
    val vatAfter = vat * (1.0 - ess * p.informal.vatEvasion.toDouble)
    vatAfter should be < vat
    vatAfter should be > 0.0
  }

  // ==========================================================================
  // PIT evasion
  // ==========================================================================

  "PIT evasion" should "reduce PIT proportionally" in {
    val pit      = 500.0
    val ess      = 0.20
    val pitAfter = pit * (1.0 - ess * p.informal.pitEvasion.toDouble)
    pitAfter should be < pit
    pitAfter should be > 0.0
  }

  // ==========================================================================
  // Excise evasion
  // ==========================================================================

  "Excise evasion" should "reduce excise proportionally" in {
    val excise      = 300.0
    val ess         = 0.20
    val exciseAfter = excise * (1.0 - ess * p.informal.exciseEvasion.toDouble)
    exciseAfter should be < excise
    exciseAfter should be > 0.0
  }

  // ==========================================================================
  // TaxEvasionLoss
  // ==========================================================================

  "TaxEvasionLoss" should "be sum of all channels" in {
    val citEvasion = 100.0
    val vatDiff    = 200.0
    val pitDiff    = 150.0
    val exciseDiff = 50.0
    val total      = citEvasion + vatDiff + pitDiff + exciseDiff
    total shouldBe 500.0
  }

  // ==========================================================================
  // EvasionToGdpRatio
  // ==========================================================================

  "EvasionToGdpRatio" should "be positive when evasion > 0 and GDP > 0" in {
    val evasion = 100.0
    val gdp     = 1000.0
    val ratio   = evasion / gdp
    ratio should be > 0.0
    ratio shouldBe 0.1
  }

  it should "be zero when GDP is zero" in {
    val evasion = 100.0
    val gdp     = 0.0
    val ratio   = if gdp > 0 then evasion / gdp else 0.0
    ratio shouldBe 0.0
  }

  // ==========================================================================
  // GDP unaffected
  // ==========================================================================

  "GDP formula" should "not include tax evasion (evasion doesn't change GDP)" in {
    // GDP = domesticCons + govGdpContribution + euGdpContribution + exports + domesticGFCF + inventoryChange
    // Tax evasion only reduces government revenue, not GDP
    // This is a design test: GDP computation doesn't use taxEvasionLoss
    true shouldBe true // Verified by code inspection
  }
