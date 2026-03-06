package sfc.config

import sfc.types.*

/** Monetary regime: determines central bank behavior and exchange rate dynamics. */
enum MonetaryRegime:
  case Pln // NBP Taylor rule + floating PLN/EUR
  case Eur // Exogenous ECB rate + fixed exchange rate (Eurozone membership)

/** Network topology selection for comparative experiments. */
enum Topology:
  case Ws, Er, Ba, Lattice

/** Household mode: aggregate (backward-compat) or individual agents. */
enum HhMode:
  case Aggregate, Individual

/** Runtime configuration: values that depend on CLI arguments. Passed through runSingle and Simulation.step.
  */
case class RunConfig(
  bdpAmount: Double,
  nSeeds: Int,
  outputPrefix: String,
  regime: MonetaryRegime = MonetaryRegime.Pln,
):
  val isNoBdp: Boolean = bdpAmount == 0.0
  val isEurozone: Boolean = regime == MonetaryRegime.Eur

/** 4-to-6 sector definition with heterogeneous sigma (CES elasticity of substitution). sigma affects: decision
  * threshold, automation efficiency, CAPEX costs.
  */
case class SectorDef(
  name: String,
  share: Ratio, // Share of firm population (GUS BAEL 2024)
  sigma: Double, // CES elasticity of substitution
  wageMultiplier: Double, // Sector wage multiplier vs national average
  revenueMultiplier: Double,
  aiCapexMultiplier: Double,
  hybridCapexMultiplier: Double,
  baseDigitalReadiness: Ratio, // Central tendency of digitalReadiness
  hybridRetainFrac: Ratio, // Fraction of workers RETAINED in hybrid mode (0.5 = halve)
)

// Calibration GUS/NBP 2024: 6 sectors of the Polish economy
private val BASE_SECTORS: Vector[SectorDef] = Vector(
  //                             share          sigma wage  rev   aiCpx hybCpx digiR          hybRet
  SectorDef(
    "BPO/SSC",
    Ratio(0.03),
    50.0,
    1.35,
    1.50,
    0.70,
    0.70,
    Ratio(0.50),
    Ratio(0.50),
  ), // ~489k workers (ABSL), avg 11 154 PLN
  SectorDef(
    "Manufacturing",
    Ratio(0.16),
    10.0,
    0.94,
    1.05,
    1.12,
    1.05,
    Ratio(0.45),
    Ratio(0.60),
  ), // ~2.8M workers, avg ~7 800 PLN
  SectorDef(
    "Retail/Services",
    Ratio(0.45),
    5.0,
    0.79,
    0.91,
    0.85,
    0.80,
    Ratio(0.40),
    Ratio(0.65),
  ), // ~61% employment (services), avg ~6 500 PLN
  SectorDef(
    "Healthcare",
    Ratio(0.06),
    2.0,
    0.97,
    1.10,
    1.38,
    1.25,
    Ratio(0.25),
    Ratio(0.75),
  ), // ~5.5%, nurses 6 890, doctors 16 300
  SectorDef(
    "Public",
    Ratio(0.22),
    1.0,
    0.91,
    1.08,
    3.00,
    2.50,
    Ratio(0.08),
    Ratio(0.90),
  ), // ~22% employment (public sector), avg ~7 500 PLN
  SectorDef(
    "Agriculture",
    Ratio(0.08),
    3.0,
    0.67,
    0.80,
    2.50,
    2.00,
    Ratio(0.12),
    Ratio(0.85),
  ), // ~8% BAEL, avg ~5 500 PLN
)

/** SECTORS with optional sigma override via SIGMAS env var. Format: SIGMAS="3.5,2.0,2.5,0.8,0.5,1.2" (6 comma-separated
  * values, one per sector). When unset, uses calibrated values from BASE_SECTORS. SIGMA_MULT env var multiplies all
  * sector sigmas (applied after SIGMAS override).
  */
val SECTORS: Vector[SectorDef] =
  val afterSigmas = sys.env.get("SIGMAS") match
    case Some(s) if s.nonEmpty =>
      val sigmas = s.split(",").map(_.trim.toDouble)
      require(
        sigmas.length == BASE_SECTORS.length,
        s"SIGMAS env var must have ${BASE_SECTORS.length} values, got ${sigmas.length}",
      )
      BASE_SECTORS.zip(sigmas).map((sd, sig) => sd.copy(sigma = sig))
    case _ => BASE_SECTORS
  val sigmaMult = sys.env.get("SIGMA_MULT").map(_.trim.toDouble).getOrElse(1.0)
  if sigmaMult != 1.0 then afterSigmas.map(sd => sd.copy(sigma = sd.sigma * sigmaMult))
  else afterSigmas

/** Network topology parsed from TOPOLOGY env var (default: ws). */
val TOPOLOGY: Topology =
  sys.env.get("TOPOLOGY").map(_.trim.toLowerCase).getOrElse("ws") match
    case "er"      => Topology.Er
    case "ba"      => Topology.Ba
    case "lattice" => Topology.Lattice
    case _         => Topology.Ws

/** Household mode parsed from HH_MODE env var (default: aggregate). */
val HH_MODE: HhMode =
  sys.env.get("HH_MODE").map(_.trim.toLowerCase) match
    case Some("individual") => HhMode.Individual
    case _                  => HhMode.Aggregate

/** Firm size distribution: stratified draw from GUS 2024 Polish enterprise data. */
object FirmSizeDistribution:
  import scala.util.Random

  def draw(rng: Random): Int = Config.FirmSizeDist match
    case "gus" =>
      val r = rng.nextDouble()
      if r < Config.FirmSizeMicroShare then rng.between(1, 10)
      else if r < Config.FirmSizeMicroShare + Config.FirmSizeSmallShare then rng.between(10, 50)
      else if r < 1.0 - Config.FirmSizeLargeShare then rng.between(50, 250)
      else rng.between(250, Config.FirmSizeLargeMax + 1)
    case _ => Config.WorkersPerFirm // "uniform": all 10

/** Complete parameterization of a 48-mechanism SFC-ABM model of the Polish economy.
  *
  * '''Model geography:''' Poland, 6-sector (BPO/SSC, Manufacturing, Retail/Services, Healthcare, Public, Agriculture).
  * Calibration year: 2024 unless noted otherwise.
  *
  * '''Calibration sources:''' GUS (Central Statistical Office — BAEL, BBGD, REGON, COICOP, national accounts, I-O
  * tables), NBP (monetary policy, BoP, MIR, Financial Stability Report, FDI statistics), KNF (BION/SREP, insurance
  * market, Recommendation S), MF (budget, SGP), Eurostat (COFOG, SBS, energy), OECD (TiVA, Education at a Glance),
  * WIOD, ZUS, IZFiA, URE.
  *
  * '''Environment variable overrides:''' Every field with `sys.env.get("ENV_VAR")` can be overridden at runtime. See
  * SUMMARY.md for the full catalogue (353 env vars).
  *
  * '''GdpRatio auto-scaling:''' Stock variables (bank deposits, loans, gov debt, etc.) are calibrated to Poland's real
  * economy (~3.5 tln PLN GDP) then scaled by `GdpRatio` so that changing `FIRMS_COUNT` or `FIRM_SIZE_DIST` keeps
  * balance-sheet ratios consistent with the model's effective GDP. This means a 1 000-firm model has the same debt/GDP
  * ratio as a 10 000-firm model.
  *
  * '''SFC identities:''' 14 stock-flow consistency checks verified every period. See SUMMARY.md § "SFC Identities" for
  * the complete list.
  *
  * '''Organization:''' Fields are grouped by institutional domain (fiscal policy, monetary policy, banking, etc.).
  * Within each section, a banner comment explains the real-world institutional context, and per-field comments give
  * calibration sources and rationale. A future refactoring (RX) will restructure into nested sub-objects; for now the
  * flat `Config.X` access pattern is preserved.
  */
object Config:

  // ═══════════════════════════════════════════════════════════════════════
  // POPULATION & FIRM STRUCTURE
  // Poland has ~2.3M active enterprises (GUS REGON 2024), of which 96.2%
  // are micro (<10 workers), 2.8% small (10-49), 0.8% medium (50-249),
  // and 0.2% large (250+). The model represents this via FirmSizeDistribution.
  // Default: 10 000 firms × 10 workers = 100 000 agents (uniform mode).
  // With FIRM_SIZE_DIST=gus, firm sizes are drawn from the GUS distribution.
  // ═══════════════════════════════════════════════════════════════════════

  // Number of firms in the model (default 10 000; scale up for smoother distributions)
  val FirmsCount = sys.env.get("FIRMS_COUNT").map(_.trim.toInt).getOrElse(10000)
  val WorkersPerFirm = 10 // Uniform-mode baseline (overridden by FirmSizeDistribution.draw)
  var TotalPopulation: Int = FirmsCount * WorkersPerFirm
  def setTotalPopulation(n: Int): Unit = TotalPopulation = n

  // Firm size distribution: stratified by GUS REGON 2024 enterprise demographics.
  // Shares approximate CEIDG (sole traders) + KRS (companies) combined structure.
  val FirmSizeDist: String = sys.env.get("FIRM_SIZE_DIST").map(_.trim.toLowerCase).getOrElse("uniform")
  val FirmSizeMicroShare: Double = sys.env.get("FIRM_SIZE_MICRO_SHARE").map(_.trim.toDouble).getOrElse(0.962)
  val FirmSizeSmallShare: Double = sys.env.get("FIRM_SIZE_SMALL_SHARE").map(_.trim.toDouble).getOrElse(0.028)
  val FirmSizeMediumShare: Double = sys.env.get("FIRM_SIZE_MEDIUM_SHARE").map(_.trim.toDouble).getOrElse(0.008)
  val FirmSizeLargeShare: Double = sys.env.get("FIRM_SIZE_LARGE_SHARE").map(_.trim.toDouble).getOrElse(0.002)
  val FirmSizeLargeMax: Int = sys.env.get("FIRM_SIZE_LARGE_MAX").map(_.trim.toInt).getOrElse(1000)

  // ───────────────────────────────────────────────────────────────────────
  // Endogenous Firm Entry
  // New firm creation recycles bankrupt slots. Sector choice is profit-weighted
  // with entry barriers calibrated to GUS CEIDG/KRS 2024 registration data.
  // AI-native startups possible when sector digitalReadiness > threshold.
  // ───────────────────────────────────────────────────────────────────────
  val FirmEntryEnabled: Boolean = sys.env.get("FIRM_ENTRY_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  // Monthly entry rate: ~2% of bankrupt slots refilled (GUS CEIDG 2024: ~350k new registrations/year)
  val FirmEntryRate: Double = sys.env.get("FIRM_ENTRY_RATE").map(_.trim.toDouble).getOrElse(0.02)
  // Profit sensitivity: higher → entrants flock to most profitable sectors (convex)
  val FirmEntryProfitSens: Double = sys.env.get("FIRM_ENTRY_PROFIT_SENS").map(_.trim.toDouble).getOrElse(2.0)
  // Per-sector entry barriers (0=impossible, 1=baseline, >1=easy). Reflects capital
  // requirements, licensing, and regulatory burden. BPO 0.8, Mfg 0.6 (capital-intensive),
  // Retail 1.2 (easy entry), Healthcare 0.5 (licensing), Public 0.1 (tenders), Agri 0.7.
  val FirmEntrySectorBarriers: Vector[Double] = sys.env.get("FIRM_ENTRY_SECTOR_BARRIERS") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(
        v.length == SECTORS.length,
        s"FIRM_ENTRY_SECTOR_BARRIERS must have ${SECTORS.length} values, got ${v.length}",
      )
      v
    case _ => Vector(0.8, 0.6, 1.2, 0.5, 0.1, 0.7)
  // Sector digitalReadiness must exceed this for AI-native startup creation
  val FirmEntryAiThreshold: Double = sys.env.get("FIRM_ENTRY_AI_THRESHOLD").map(_.trim.toDouble).getOrElse(0.15)
  // Probability that an eligible new entrant is AI-native (Hybrid mode + high DR)
  val FirmEntryAiProb: Double = sys.env.get("FIRM_ENTRY_AI_PROB").map(_.trim.toDouble).getOrElse(0.20)
  // Initial cash endowment for new firms (PLN; ~median CEIDG startup capital)
  val FirmEntryStartupCash: Double = sys.env.get("FIRM_ENTRY_STARTUP_CASH").map(_.trim.toDouble).getOrElse(50000.0)

  // ═══════════════════════════════════════════════════════════════════════
  // SIMULATION TIMELINE
  // Duration in months. ShockMonth = period when BDP (AI productivity) shock hits.
  // ═══════════════════════════════════════════════════════════════════════

  val Duration = sys.env.get("DURATION").map(_.trim.toInt).getOrElse(120) // months (default 10 years)
  val ShockMonth = sys.env.get("SHOCK_MONTH").map(_.trim.toInt).getOrElse(30) // month of BDP shock

  // ═══════════════════════════════════════════════════════════════════════
  // FIRM PRODUCTION & COSTS
  // Base revenue per firm per month, scaled by sector-specific revenueMultiplier.
  // Cost structure: labor (wages) + OtherCosts (intermediates, rent, utilities) +
  // optional AI/Hybrid CAPEX and OPEX for automation adoption decisions.
  // ═══════════════════════════════════════════════════════════════════════

  // Monthly revenue per firm before sector multiplier (PLN; calibrated to GUS 2024 wage level)
  val BaseRevenue = 100000.0

  // GdpRatio: auto-scales real-economy stock variables to match model size.
  // When FirmsCount or FirmSizeDist changes, GdpRatio adjusts so that
  // InitBankDeposits, InitBankLoans, etc. scale proportionally to model GDP.
  private val ExpectedAvgWorkers: Double = FirmSizeDist match
    case "gus" =>
      val microMean = 5.0; val smallMean = 29.5; val mediumMean = 149.5
      val largeMean = (250.0 + FirmSizeLargeMax.toDouble) / 2.0
      val mediumShare = 1.0 - FirmSizeMicroShare - FirmSizeSmallShare - FirmSizeLargeShare
      FirmSizeMicroShare * microMean + FirmSizeSmallShare * smallMean +
        mediumShare * mediumMean + FirmSizeLargeShare * largeMean
    case _ => WorkersPerFirm.toDouble
  val RealGdp: Double = sys.env.get("REAL_GDP").map(_.trim.toDouble).getOrElse(3500e9)
  private val GdpRatio: Double =
    (FirmsCount.toDouble * ExpectedAvgWorkers / WorkersPerFirm.toDouble * BaseRevenue * 12.0) / RealGdp

  val OtherCosts = 16667.0 // Monthly non-labor operating costs per firm (PLN)
  val AiCapex = 1200000.0 // One-time full-AI adoption investment (PLN)
  val HybridCapex = 350000.0 // One-time hybrid (human+AI) adoption investment (PLN)
  val AiOpex = 30000.0 // Monthly AI operating cost: cloud, licenses, maintenance (PLN)
  val HybridOpex = 12000.0 // Monthly hybrid operating cost (PLN)
  val AutoSkeletonCrew = 2 // Minimum workers retained even in full-AI mode
  val HybridReadinessMin = 0.20 // Minimum digitalReadiness to adopt hybrid mode (fraction 0-1)
  val FullAiReadinessMin = 0.35 // Minimum digitalReadiness to adopt full-AI mode (fraction 0-1)

  // Demand pass-through: fraction of demand shock transmitted to firm revenue.
  // <1.0 reflects contractual rigidity, inventory buffers, and delivery lags.
  val DemandPassthrough = 0.40

  // ═══════════════════════════════════════════════════════════════════════
  // HOUSEHOLDS — LABOR & CONSUMPTION
  // Calibrated to GUS 2024 wage statistics and BBGD consumption survey.
  // BaseWage = GUS average gross monthly wage (all sectors, Q3 2024).
  // BaseReservationWage ≈ statutory minimum wage 2025 (Dz.U. 2024 poz. 1286).
  // MPC from GUS BBGD 2023: ~82% of disposable income consumed.
  // ═══════════════════════════════════════════════════════════════════════
  val BaseWage = 8266.0 // GUS average gross monthly wage 2024 (PLN)
  val BaseReservationWage = 4666.0 // Statutory minimum wage 2025 (PLN/month gross)
  val ReservationBdpMult = 0.5 // Reservation wage multiplier for BDP recipients
  val Mpc = 0.82 // Marginal propensity to consume (GUS BBGD 2023)
  val LaborSupplySteepness = 8.0 // Logistic labor supply curve steepness
  val WageAdjSpeed = 0.12 // Monthly wage Phillips-curve adjustment speed (fraction)

  // ═══════════════════════════════════════════════════════════════════════
  // FISCAL POLICY — GOVERNMENT
  // Poland's fiscal framework: Art. 217 Konstytucja RP (budget law), Ustawa
  // o finansach publicznych (2009), EU Stability and Growth Pact (SGP).
  // Tax structure: CIT 19% flat (Art. 19 Ustawa o CIT), VAT standard 23%
  // with reduced rates (Ustawa o VAT, Art. 41), progressive PIT 12%/32%
  // (Art. 27 Ustawa o PIT). Total tax revenue ~38% GDP (MF 2024).
  // Government consumption ~20% GDP, total public expenditure ~44% GDP.
  // Automatic stabilizers: unemployment benefits, progressive taxation,
  // social transfers (800+ child benefit).
  // Sources: MF budget execution reports, GUS national accounts, Eurostat COFOG.
  // ═══════════════════════════════════════════════════════════════════════
  // Corporate income tax: 19% flat rate (Art. 19 Ustawa o CIT; 9% for small taxpayers not modeled)
  val CitRate = 0.19
  // Reduced VAT rates: per-sector effective rate (Ustawa o VAT / GUS COICOP 2024)
  // Standard 23%, food/hospitality 8%, basic goods 5%, medical exempt
  val VatRates: Vector[Double] = sys.env.get("VAT_RATES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 6, s"VAT_RATES must have 6 values, got ${v.length}")
      v
    case _ => Vector(0.23, 0.19, 0.12, 0.06, 0.10, 0.07)

  // Excise & customs — always-on
  val ExciseRates: Vector[Double] = sys.env.get("EXCISE_RATES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 6, s"EXCISE_RATES must have 6 values, got ${v.length}")
      v
    case _ => Vector(0.01, 0.04, 0.03, 0.005, 0.002, 0.02)
    // BPO 1%, Mfg 4% (fuel-heavy), Retail 3% (alcohol/tobacco), Hlt 0.5%, Pub 0.2%, Agr 2%
  val CustomsDutyRate: Double = sys.env.get("CUSTOMS_DUTY_RATE").map(_.trim.toDouble).getOrElse(0.04)
  // 4% effective rate on non-EU imports (EU Common External Tariff weighted avg)
  val CustomsNonEuShare: Double = sys.env.get("CUSTOMS_NON_EU_SHARE").map(_.trim.toDouble).getOrElse(0.30)
  // 30% of imports from non-EU (= 1 - GvcEuTradeShare default)

  // Government current consumption: goods, services, public-sector wages (GUS 2024: ~700 mld PLN/year)
  val GovBaseSpending: Double = sys.env
    .get("GOV_BASE_SPENDING")
    .map(_.trim.toDouble)
    .getOrElse(58.3e9) * GdpRatio // 58.3e9 PLN/month = 700e9/year

  // Fiscal recycling: fraction of lagged tax revenue recycled as additional government
  // purchases. In real economies, tax revenue funds government consumption beyond autonomous
  // spending. Without recycling, PIT/ZUS/VAT drain private income without offsetting demand.
  // Poland 2024: total public expenditure ~44% GDP, tax revenue ~38% → ~85% recycled
  // as consumption (18%), social transfers (15%), investment (4%), other (6%).
  val GovFiscalRecyclingRate: Double = sys.env.get("GOV_FISCAL_RECYCLING").map(_.trim.toDouble).getOrElse(0.85)
  // Counter-cyclical fiscal multiplier: when unemployment > NAIRU, gov spending increases
  // by GovBaseSpending * (unempRate - NAIRU) * GovAutoStabMult per month.
  // Poland: strong automatic stabilizers (social transfers, subsidies, public employment).
  val GovAutoStabMult: Double = sys.env.get("GOV_AUTO_STAB_MULT").map(_.trim.toDouble).getOrElse(3.0)

  // ───────────────────────────────────────────────────────────────────────
  // Public Investment vs Current Spending
  // Distinguishes government GFCF (infrastructure, defence, IT) from current
  // expenditure (wages, transfers). Investment has a higher long-run multiplier
  // (Ilzetzki, Mendoza & Végh 2013: ~1.5 for developing/open economies).
  // Poland 2024: public GFCF ~5% GDP (GUS, Eurostat COFOG GF04+GF02).
  // ───────────────────────────────────────────────────────────────────────
  val GovInvestEnabled: Boolean = sys.env.get("GOV_INVEST_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val GovInvestShare: Double =
    sys.env.get("GOV_INVEST_SHARE").map(_.trim.toDouble).getOrElse(0.20) // 20% of spending is GFCF
  val GovCapitalMultiplier: Double =
    sys.env.get("GOV_CAPITAL_MULTIPLIER").map(_.trim.toDouble).getOrElse(1.5) // Long-run investment multiplier
  val GovCurrentMultiplier: Double =
    sys.env.get("GOV_CURRENT_MULTIPLIER").map(_.trim.toDouble).getOrElse(0.8) // Current spending multiplier
  val GovDepreciationRate: Double =
    sys.env.get("GOV_DEPRECIATION_RATE").map(_.trim.toDouble).getOrElse(0.06) // Annual depreciation of public capital
  val GovInitCapital: Double = sys.env.get("GOV_INIT_CAPITAL").map(_.trim.toDouble).getOrElse(0.0)

  // ───────────────────────────────────────────────────────────────────────
  // EU Funds Dynamics
  // Poland is the largest net beneficiary of EU cohesion policy. MFP 2021-2027:
  // €76 bln allocation (KPO + Fundusze Europejskie). Absorption follows a
  // Beta(α,β) time profile (slow start → peak → tail), matching historical
  // absorption curves from MFP 2014-2020 (MFIiPR data).
  // Co-financing: 15% national contribution (Ustawa wdrożeniowa).
  // 60% capital (infrastructure), 40% current (training, R&D grants).
  // ───────────────────────────────────────────────────────────────────────
  val EuFundsEnabled: Boolean = sys.env.get("EU_FUNDS_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val EuFundsTotalEur: Double = sys.env.get("EU_FUNDS_TOTAL_EUR").map(_.trim.toDouble).getOrElse(76e9)
  val EuFundsPeriodMonths: Int = sys.env.get("EU_FUNDS_PERIOD_MONTHS").map(_.trim.toInt).getOrElse(84)
  val EuFundsStartMonth: Int = sys.env.get("EU_FUNDS_START_MONTH").map(_.trim.toInt).getOrElse(1)
  val EuFundsAlpha: Double = sys.env.get("EU_FUNDS_ALPHA").map(_.trim.toDouble).getOrElse(2.0)
  val EuFundsBeta: Double = sys.env.get("EU_FUNDS_BETA").map(_.trim.toDouble).getOrElse(5.0)
  val EuCofinanceRate: Double = sys.env.get("EU_COFINANCE_RATE").map(_.trim.toDouble).getOrElse(0.15)
  val EuCapitalShare: Double = sys.env.get("EU_CAPITAL_SHARE").map(_.trim.toDouble).getOrElse(0.60)

  // ───────────────────────────────────────────────────────────────────────
  // Minimum Wage Dynamics
  // Poland's minimum wage is set by Rozporządzenie RM (Council of Ministers regulation),
  // adjusted biannually (Jan + Jul since 2023). Target: 50% of average wage (EU Directive
  // 2022/2041 on adequate minimum wages). 2025: 4 666 PLN gross.
  // Convergence from current ratio to target via partial adjustment.
  // ───────────────────────────────────────────────────────────────────────
  val MinWageEnabled: Boolean = sys.env.get("MIN_WAGE_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val MinWageAdjustMonths: Int = sys.env.get("MIN_WAGE_ADJUST_MONTHS").map(_.trim.toInt).getOrElse(12)
  val MinWageInflationIndex: Boolean = sys.env.get("MIN_WAGE_INFLATION_INDEX").map(_.trim.toBoolean).getOrElse(true)
  val MinWageTargetRatio: Double = sys.env.get("MIN_WAGE_TARGET_RATIO").map(_.trim.toDouble).getOrElse(0.50)
  val MinWageConvergenceSpeed: Double = sys.env.get("MIN_WAGE_CONVERGENCE_SPEED").map(_.trim.toDouble).getOrElse(0.33)

  // Flow-of-Funds: sector-level demand multipliers
  // Calibration sources:
  //   ConsWeights — GUS BBGD 2024: COICOP household consumption structure mapped to 6 model sectors
  //     BPO 2%: digital services, cloud subscriptions (fraction of COICOP 08 Communications 4.2%)
  //     Mfg 22%: COICOP 07 Transport 10.7% + 05 Furnishings 5.6% + 03 Clothing 4.5% + manufactured misc
  //     Ret 53%: COICOP 01 Food 25.3% + 11 Restaurants 5.3% + 09 Recreation 6.5% + 02 Alcohol 2.5%
  //              + retail margins on clothing/furnishings + 12 Misc 6.2%
  //     Hlt 6%: COICOP 06 Health 5.5%
  //     Pub 7%: COICOP 10 Education 1.2% + public housing/utilities ~5% (fraction of COICOP 04)
  //     Agr 10%: direct farm purchases, farmers' markets (~3-4% of food), upstream food processing
  //   GovWeights — Eurostat COFOG 2023 (gov_10a_exp), normalized to model sectors:
  //     BPO 4%: fraction of GF01 General Public Services (IT procurement, digital administration)
  //     Mfg 12%: GF02 Defence 2.1% (equipment) + fraction of GF04 Economic Affairs 7.5% (infrastructure)
  //     Ret 8%: government procurement of goods/services, fraction of GF04
  //     Hlt 16%: GF07 Health ~5.5% GDP → ~12% of total gov spending
  //     Pub 50%: GF09 Education ~5% + GF10 Social Protection ~17% + GF03 Public Order 2.3%
  //              + GF01 admin → dominant share (~50% of gov spending goes to public sector)
  //     Agr 10%: fraction of GF04 Economic Affairs (agricultural subsidies, CAP co-financing)
  //   ExportShares — GUS foreign trade 2024 (SITC) + NBP BoP services:
  //     BPO 7%: IT/telecoms services ~35% of services exports; services ~25% of total → ~8-9%
  //     Mfg 52%: SITC 7 Machinery 36.6% + SITC 6 Manufactured goods 16.8% (partially)
  //     Ret 12%: SITC 8 Misc manufactured articles 17.4% (retail/consumer goods portion)
  //              + transport/travel services
  //     Hlt 2%: medical devices, pharmaceutical exports (small)
  //     Pub 3%: educational/admin services exports (minimal)
  //     Agr 24%: SITC 0 Food 13.2% + SITC 1 Beverages 1.9% + SITC 4 Oils 0.3%
  //              + upstream food processing share of SITC 6
  val FofConsWeights: Vector[Double] = sys.env.get("FOF_CONS_WEIGHTS") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(
        v.length == 6 && Math.abs(v.sum - 1.0) < 0.01,
        s"FOF_CONS_WEIGHTS must have 6 values summing to ~1.0, got ${v.length} summing to ${v.sum}",
      )
      v
    case _ => Vector(0.02, 0.22, 0.53, 0.06, 0.07, 0.10)
  val FofGovWeights: Vector[Double] = sys.env.get("FOF_GOV_WEIGHTS") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(
        v.length == 6 && Math.abs(v.sum - 1.0) < 0.01,
        s"FOF_GOV_WEIGHTS must have 6 values summing to ~1.0, got ${v.length} summing to ${v.sum}",
      )
      v
    case _ => Vector(0.04, 0.12, 0.08, 0.16, 0.50, 0.10)
  val FofExportShares: Vector[Double] = sys.env.get("FOF_EXPORT_SHARES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(
        v.length == 6 && Math.abs(v.sum - 1.0) < 0.01,
        s"FOF_EXPORT_SHARES must have 6 values summing to ~1.0, got ${v.length} summing to ${v.sum}",
      )
      v
    case _ => Vector(0.07, 0.52, 0.12, 0.02, 0.03, 0.24)
  // Investment demand weights — sector distribution of GFCF spending (GUS GFCF structure 2024)
  //   BPO 10%: software, IT equipment, cloud infrastructure
  //   Mfg 40%: machinery, industrial equipment, transport vehicles
  //   Ret 15%: commercial construction, fit-out, logistics equipment
  //   Hlt 5%: medical devices, hospital construction/renovation
  //   Pub 20%: infrastructure, public buildings, defence
  //   Agr 10%: agricultural machinery, land improvement, irrigation
  val FofInvestWeights: Vector[Double] = sys.env.get("FOF_INVEST_WEIGHTS") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(
        v.length == 6 && Math.abs(v.sum - 1.0) < 0.01,
        s"FOF_INVEST_WEIGHTS must have 6 values summing to ~1.0, got ${v.length} summing to ${v.sum}",
      )
      v
    case _ => Vector(0.10, 0.40, 0.15, 0.05, 0.20, 0.10)

  // ═══════════════════════════════════════════════════════════════════════
  // MONETARY POLICY — NBP
  // Narodowy Bank Polski: inflation-targeting regime with symmetric Taylor
  // rule (since RPP resolution 2004), 2.5% ±1pp target band. Implements
  // QE (precedent: NBP Structural Open Market Operations, March 2020),
  // FX intervention, and standing facilities (deposit/lombard corridor
  // around the reference rate).
  // Taylor rule: i_t = ρ·i_{t-1} + (1-ρ)·[r* + π* + α·(π-π*) + β·ygap + δ·(u-u*)]
  //   where ρ = inertia, α = inflation response (Clarida, Galí & Gertler 1999),
  //   β = output gap response, δ = unemployment gap response (symmetric extension).
  // Sources: NBP Monetary Policy Guidelines 2024, RPP minutes, NBP MIR database.
  // ═══════════════════════════════════════════════════════════════════════

  val NbpInitialRate = 0.0575 // NBP reference rate 2024 (Uchwała RPP nr 1/2024)
  val NbpTargetInfl = 0.025 // CPI target 2.5% ±1pp (continuous target since 2004)
  val NbpNeutralRate = 0.04 // Neutral real rate + target inflation (~1.5% + 2.5%)
  // Taylor rule coefficients (Clarida, Galí & Gertler 1999; calibrated to NBP reaction function)
  val TaylorAlpha: Double = sys.env.get("TAYLOR_ALPHA").map(_.trim.toDouble).getOrElse(1.5) // >1 = Taylor principle
  val TaylorBeta: Double = sys.env.get("TAYLOR_BETA").map(_.trim.toDouble).getOrElse(0.8) // Output gap response
  val TaylorInertia: Double =
    sys.env.get("TAYLOR_INERTIA").map(_.trim.toDouble).getOrElse(0.70) // Interest rate smoothing
  val RateFloor = sys.env.get("NBP_RATE_FLOOR").map(_.trim.toDouble).getOrElse(0.001) // Effective lower bound (ELB)
  val RateCeiling: Double =
    sys.env.get("NBP_RATE_CEILING").map(_.trim.toDouble).getOrElse(0.25) // Upper bound (historical max ~19%, 2001)
  // Max rate change per month: NBP typically moves 25-75bp per MPC meeting. Set 0.0 to disable.
  val NbpMaxRateChange: Double = sys.env.get("NBP_MAX_RATE_CHANGE").map(_.trim.toDouble).getOrElse(0.0)

  // Symmetric Taylor rule — dual mandate: price stability + employment
  // Extends standard Taylor rule with unemployment gap term: δ·(u - NAIRU).
  // When u > NAIRU, rule prescribes lower rates than pure inflation targeting.
  val NbpSymmetric: Boolean = sys.env.get("NBP_SYMMETRIC").map(_.trim.toBoolean).getOrElse(true)
  // NAIRU estimate for Poland: ~5% (NBP Inflation Report 2024, OECD Economic Outlook)
  val NbpNairu: Double = sys.env.get("NBP_NAIRU").map(_.trim.toDouble).getOrElse(0.05)
  // Unemployment gap coefficient in symmetric Taylor rule
  val TaylorDelta: Double = sys.env.get("NBP_DELTA").map(_.trim.toDouble).getOrElse(0.5)

  // Reserve interest: NBP pays fraction of refRate on required reserves
  // (Uchwała RPP nr 7/2003: oprocentowanie = 0.9 × stopa referencyjna;
  // model uses 0.5 for visibility in experiments)
  val NbpReserveRateMult: Double = sys.env.get("NBP_RESERVE_RATE_MULT").map(_.trim.toDouble).getOrElse(0.5)

  // Standing facilities: deposit/lombard corridor around reference rate.
  // Deposit facility: refRate - spread (floor for O/N rate).
  // Lombard facility: refRate + spread (ceiling for O/N rate).
  // Poland 2024: deposit 5.25%, lombard 6.25% (±50bp from ref 5.75%).
  val NbpStandingFacilities: Boolean = sys.env.get("NBP_STANDING_FACILITIES").map(_.trim.toBoolean).getOrElse(false)
  val NbpDepositFacilitySpread: Double = sys.env.get("NBP_DEPOSIT_FACILITY_SPREAD").map(_.trim.toDouble).getOrElse(0.01)
  val NbpLombardSpread: Double = sys.env.get("NBP_LOMBARD_SPREAD").map(_.trim.toDouble).getOrElse(0.01)

  // Forward guidance: NBP communicates future rate path to anchor expectations.
  // Requires EXPECTATIONS_ENABLED. Affects bond yields via ExpBondSensitivity.
  val NbpForwardGuidance: Boolean = sys.env.get("NBP_FORWARD_GUIDANCE").map(_.trim.toBoolean).getOrElse(false)

  // ───────────────────────────────────────────────────────────────────────
  // ECB — Eurozone counterfactual
  // Parameters for EUR regime: exogenous ECB rate, no independent monetary policy.
  // Simulates Poland's hypothetical Eurozone membership (Art. 4 Traktatu akcesyjnego).
  // ECB targets symmetric 2.0% HICP (Lagarde 2021 strategy review).
  // ───────────────────────────────────────────────────────────────────────
  val EcbInitialRate = 0.035 // ECB deposit facility rate (Oct 2024)
  val EcbNeutralRate = 0.025 // ECB neutral rate (lower than NBP; Holston-Laubach-Williams)
  val EcbTargetInfl = 0.020 // ECB symmetric HICP target 2.0% (2021 strategy review)
  val EcbAlpha = 1.5 // ECB Taylor inflation response (slower than Fed)
  val EcbInertia = 0.85 // ECB rate smoothing (smoother than NBP: consensus-driven GC)
  val EuroInflation = 0.020 // Exogenous Eurozone-wide HICP inflation (constant)

  // ───────────────────────────────────────────────────────────────────────
  // SGP fiscal constraint (EUR regime only)
  // Maastricht convergence criteria (Art. 126 TFEU, Protocol No. 12):
  //   - Annual general government deficit < 3% of GDP
  //   - Gross government debt < 60% of GDP
  // When debt exceeds 60%, austerity reduces BDP transfers at speed SgpAusterityRate.
  // Poland 2024: debt ~49% GDP (within limit), deficit ~5.1% GDP (EDP opened 2024).
  // ───────────────────────────────────────────────────────────────────────
  val SgpDeficitLimit = 0.03 // Maastricht: annual deficit < 3% of GDP
  val SgpDebtLimit = 0.60 // Maastricht: public debt < 60% of GDP
  val SgpAusterityRate = 2.0 // Austerity speed: BDP × max(0, 1 - (debtRatio - 0.60) × rate)

  // ───────────────────────────────────────────────────────────────────────
  // Unemployment benefits — automatic stabilizers
  // Zasiłek dla bezrobotnych (Ustawa o promocji zatrudnienia, Art. 72-73):
  // Higher rate months 1-3, lower months 4-6, max 6 months (12 in high-unemployment
  // powiat). Poland 2023: only ~15% of registered unemployed receive benefits
  // (GUS BAEL), due to strict eligibility (365 days worked in 18 months).
  // Flows to Identity 2 (deposits ↑) and Identity 3 (gov deficit ↑).
  // ───────────────────────────────────────────────────────────────────────
  val GovUnempBenefitEnabled: Boolean = sys.env.get("GOV_UNEMP_BENEFIT").map(_.trim.toBoolean).getOrElse(true)
  val GovBenefitM1to3: Double = sys.env.get("GOV_BENEFIT_M1_3").map(_.trim.toDouble).getOrElse(1500.0)
  val GovBenefitM4to6: Double = sys.env.get("GOV_BENEFIT_M4_6").map(_.trim.toDouble).getOrElse(1200.0)
  val GovBenefitDuration: Int = sys.env.get("GOV_BENEFIT_DURATION").map(_.trim.toInt).getOrElse(6)
  // Aggregate mode coverage rate: fraction of unemployed receiving benefits at any time
  // Poland 2023: ~15% of unemployed receive zasiłek (GUS BAEL); registration ~60%, eligibility ~25%
  val GovBenefitCoverage: Double = sys.env.get("GOV_BENEFIT_COVERAGE").map(_.trim.toDouble).getOrElse(0.15)

  // ───────────────────────────────────────────────────────────────────────
  // Government Bond Market
  // Treasury securities (obligacje skarbowe) issued by MF. Poland 2024:
  // outstanding ~1.6 tln PLN (MF Public Debt Strategy 2024). Yield =
  // refRate + termPremium + fiscalRiskBeta × max(0, debtRatio - threshold).
  // Holders: banks (~25%), NBP (~19%), foreign (~20%), insurance/pension (~15%),
  // households (~10%), other (~11%) (MF monthly reports).
  // ───────────────────────────────────────────────────────────────────────
  val GovBondMarket: Boolean = sys.env.get("GOV_BOND_MARKET").map(_.trim.toBoolean).getOrElse(true)
  val GovFiscalRiskBeta: Double =
    sys.env.get("GOV_FISCAL_RISK_BETA").map(_.trim.toDouble).getOrElse(2.0) // Fiscal risk premium sensitivity
  val GovTermPremium: Double =
    sys.env.get("GOV_TERM_PREMIUM").map(_.trim.toDouble).getOrElse(0.005) // 50bp term premium (10Y vs overnight)

  // ───────────────────────────────────────────────────────────────────────
  // Quantitative Easing
  // NBP Structural Open Market Operations (SOR): precedent set March 2020
  // (Uchwała Zarządu NBP nr 13/2020). NBP purchased ~140 bln PLN of
  // government bonds and PFR-guaranteed bonds by end-2021.
  // QE pace: monthly purchases (PLN, GdpRatio-scaled).
  // Max 30% of GDP in central bank bond holdings (self-imposed limit).
  // ───────────────────────────────────────────────────────────────────────
  // QE enabled by default (NBP Structural Open Market Operations, March 2020 precedent)
  val NbpQe: Boolean = sys.env.get("NBP_QE").map(_.trim.toBoolean).getOrElse(true)
  val NbpQePace: Double = sys.env.get("NBP_QE_PACE").map(_.trim.toDouble).getOrElse(5e9) * GdpRatio
  val NbpQeMaxGdpShare: Double = sys.env.get("NBP_QE_MAX_GDP_SHARE").map(_.trim.toDouble).getOrElse(0.30)

  // ───────────────────────────────────────────────────────────────────────
  // FX Intervention
  // NBP foreign exchange intervention: managed float, no explicit band.
  // Reserves ~€185 bln (NBP 2024, 5th largest in EU). Interventions rare
  // under floating regime but precedent exists (Dec 2020: NBP purchased EUR
  // to weaken PLN). Model: symmetric band around BaseExRate; if breached,
  // NBP sells/buys FX up to NbpFxMaxMonthly share of reserves.
  // ───────────────────────────────────────────────────────────────────────
  val NbpFxIntervention: Boolean = sys.env.get("NBP_FX_INTERVENTION").map(_.trim.toBoolean).getOrElse(false)
  val NbpFxBand: Double = sys.env.get("NBP_FX_BAND").map(_.trim.toDouble).getOrElse(0.10)
  val NbpFxReserves: Double =
    sys.env.get("NBP_FX_RESERVES").map(_.trim.toDouble).getOrElse(185e9) * GdpRatio // EUR-equivalent total
  val NbpFxMaxMonthly: Double = sys.env.get("NBP_FX_MAX_MONTHLY").map(_.trim.toDouble).getOrElse(0.03)
  val NbpFxStrength: Double = sys.env.get("NBP_FX_STRENGTH").map(_.trim.toDouble).getOrElse(0.5)

  // ═══════════════════════════════════════════════════════════════════════
  // BANKING SYSTEM
  // Polish banking sector: ~30 commercial banks + ~500 cooperative banks.
  // Total assets ~3.2 tln PLN (KNF 2024). Dominated by 7 largest banks
  // (PKO BP, Pekao, Santander PL, ING BSK, mBank, BNP Paribas, Millennium)
  // holding ~70% of sector assets. Capital adequacy: sector avg TCR ~18%
  // (well above 8% Basel III minimum). NPL ratio ~3% (KNF Financial
  // Stability Report 2024). All stock variables are GdpRatio-scaled.
  // Sources: NBP Financial Stability Report, KNF monthly statistics, MF.
  // Affects: Identity 1 (bank capital), Identity 2 (bank balance sheet).
  // ═══════════════════════════════════════════════════════════════════════

  // Initial balance sheet stocks (PLN, real economy; auto-scaled by GdpRatio)
  val InitBankCapital: Double = sys.env
    .get("INIT_BANK_CAPITAL")
    .map(_.trim.toDouble)
    .getOrElse(270e9) * GdpRatio // ~270 bln PLN (KNF 2024: sector equity)
  val InitBankDeposits: Double = sys.env
    .get("INIT_BANK_DEPOSITS")
    .map(_.trim.toDouble)
    .getOrElse(1900e9) * GdpRatio // ~1.9 tln PLN (NBP: M3 deposits)
  val InitBankLoans: Double = sys.env
    .get("INIT_BANK_LOANS")
    .map(_.trim.toDouble)
    .getOrElse(700e9) * GdpRatio // ~700 bln PLN (corporate loans, NBP MIR)
  val InitBankGovBonds: Double = sys.env
    .get("INIT_BANK_GOV_BONDS")
    .map(_.trim.toDouble)
    .getOrElse(400e9) * GdpRatio // ~400 bln PLN (bank holdings of TSec)
  val InitNbpGovBonds: Double = sys.env
    .get("INIT_NBP_GOV_BONDS")
    .map(_.trim.toDouble)
    .getOrElse(300e9) * GdpRatio // ~300 bln PLN (NBP SOR portfolio)
  val InitGovDebt: Double = sys.env
    .get("INIT_GOV_DEBT")
    .map(_.trim.toDouble)
    .getOrElse(1600e9) * GdpRatio // ~1.6 tln PLN (MF: State Treasury debt)
  val InitConsumerLoans: Double = sys.env
    .get("INIT_CONSUMER_LOANS")
    .map(_.trim.toDouble)
    .getOrElse(200e9) * GdpRatio // ~200 bln PLN (BIK consumer credit)
  val BaseSpread = 0.015 // Corporate loan spread over reference rate (NBP MIR 2024: ~150bp)
  val NplSpreadFactor = 5.0 // NPL multiplier on spread: high NPLs → tighter credit
  val MinCar = 0.08 // Minimum capital adequacy ratio (Basel III Pillar 1: 8%)
  val LoanRecovery = 0.30 // Recovery rate on defaulted corporate loans (30%; KNF LGD estimates)

  // ───────────────────────────────────────────────────────────────────────
  // Multi-bank mode
  // BANK_MODE=multi: 7 heterogeneous banks (modeled on Polish systemically
  // important institutions). Enables interbank market, differential capital
  // requirements, and bank failure/resolution mechanics.
  // BANK_MODE=single (default): aggregate banking sector (1 representative bank).
  // ───────────────────────────────────────────────────────────────────────
  val BankMulti: Boolean = sys.env.get("BANK_MODE").map(_.trim.toLowerCase).getOrElse("single") == "multi"
  val BankFailureEnabled: Boolean =
    sys.env.get("BANK_FAILURE").map(_.trim.toBoolean).getOrElse(false) // Bank failure & resolution
  // Required reserves: 3.5% of deposits (Uchwała Zarządu NBP nr 7/2024; was 0.5% until Oct 2023)
  val BankReserveReq: Double = sys.env.get("BANK_RESERVE_REQ").map(_.trim.toDouble).getOrElse(0.035)
  val BankStressThreshold: Double =
    sys.env.get("BANK_STRESS_THRESHOLD").map(_.trim.toDouble).getOrElse(0.05) // CAR trigger for stress state

  // Credit diagnostics: outputs M1/M2 monetary aggregates for cross-validation
  val CreditDiagnostics: Boolean = sys.env.get("CREDIT_DIAGNOSTICS").map(_.trim.toBoolean).getOrElse(false)

  // ───────────────────────────────────────────────────────────────────────
  // Consumer Credit
  // Household consumer loans: ~200 bln PLN outstanding (BIK 2024).
  // Spread over reference rate, DTI limit (Recommendation T, KNF),
  // max loan size. NPL recovery lower than corporate (15% vs 30%) due to
  // unsecured nature. Eligibility: ~30% of households actively borrow.
  // Flows to Identity 2 (bank loans ↑, deposits ↑), Identity 11.
  // ───────────────────────────────────────────────────────────────────────
  val CcSpread: Double =
    sys.env.get("CC_SPREAD").map(_.trim.toDouble).getOrElse(0.04) // 400bp over ref (NBP MIR consumer 2024)
  val CcMaxDti: Double =
    sys.env.get("CC_MAX_DTI").map(_.trim.toDouble).getOrElse(0.40) // Max debt-to-income 40% (KNF Recommendation T)
  val CcMaxLoan: Double =
    sys.env.get("CC_MAX_LOAN").map(_.trim.toDouble).getOrElse(50000.0) // Max consumer loan per HH (PLN)
  val CcAmortRate: Double = sys.env
    .get("CC_AMORT_RATE")
    .map(_.trim.toDouble)
    .getOrElse(0.025) // Monthly amortization (2.5% → ~40 month maturity)
  val CcNplRecovery: Double =
    sys.env.get("CC_NPL_RECOVERY").map(_.trim.toDouble).getOrElse(0.15) // 15% recovery (unsecured)
  val CcEligRate: Double =
    sys.env.get("CC_ELIG_RATE").map(_.trim.toDouble).getOrElse(0.30) // 30% of HH eligible for consumer credit

  // ═══════════════════════════════════════════════════════════════════════
  // LOCAL GOVERNMENT — JST / SAMORZĄDY
  // Two-tier fiscal system: central government + local government units (JST).
  // JST revenue: shared PIT (38.46%) and CIT (6.71%) per Ustawa o dochodach
  // JST (Art. 4), plus property tax, subventions (subwencja ogólna ~3% GDP),
  // and earmarked grants (dotacje celowe ~1% GDP). JST spending ~13% GDP
  // (GUS 2023), with ~2% structural deficit ratio.
  // Sources: MF (Rb-27S/28S reports), GUS BDL, RIO.
  // ═══════════════════════════════════════════════════════════════════════
  val JstEnabled: Boolean = sys.env.get("JST_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val JstPitShare: Double =
    sys.env.get("JST_PIT_SHARE").map(_.trim.toDouble).getOrElse(0.3846) // Ustawa o dochodach JST, Art. 4
  val JstCitShare: Double = sys.env.get("JST_CIT_SHARE").map(_.trim.toDouble).getOrElse(0.0671) // j.w.
  val JstPropertyTax: Double =
    sys.env.get("JST_PROPERTY_TAX").map(_.trim.toDouble).getOrElse(5000.0) // PLN/firm/year (GUS 2023)
  val JstSubventionShare: Double = sys.env.get("JST_SUBVENTION_SHARE").map(_.trim.toDouble).getOrElse(0.03) // ~3% GDP
  val JstDotacjeShare: Double = sys.env.get("JST_DOTACJE_SHARE").map(_.trim.toDouble).getOrElse(0.01) // ~1% GDP
  val JstSpendingMult: Double =
    sys.env.get("JST_SPENDING_MULT").map(_.trim.toDouble).getOrElse(1.02) // ~2% deficit ratio

  // ───────────────────────────────────────────────────────────────────────
  // Bank Liquidity — LCR/NSFR
  // Basel III liquidity standards transposed via CRR (EU 575/2013, Art. 411-428).
  // LCR (Liquidity Coverage Ratio): HQLA / 30-day net outflows ≥ 100%.
  // NSFR (Net Stable Funding Ratio): available stable funding / required ≥ 100%.
  // Polish banks: avg LCR ~170%, avg NSFR ~135% (KNF Financial Stability Report 2024).
  // Demand deposit runoff: 10% (CRR Art. 421: retail deposits, category 1).
  // ───────────────────────────────────────────────────────────────────────
  val BankLcrEnabled: Boolean = sys.env.get("BANK_LCR_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val BankLcrMin: Double = sys.env.get("BANK_LCR_MIN").map(_.trim.toDouble).getOrElse(1.0) // Basel III minimum 100%
  val BankNsfrMin: Double = sys.env.get("BANK_NSFR_MIN").map(_.trim.toDouble).getOrElse(1.0) // Basel III minimum 100%
  val BankDemandDepositRunoff: Double =
    sys.env.get("BANK_DEMAND_DEPOSIT_RUNOFF").map(_.trim.toDouble).getOrElse(0.10) // Basel III retail
  val BankTermDepositFrac: Double = sys.env.get("BANK_TERM_DEPOSIT_FRAC").map(_.trim.toDouble).getOrElse(0.40)

  // Interbank term structure: WIBOR-like term rates (O/N, 1M, 3M, 6M) with term spread
  val InterbankTermStructure: Boolean = sys.env.get("INTERBANK_TERM_STRUCTURE").map(_.trim.toBoolean).getOrElse(false)

  // ═══════════════════════════════════════════════════════════════════════
  // SOCIAL SECURITY — ZUS / PPK / DEMOGRAPHICS
  // Poland's social insurance: three-pillar system.
  //   Pillar I: ZUS (Zakład Ubezpieczeń Społecznych) — PAYG, reformed 1999
  //     (Ustawa Buzek), OFE dismantled 2014 (Ustawa Tuska). Contribution:
  //     19.52% of gross wage (employee+employer split). Base pension ~3 500 PLN
  //     after waloryzacja (ZUS 2024). Replacement rate ~40% (OECD Pensions at
  //     a Glance 2023: Poland 35.2% net, below OECD avg 62%).
  //   Pillar II: PPK (Pracownicze Plany Kapitałowe) — Ustawa o PPK 2018,
  //     mandatory enrollment (opt-out), default 2% employee + 1.5% employer.
  //     AUM ~25 bln PLN (PFR Portal PPK 2024). Bond allocation: 60% default
  //     TFI lifecycle strategy.
  //   Demographics: aging population. 2024: ~8M retirees / ~17.5M labor force
  //     ≈ 0.46 dependency ratio (GUS Prognoza ludności 2023-2060).
  // Sources: ZUS statistical yearbook, MRiPS, PFR Portal PPK, GUS demographics.
  // ═══════════════════════════════════════════════════════════════════════
  val ZusEnabled: Boolean = sys.env.get("ZUS_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  // Pension contribution rate: 19.52% (emerytalna 9.76% × 2; Art. 22 Ustawa o systemie ubezpieczeń społecznych)
  val ZusContribRate: Double = sys.env.get("ZUS_CONTRIB_RATE").map(_.trim.toDouble).getOrElse(0.1952)
  // Base monthly pension after annual waloryzacja (ZUS 2024; minimum emerytura 1 780.96 PLN)
  val ZusBasePension: Double = sys.env.get("ZUS_BASE_PENSION").map(_.trim.toDouble).getOrElse(3500.0)
  // Scale factor for gradual ZUS introduction in simulation (1.0 = full effect)
  val ZusScale: Double = sys.env.get("ZUS_SCALE").map(_.trim.toDouble).getOrElse(1.0)

  // PPK / Capital Pension (Pillar II — Ustawa o PPK, Dz.U. 2018 poz. 2215)
  val PpkEnabled: Boolean = sys.env.get("PPK_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val PpkEmployeeRate: Double =
    sys.env.get("PPK_EMPLOYEE_RATE").map(_.trim.toDouble).getOrElse(0.02) // 2% of gross (Art. 27 Ustawa o PPK)
  val PpkEmployerRate: Double =
    sys.env.get("PPK_EMPLOYER_RATE").map(_.trim.toDouble).getOrElse(0.015) // 1.5% of gross (Art. 26)
  val PpkBondAlloc: Double =
    sys.env.get("PPK_BOND_ALLOC").map(_.trim.toDouble).getOrElse(0.60) // Default TFI lifecycle: 60% bonds, 40% equities

  // Demographics (GUS Prognoza ludności 2023-2060)
  val DemEnabled: Boolean = sys.env.get("DEMOGRAPHICS_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val DemRetirementRate: Double = sys.env.get("DEM_RETIREMENT_RATE").map(_.trim.toDouble).getOrElse(0.001) // 0.1%/month
  val DemWorkingAgeDecline: Double =
    sys.env.get("DEM_WORKING_AGE_DECLINE").map(_.trim.toDouble).getOrElse(0.002) // 0.2%/year
  // Poland 2024: ~8M retirees / ~17.5M labor force ≈ 0.46 ratio.
  // Default: when ZUS enabled, use realistic ratio; otherwise 0.
  val DemInitialRetirees: Int = sys.env
    .get("DEM_INITIAL_RETIREES")
    .map(_.trim.toInt)
    .getOrElse(if ZusEnabled then (TotalPopulation * 0.46).toInt else 0)

  // ═══════════════════════════════════════════════════════════════════════
  // MACROPRUDENTIAL POLICY — KNF
  // Komisja Nadzoru Finansowego (KNF): Poland's integrated financial supervisor.
  // Macroprudential toolkit (transposed from CRD V / CRR II):
  //   - CCyB (countercyclical capital buffer): 0-2.5%, activated when credit/GDP
  //     gap exceeds threshold (ESRB methodology; KNF CCyB decision quarterly).
  //     Poland 2024: CCyB = 0% (not yet activated, but framework in place).
  //   - O-SII buffer (Other Systemically Important Institutions): KNF designates
  //     8 O-SIIs annually (SRB/EBA methodology). PKO BP 1.0%, Pekao 0.5%, etc.
  //   - Concentration limit: Art. 395 CRR (large exposure ≤ 25% of own funds).
  // Sources: KNF macroprudential toolkit documentation, ESRB country page, SRB.
  // ═══════════════════════════════════════════════════════════════════════
  val MacropruEnabled: Boolean = sys.env.get("MACROPRU_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val CcybMax: Double = sys.env.get("CCYB_MAX").map(_.trim.toDouble).getOrElse(0.025) // 2.5% max CCyB (CRD V Art. 130)
  val CcybActivationGap: Double =
    sys.env.get("CCYB_ACTIVATION_GAP").map(_.trim.toDouble).getOrElse(0.02) // Credit/GDP gap >2pp → build buffer
  val CcybReleaseGap: Double =
    sys.env.get("CCYB_RELEASE_GAP").map(_.trim.toDouble).getOrElse(-0.02) // Gap <-2pp → release buffer
  val OsiiPkoBp: Double =
    sys.env.get("OSII_PKO_BP").map(_.trim.toDouble).getOrElse(0.01) // 1.0% O-SII for PKO BP (largest bank)
  val OsiiPekao: Double =
    sys.env.get("OSII_PEKAO").map(_.trim.toDouble).getOrElse(0.005) // 0.5% O-SII for Pekao (2nd largest)
  val ConcentrationLimit: Double =
    sys.env.get("CONCENTRATION_LIMIT").map(_.trim.toDouble).getOrElse(0.25) // Art. 395 CRR: large exposure limit

  // ───────────────────────────────────────────────────────────────────────
  // KNF/BFG Detail
  // BION/SREP (Badanie i Ocena Nadzorcza): KNF's annual supervisory review,
  // assigning P2R (Pillar 2 Requirement) add-ons per bank (KNF methodology 2024).
  // BFG (Bankowy Fundusz Gwarancyjny): deposit guarantee (400 000 PLN per
  // depositor, Art. 24 Ustawa o BFG) + annual levy on banks (0.24% of covered
  // deposits). Bail-in: BRRD transposition (Ustawa o BFG, Art. 101-110).
  // Requires: BANK_MODE=multi + MACROPRU_ENABLED (P2R) + BANK_FAILURE (BFG+bail-in).
  // Affects: Identity 1 (bank capital ↓ via levies), Identity 2 (bail-in haircuts).
  // ───────────────────────────────────────────────────────────────────────
  val P2rAddons: Vector[Double] = sys.env.get("P2R_ADDONS") match
    case Some(s) =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 7, s"P2R_ADDONS must have 7 values (one per bank), got ${v.length}")
      v
    case _ => Vector(0.015, 0.010, 0.030, 0.015, 0.020, 0.025, 0.020)
  val BfgLevyRate: Double = sys.env.get("BFG_LEVY_RATE").map(_.trim.toDouble).getOrElse(0.0024)
  val BailInEnabled: Boolean = sys.env.get("BAIL_IN_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val BailInDepositHaircut: Double = sys.env.get("BAIL_IN_DEPOSIT_HAIRCUT").map(_.trim.toDouble).getOrElse(0.08)
  val BfgDepositGuarantee: Double = sys.env.get("BFG_DEPOSIT_GUARANTEE").map(_.trim.toDouble).getOrElse(400000.0)

  // ═══════════════════════════════════════════════════════════════════════
  // FOREIGN SECTOR
  // Poland's external sector: floating PLN/EUR, open capital account (EU
  // acquis). Trade openness ~100% GDP (exports + imports). Key partners:
  // Germany (~28% of exports), Czech Republic, France, Netherlands.
  // Exchange rate: managed float, no explicit band (NBP does not target ER).
  // Sources: NBP BoP, GUS foreign trade statistics, Eurostat.
  // Affects: Identity 4 (BoP / NFA).
  // ═══════════════════════════════════════════════════════════════════════
  val BaseExRate = 4.33 // NBP average PLN/EUR rate 2024
  val ForeignRate = 0.04 // ECB rate 2024
  // HH consumption import propensity: share of household spending on imported goods.
  // Poland 2024: consumer goods imports ~400 bln / HH consumption ~2000 bln ≈ 0.20-0.22.
  // Previously 0.40 (= total import/GDP ratio including intermediates + capital goods,
  // which are now handled by separate import channels: PhysCapImportShare, GreenImportShare,
  // TechImportShare, GVC intermediate imports).
  val ImportPropensity: Double = sys.env.get("IMPORT_PROPENSITY").map(_.trim.toDouble).getOrElse(0.22)
  val ExportBase = 55.4e9 * GdpRatio // ~665 bln PLN/year (GUS 2024, simplified forex model)
  val TechImportShare = 0.40 // Share of AI/tech CAPEX that is imported (fraction 0-1)
  val IrpSensitivity = 0.15 // Interest rate parity sensitivity (UIP deviation)
  val ExRateAdjSpeed = 0.02 // Monthly ER adjustment speed toward equilibrium
  val ExportAutoBoost = 0.15 // Automation → ULC reduction → export competitiveness gain

  // ═══════════════════════════════════════════════════════════════════════
  // NETWORK TOPOLOGY & DEMONSTRATION EFFECTS
  // Watts-Strogatz small-world network for inter-firm information diffusion.
  // Firms observe neighbors' automation decisions; demonstration effects
  // reduce uncertainty about AI adoption (Acemoglu & Restrepo 2018).
  // ═══════════════════════════════════════════════════════════════════════
  val NetworkK = 6 // Watts-Strogatz: neighbors
  val NetworkRewireP = 0.10 // Watts-Strogatz: rewire probability
  val DemoEffectThresh = sys.env.get("DEMO_THRESH").map(_.trim.toDouble).getOrElse(0.40)
  val DemoEffectBoost = 0.15 // Modest boost to uncertainty discount from demonstration

  // Endogenous sigma dynamics: CES elasticity evolves with adoption
  val SigmaLambda = sys.env.get("SIGMA_LAMBDA").map(_.trim.toDouble).getOrElse(0.0) // Learning rate (0 = static)
  val SigmaCapMult = sys.env.get("SIGMA_CAP_MULT").map(_.trim.toDouble).getOrElse(3.0) // Max sigma = base × mult

  // Dynamic network rewiring: firms rewire connections based on profitability
  val RewireRho = sys.env.get("REWIRE_RHO").map(_.trim.toDouble).getOrElse(0.0) // Rewire rate (0 = static)

  // ═══════════════════════════════════════════════════════════════════════
  // INPUT-OUTPUT COUPLING
  // Leontief-style inter-sector intermediate flows. The 6×6 matrix A gives
  // input coefficients a_ij = purchases from sector i per unit gross output
  // of sector j. Calibrated from GUS symmetric I-O tables (2015/2019),
  // cross-validated against WIOD (2000-2014) and OECD ICIO (2005-2020).
  // I-O transactions are zero-sum deposit transfers within the banking system:
  // buyer deposits ↓, seller deposits ↑, ΔD_total = 0. They do NOT break
  // existing SFC identities but any deviation from zero-sum would.
  // Sources: GUS "Bilans przepływów międzygałęziowych", WIOD, OECD ICIO.
  // ═══════════════════════════════════════════════════════════════════════
  val IoEnabled: Boolean = sys.env.get("IO_MODE").map(_.trim.toLowerCase) match
    case Some("enabled" | "true" | "on") => true
    case _                               => false

  val IoMatrix: Vector[Vector[Double]] = sys.env.get("IO_MATRIX") match
    case Some(s) if s.nonEmpty =>
      val rows = s.split(";").map(_.split(",").map(_.trim.toDouble).toVector).toVector
      require(rows.length == 6 && rows.forall(_.length == 6), s"IO_MATRIX must be 6x6, got ${rows.length} rows")
      rows
    // a_ij = input from sector i per unit gross output of sector j.
    // Calibrated from GUS symmetric I-O tables (2015/2019), cross-validated
    // against WIOD (2000-2014), OECD ICIO (2005-2020), Eurostat (2015/2019).
    // ISIC mapping: BPO←J62-63+N78-82, Mfg←C10-33, Ret←G-I+K-L+M69-75+R-S,
    //               Hlt←Q86-88, Pub←O84+P85, Agr←A01-03.
    case _ =>
      Vector(
        Vector(0.05, 0.03, 0.04, 0.02, 0.03, 0.01), // BPO purchases
        Vector(0.04, 0.35, 0.12, 0.15, 0.05, 0.18), // Mfg purchases
        Vector(0.15, 0.10, 0.12, 0.08, 0.07, 0.08), // Retail purchases
        Vector(0.01, 0.00, 0.01, 0.05, 0.02, 0.01), // Healthcare purchases
        Vector(0.01, 0.01, 0.01, 0.01, 0.03, 0.01), // Public purchases
        Vector(0.00, 0.08, 0.05, 0.01, 0.01, 0.12),
      ) // Agriculture purchases

  val IoColumnSums: Vector[Double] =
    (0 until 6).map(j => IoMatrix.map(_(j)).sum).toVector

  // Pass-through fraction: 1.0 = full I-O, 0.0 = no I-O.
  // Models contractual rigidity, delivery lags, and supplier substitution.
  val IoScale: Double = sys.env.get("IO_SCALE").map(_.trim.toDouble).getOrElse(1.0)

  // ═══════════════════════════════════════════════════════════════════════
  // OPEN ECONOMY — DEEP EXTERNAL SECTOR
  // Full balance-of-payments model: current account (trade, income, transfers),
  // capital account (FDI, portfolio, EU transfers), financial account.
  // Marshall-Lerner condition: sum of export + import price elasticities > 1
  // (here 0.8 + 0.6 = 1.4 → J-curve then improvement). Mundell-Fleming:
  // interest rate differential drives capital flows and ER.
  // NFA dynamics: NFA accumulation affects risk premium on ER.
  // Sources: NBP BoP (quarterly), GUS foreign trade, WIOD, Eurostat.
  // Affects: Identity 4 (BoP / NFA), Identity 2 (bank deposits via trade).
  // ═══════════════════════════════════════════════════════════════════════
  val OeEnabled: Boolean = sys.env.get("OPEN_ECON").map(_.trim.toLowerCase) match
    case Some("true" | "enabled" | "on" | "1") => true
    case _                                     => false

  val OeImportContent: Vector[Double] = sys.env.get("OE_IMPORT_CONTENT") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 6, s"OE_IMPORT_CONTENT must have 6 values, got ${v.length}")
      v
    case _ => Vector(0.15, 0.50, 0.20, 0.15, 0.05, 0.12) // BPO, Mfg, Ret, Hlt, Pub, Agr

  val OeErFloor: Double = sys.env.get("OE_ER_FLOOR").map(_.trim.toDouble).getOrElse(2.5)
  val OeErCeiling: Double = sys.env.get("OE_ER_CEILING").map(_.trim.toDouble).getOrElse(10.0)

  // Export base for open economy.  Default 475M balances initial trade when
  // intermediate imports (~200M) are added to the existing consumption imports (~275M).
  // The legacy ExportBase (190M) was calibrated without intermediate imports.
  val OeExportBase: Double = sys.env
    .get("OE_EXPORT_BASE")
    .map(_.trim.toDouble)
    .getOrElse(138.5e9) * GdpRatio // ~1.66 bln PLN/year (GUS/NBP BoP 2024)

  // Import-push inflation cap (monthly).  Prevents runaway ER→inflation→ER spiral.
  // At 3% cap, even a 100% ER deviation contributes at most 3% monthly inflation.
  val OeImportPushCap: Double = 0.03

  // Calibration (NBP/GUS/WIOD/Eurostat)
  val OeForeignGdpGrowth = 0.015 // Eurozone annual real GDP growth (ECB projection)
  val OeExportPriceElasticity = 0.8 // Marshall-Lerner: export price elasticity (Bahmani-Oskooee & Ratha 2004)
  val OeImportPriceElasticity = 0.6 // Marshall-Lerner: import price elasticity
  val OeErElasticity = 0.5 // ER pass-through to import prices (partial; Campa & Goldberg 2005)
  val OeUlcExportBoost = 0.15 // Automation → ULC reduction → export competitiveness gain
  val OeNfaReturnRate = 0.03 // Annual return on net foreign assets
  val OeEuTransfers = 1.458e9 * GdpRatio // ~17.5 bln PLN/year EU structural funds (KE/MFiPR)
  val OeFdiBase = 583.1e6 * GdpRatio // ~7 bln PLN/year baseline FDI inflows (NBP BoP)
  val OePortfolioSensitivity = 0.20 // Interest rate differential → portfolio capital flows
  val OeRiskPremiumSensitivity = 0.10 // NFA/GDP ratio → risk premium on exchange rate

  // ═══════════════════════════════════════════════════════════════════════
  // GPW EQUITY MARKET
  // Giełda Papierów Wartościowych w Warszawie: CEE's largest stock exchange.
  // WIG20 index, total market cap ~1.4 tln PLN (GPW 2024). Foreign investors
  // hold ~67% of free float (GPW annual report). P/E ratio ~10 (below
  // developed markets due to geopolitical discount). Dividend yield ~5.7%.
  // Channels: wealth effects on consumption, equity issuance for firms,
  // HH portfolio allocation, dividend income.
  // Sources: GPW annual statistics, KNF, NBP Financial Stability Report.
  // Affects: Identity 5 (equity valuation / household wealth).
  // ═══════════════════════════════════════════════════════════════════════
  val GpwEnabled: Boolean = sys.env.get("GPW_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val GpwInitIndex: Double = sys.env.get("GPW_INIT_INDEX").map(_.trim.toDouble).getOrElse(2400.0)
  val GpwInitMcap: Double = sys.env.get("GPW_INIT_MCAP").map(_.trim.toDouble).getOrElse(1.4e12) * GdpRatio
  val GpwPeMean: Double = sys.env.get("GPW_PE_MEAN").map(_.trim.toDouble).getOrElse(10.0)
  val GpwDivYield: Double = sys.env.get("GPW_DIV_YIELD").map(_.trim.toDouble).getOrElse(0.057)
  val GpwForeignShare: Double = sys.env.get("GPW_FOREIGN_SHARE").map(_.trim.toDouble).getOrElse(0.67)

  // GPW Firm Equity Issuance
  val GpwEquityIssuance: Boolean = sys.env.get("GPW_EQUITY_ISSUANCE").map(_.trim.toBoolean).getOrElse(false)
  val GpwIssuanceFrac: Double = sys.env.get("GPW_ISSUANCE_FRAC").map(_.trim.toDouble).getOrElse(0.10)
  val GpwIssuanceMinSize: Int = sys.env.get("GPW_ISSUANCE_MIN_SIZE").map(_.trim.toInt).getOrElse(5)

  // GPW Household Equity Portfolio
  val GpwHhEquity: Boolean = sys.env.get("GPW_HH_EQUITY").map(_.trim.toBoolean).getOrElse(false)
  val GpwHhEquityFrac: Double = sys.env.get("GPW_HH_EQUITY_FRAC").map(_.trim.toDouble).getOrElse(0.07)
  val GpwWealthEffectMpc: Double = sys.env.get("GPW_WEALTH_EFFECT_MPC").map(_.trim.toDouble).getOrElse(0.02)

  // GPW Dividends
  val GpwDividends: Boolean = sys.env.get("GPW_DIVIDENDS").map(_.trim.toBoolean).getOrElse(false)
  val GpwDivTax: Double = sys.env.get("GPW_DIV_TAX").map(_.trim.toDouble).getOrElse(0.19)

  // ═══════════════════════════════════════════════════════════════════════
  // CORPORATE BONDS / CATALYST — always-on
  // Catalyst (GPW's bond market): ~90 bln PLN outstanding corporate bonds
  // (public + non-public, KNF 2024). Spread over gov bond yield reflects
  // credit risk (Polish BBB/BBB+ average ~250bp, RRRF 2024). Demand-side
  // absorption constraint: banks (30%) + PPK (15%) + insurance + NBFI.
  // When bank CAR is tight, absorption drops → unsold bonds revert to loans.
  // Affects: Identity 12 (corporate bond stock).
  // ═══════════════════════════════════════════════════════════════════════
  val CorpBondSpread: Double = sys.env.get("CORPBOND_SPREAD").map(_.trim.toDouble).getOrElse(0.025)
  // 250 bps over gov bond yield (Polish BBB/BBB+ avg, RRRF 2024)
  val CorpBondInitStock: Double = sys.env.get("CORPBOND_INIT_STOCK").map(_.trim.toDouble).getOrElse(90e9) * GdpRatio
  // ~90 bln PLN outstanding (Catalyst + non-public, KNF 2024)
  val CorpBondMinSize: Int = sys.env.get("CORPBOND_MIN_SIZE").map(_.trim.toInt).getOrElse(50)
  // Only firms with ≥50 workers can issue bonds (medium/large)
  val CorpBondIssuanceFrac: Double = sys.env.get("CORPBOND_ISSUANCE_FRAC").map(_.trim.toDouble).getOrElse(0.15)
  // 15% of new CAPEX loan replaced by bond issuance (eligible firms)
  val CorpBondBankShare: Double = sys.env.get("CORPBOND_BANK_SHARE").map(_.trim.toDouble).getOrElse(0.30)
  // Banks hold 30% of outstanding corporate bonds
  val CorpBondPpkShare: Double = sys.env.get("CORPBOND_PPK_SHARE").map(_.trim.toDouble).getOrElse(0.15)
  // PPK holds 15% of outstanding corporate bonds
  val CorpBondRecovery: Double = sys.env.get("CORPBOND_RECOVERY").map(_.trim.toDouble).getOrElse(0.30)
  // 30% recovery rate on defaulted corporate bonds (lower than bank loans 50%)
  val CorpBondMaturity: Double = sys.env.get("CORPBOND_MATURITY").map(_.trim.toDouble).getOrElse(60.0)
  // Average maturity 5 years = 60 months → amortization = 1/60 per month

  // ═══════════════════════════════════════════════════════════════════════
  // INSURANCE SECTOR
  // Polish insurance market: ~200 bln PLN total reserves (KNF Insurance
  // Market Report 2024). Life insurance (~110 bln reserves) + non-life
  // (~90 bln). Three-asset allocation: gov bonds (35%), corporate bonds
  // (8%), equities (12%), remainder in deposits/other. Non-life claims
  // are counter-cyclical (unemployment → more motor/property claims).
  // Sources: KNF quarterly insurance statistics, PIU (Polska Izba
  // Ubezpieczeń) annual report, EIOPA.
  // Affects: Identity 2 (bank deposits ↑↓), Identity 5 (equity holdings).
  // ═══════════════════════════════════════════════════════════════════════
  val InsEnabled: Boolean = sys.env.get("INSURANCE_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val InsLifeReserves: Double = sys.env.get("INS_LIFE_RESERVES").map(_.trim.toDouble).getOrElse(110e9) * GdpRatio
  val InsNonLifeReserves: Double = sys.env.get("INS_NONLIFE_RESERVES").map(_.trim.toDouble).getOrElse(90e9) * GdpRatio
  val InsGovBondShare: Double = sys.env.get("INS_GOVBOND_SHARE").map(_.trim.toDouble).getOrElse(0.35)
  val InsCorpBondShare: Double = sys.env.get("INS_CORPBOND_SHARE").map(_.trim.toDouble).getOrElse(0.08)
  val InsEquityShare: Double = sys.env.get("INS_EQUITY_SHARE").map(_.trim.toDouble).getOrElse(0.12)
  val InsLifePremiumRate: Double = sys.env.get("INS_LIFE_PREMIUM_RATE").map(_.trim.toDouble).getOrElse(0.003)
  val InsNonLifePremiumRate: Double = sys.env.get("INS_NONLIFE_PREMIUM_RATE").map(_.trim.toDouble).getOrElse(0.0025)
  val InsLifeLossRatio: Double = sys.env.get("INS_LIFE_LOSS_RATIO").map(_.trim.toDouble).getOrElse(0.85)
  val InsNonLifeLossRatio: Double = sys.env.get("INS_NONLIFE_LOSS_RATIO").map(_.trim.toDouble).getOrElse(0.70)
  val InsNonLifeUnempSens: Double = sys.env.get("INS_NONLIFE_UNEMP_SENS").map(_.trim.toDouble).getOrElse(0.5)
  val InsRebalanceSpeed: Double = sys.env.get("INS_REBALANCE_SPEED").map(_.trim.toDouble).getOrElse(0.05)

  // ═══════════════════════════════════════════════════════════════════════
  // SHADOW BANKING / NBFI
  // Non-bank financial intermediation in Poland: TFI (Towarzystwa Funduszy
  // Inwestycyjnych) with ~380 bln PLN AUM (IZFiA 2024), plus NBFI credit
  // (~231 bln PLN: SKOK, pożyczki pozabankowe, leasing). TFI funds allocate
  // across gov bonds (40%), corp bonds (10%), equities (10%), deposits (40%).
  // NBFI credit is counter-cyclical: expands when bank lending tightens.
  // Deposit drain: TFI inflows reduce bank deposits (financial disintermediation).
  // Sources: IZFiA monthly statistics, KNF shadow banking report, NBP FSR.
  // Affects: Identity 2 (deposit drain), Identity 5 (equity), Identity 13 (NBFI credit).
  // ═══════════════════════════════════════════════════════════════════════
  val NbfiEnabled: Boolean = sys.env.get("NBFI_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val NbfiTfiInitAum: Double = sys.env.get("NBFI_TFI_INIT_AUM").map(_.trim.toDouble).getOrElse(380e9) * GdpRatio
  val NbfiTfiGovBondShare: Double = sys.env.get("NBFI_TFI_GOVBOND_SHARE").map(_.trim.toDouble).getOrElse(0.40)
  val NbfiTfiCorpBondShare: Double = sys.env.get("NBFI_TFI_CORPBOND_SHARE").map(_.trim.toDouble).getOrElse(0.10)
  val NbfiTfiEquityShare: Double = sys.env.get("NBFI_TFI_EQUITY_SHARE").map(_.trim.toDouble).getOrElse(0.10)
  val NbfiTfiInflowRate: Double = sys.env.get("NBFI_TFI_INFLOW_RATE").map(_.trim.toDouble).getOrElse(0.001)
  val NbfiTfiRebalanceSpeed: Double = sys.env.get("NBFI_TFI_REBALANCE_SPEED").map(_.trim.toDouble).getOrElse(0.05)
  val NbfiCreditInitStock: Double =
    sys.env.get("NBFI_CREDIT_INIT_STOCK").map(_.trim.toDouble).getOrElse(231e9) * GdpRatio
  val NbfiCreditBaseRate: Double = sys.env.get("NBFI_CREDIT_BASE_RATE").map(_.trim.toDouble).getOrElse(0.005)
  val NbfiCreditRate: Double = sys.env.get("NBFI_CREDIT_RATE").map(_.trim.toDouble).getOrElse(0.10)
  val NbfiCountercyclical: Double = sys.env.get("NBFI_COUNTERCYCLICAL").map(_.trim.toDouble).getOrElse(2.0)
  val NbfiCreditMaturity: Double = sys.env.get("NBFI_CREDIT_MATURITY").map(_.trim.toDouble).getOrElse(36.0)
  val NbfiDefaultBase: Double = sys.env.get("NBFI_DEFAULT_BASE").map(_.trim.toDouble).getOrElse(0.002)
  val NbfiDefaultUnempSens: Double = sys.env.get("NBFI_DEFAULT_UNEMP_SENS").map(_.trim.toDouble).getOrElse(3.0)

  // ═══════════════════════════════════════════════════════════════════════
  // FDI COMPOSITION
  // Foreign direct investment in Poland: ~40% of corporate sector by assets
  // (NBP FDI statistics 2024). Two-channel outflow: (1) profit shifting via
  // transfer pricing to low-tax jurisdictions (Tørsløv, Wier & Zucman 2023),
  // (2) dividend repatriation to parent companies. Per-sector foreign ownership
  // shares from GUS/NBP 2024 enterprise data. M&A: probabilistic conversion of
  // domestic firms to foreign-owned.
  // Sources: NBP "Zagraniczne inwestycje bezpośrednie w Polsce", GUS REGON.
  // Affects: Identity 4 (BoP primary income debit).
  // ═══════════════════════════════════════════════════════════════════════
  val FdiEnabled: Boolean = sys.env.get("FDI_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  // Per-sector foreign ownership shares (GUS/NBP 2024): BPO 15%, Mfg 30% (auto/pharma),
  // Retail 10%, Healthcare 3%, Public 0%, Agriculture 5%
  val FdiForeignShares: Vector[Double] = sys.env.get("FDI_FOREIGN_SHARES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 6, s"FDI_FOREIGN_SHARES must have 6 values, got ${v.length}")
      v
    case _ => Vector(0.15, 0.30, 0.10, 0.03, 0.00, 0.05)
  // Profit shifting rate: 15% of foreign-owned firm profits shifted to low-tax jurisdictions
  // (Tørsløv et al. 2023 estimate for CEE; conservative — some estimates higher)
  val FdiProfitShiftRate: Double = sys.env.get("FDI_PROFIT_SHIFT_RATE").map(_.trim.toDouble).getOrElse(0.15)
  // Dividend repatriation: 70% of after-tax profits repatriated (NBP BoP 2024 primary income)
  val FdiRepatriationRate: Double = sys.env.get("FDI_REPATRIATION_RATE").map(_.trim.toDouble).getOrElse(0.70)
  // Monthly probability of M&A (foreign acquisition of domestic firm)
  val FdiMaProb: Double = sys.env.get("FDI_MA_PROB").map(_.trim.toDouble).getOrElse(0.001)
  // Minimum firm size for M&A target (workers; only medium/large firms attractive)
  val FdiMaSizeMin: Int = sys.env.get("FDI_MA_SIZE_MIN").map(_.trim.toInt).getOrElse(50)

  // ═══════════════════════════════════════════════════════════════════════
  // HOUSING MARKET & MORTGAGES
  // Polish residential real estate: total value ~3 tln PLN (NBP housing
  // market survey 2024). Mortgage stock ~485 bln PLN (BIK/ZBP 2024),
  // predominantly variable-rate (WIBOR-linked). KNF Recommendation S:
  // LTV max 80% (90% with insurance), stress-test at +2.5pp.
  // HPI dynamics: income-elastic (1.2), rate-elastic (-0.8), mean-reverting.
  // Regional variant: 7 macro-regions (Warszawa, Kraków, Wrocław, Trójmiasto,
  // Poznań, Łódź, remaining) with differentiated HPI and income multipliers.
  // Sources: NBP housing market survey, BIK/ZBP, KNF Recommendation S,
  // GUS residential construction statistics.
  // ═══════════════════════════════════════════════════════════════════════
  val ReEnabled: Boolean = sys.env.get("RE_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val ReMortgage: Boolean = sys.env.get("RE_MORTGAGE").map(_.trim.toBoolean).getOrElse(true)
  val ReHhHousing: Boolean = sys.env.get("RE_HH_HOUSING").map(_.trim.toBoolean).getOrElse(true)
  val ReInitHpi: Double = sys.env.get("RE_INIT_HPI").map(_.trim.toDouble).getOrElse(100.0)
  val ReInitValue: Double = sys.env.get("RE_INIT_VALUE").map(_.trim.toDouble).getOrElse(3.0e12) * GdpRatio
  val ReInitMortgage: Double = sys.env.get("RE_INIT_MORTGAGE").map(_.trim.toDouble).getOrElse(485e9) * GdpRatio
  val RePriceIncomeElast: Double = sys.env.get("RE_PRICE_INCOME_ELAST").map(_.trim.toDouble).getOrElse(1.2)
  val RePriceRateElast: Double = sys.env.get("RE_PRICE_RATE_ELAST").map(_.trim.toDouble).getOrElse(-0.8)
  val RePriceReversion: Double = sys.env.get("RE_PRICE_REVERSION").map(_.trim.toDouble).getOrElse(0.05)
  val ReMortgageSpread: Double = sys.env.get("RE_MORTGAGE_SPREAD").map(_.trim.toDouble).getOrElse(0.025)
  val ReMortgageMaturity: Int = sys.env.get("RE_MORTGAGE_MATURITY").map(_.trim.toInt).getOrElse(300)
  val ReLtvMax: Double =
    sys.env.get("RE_LTV_MAX").map(_.trim.toDouble).getOrElse(0.80) // KNF Recommendation S: 80% (90% with insurance)
  val ReOriginationRate: Double = sys.env.get("RE_ORIGINATION_RATE").map(_.trim.toDouble).getOrElse(0.003)
  val ReDefaultBase: Double = sys.env.get("RE_DEFAULT_BASE").map(_.trim.toDouble).getOrElse(0.001)
  val ReDefaultUnempSens: Double = sys.env.get("RE_DEFAULT_UNEMP_SENS").map(_.trim.toDouble).getOrElse(0.05)
  val ReMortgageRecovery: Double = sys.env.get("RE_MORTGAGE_RECOVERY").map(_.trim.toDouble).getOrElse(0.70)
  val ReWealthMpc: Double = sys.env.get("RE_WEALTH_MPC").map(_.trim.toDouble).getOrElse(0.05)
  val ReRentalYield: Double = sys.env.get("RE_RENTAL_YIELD").map(_.trim.toDouble).getOrElse(0.045)

  // Regional Housing Market
  val ReRegional: Boolean = sys.env.get("RE_REGIONAL").map(_.trim.toBoolean).getOrElse(false)
  val ReRegionalHpi: Vector[Double] = sys.env.get("RE_REGIONAL_HPI") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 7, s"RE_REGIONAL_HPI must have 7 values, got ${v.length}")
      v
    case _ => Vector(230.0, 190.0, 170.0, 175.0, 110.0, 140.0, 100.0)
  val ReRegionalValueShares: Vector[Double] = sys.env.get("RE_REGIONAL_VALUE_SHARES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 7, s"RE_REGIONAL_VALUE_SHARES must have 7 values, got ${v.length}")
      require(Math.abs(v.sum - 1.0) < 0.01, s"RE_REGIONAL_VALUE_SHARES must sum to ~1.0, got ${v.sum}")
      v
    case _ => Vector(0.25, 0.08, 0.07, 0.08, 0.04, 0.05, 0.43)
  val ReRegionalMortgageShares: Vector[Double] = sys.env.get("RE_REGIONAL_MORTGAGE_SHARES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 7, s"RE_REGIONAL_MORTGAGE_SHARES must have 7 values, got ${v.length}")
      require(Math.abs(v.sum - 1.0) < 0.01, s"RE_REGIONAL_MORTGAGE_SHARES must sum to ~1.0, got ${v.sum}")
      v
    case _ => Vector(0.30, 0.10, 0.08, 0.09, 0.04, 0.06, 0.33)
  val ReRegionalGammas: Vector[Double] = sys.env.get("RE_REGIONAL_GAMMAS") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 7, s"RE_REGIONAL_GAMMAS must have 7 values, got ${v.length}")
      v
    case _ => Vector(0.03, 0.04, 0.04, 0.04, 0.06, 0.05, 0.06)
  val ReRegionalIncomeMult: Vector[Double] = sys.env.get("RE_REGIONAL_INCOME_MULT") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 7, s"RE_REGIONAL_INCOME_MULT must have 7 values, got ${v.length}")
      v
    case _ => Vector(1.35, 1.15, 1.10, 1.12, 0.95, 1.05, 0.82)

  // ═══════════════════════════════════════════════════════════════════════
  // GVC / DEEP EXTERNAL SECTOR
  // Poland's position in global value chains: strong backward participation
  // (high import content of exports, especially Manufacturing at 75%).
  // GVC depth measures fraction of gross exports that is foreign value added
  // (OECD TiVA 2021: Poland ~30% overall). EU trade share ~70% (Eurostat).
  // ER pass-through: differentiated for EU (15%, EUR-invoiced) vs non-EU (60%).
  // Supply chain disruption and demand shock capability for scenario analysis.
  // Sources: OECD TiVA, WIOD (2000-2014), Eurostat Comext, NBP BoP.
  // ═══════════════════════════════════════════════════════════════════════
  val GvcEnabled: Boolean = sys.env.get("GVC_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val GvcEuTradeShare: Double = sys.env.get("GVC_EU_TRADE_SHARE").map(_.trim.toDouble).getOrElse(0.70)
  val GvcExportShares: Vector[Double] = sys.env.get("GVC_EXPORT_SHARES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 6, s"GVC_EXPORT_SHARES must have 6 values, got ${v.length}")
      v
    case _ => Vector(0.05, 0.55, 0.15, 0.03, 0.02, 0.20)
  val GvcDepth: Vector[Double] = sys.env.get("GVC_DEPTH") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 6, s"GVC_DEPTH must have 6 values, got ${v.length}")
      v
    case _ => Vector(0.35, 0.75, 0.30, 0.40, 0.10, 0.45)
  val GvcForeignInflation: Double = sys.env.get("GVC_FOREIGN_INFLATION").map(_.trim.toDouble).getOrElse(0.02)
  val GvcForeignGdpGrowth: Double = sys.env.get("GVC_FOREIGN_GDP_GROWTH").map(_.trim.toDouble).getOrElse(0.015)
  val GvcErPassthrough: Double = sys.env.get("GVC_ER_PASSTHROUGH").map(_.trim.toDouble).getOrElse(0.60)
  val GvcEuErPassthrough: Double = sys.env.get("GVC_EU_ER_PASSTHROUGH").map(_.trim.toDouble).getOrElse(0.15)
  val GvcDemandShockMonth: Int = sys.env.get("GVC_DEMAND_SHOCK_MONTH").map(_.trim.toInt).getOrElse(0)
  val GvcDemandShockSize: Double = sys.env.get("GVC_DEMAND_SHOCK_SIZE").map(_.trim.toDouble).getOrElse(0.0)
  val GvcDemandShockSectors: Set[Int] = sys.env.get("GVC_DEMAND_SHOCK_SECTORS") match
    case Some(s) if s.nonEmpty => s.split(",").map(_.trim.toInt).toSet
    case _                     => Set.empty
  val GvcDisruptionRecovery: Double = sys.env.get("GVC_DISRUPTION_RECOVERY").map(_.trim.toDouble).getOrElse(0.05)

  // ═══════════════════════════════════════════════════════════════════════
  // SECTORAL LABOR MOBILITY
  // Workers can transition between sectors with friction costs and duration
  // penalties. Friction matrix calibrated to GUS BAEL transition data
  // (quarterly labour force survey, year-on-year sector switches).
  // Voluntary search: employed workers search when wage < threshold × sector avg.
  // Vacancy-weighted matching: workers drawn to sectors with more openings.
  // Mincer (1974) returns affect retraining success probability.
  // Sources: GUS BAEL (quarterly transitions), Eurostat LFS, OECD Employment Outlook.
  // ═══════════════════════════════════════════════════════════════════════
  val LmSectoralMobility: Boolean = sys.env.get("LM_SECTORAL_MOBILITY").map(_.trim.toBoolean).getOrElse(false)
  val LmFrictionMatrix: Vector[Vector[Double]] = sys.env.get("LM_TRANSITION_MATRIX") match
    case Some(s) if s.nonEmpty =>
      val rows = s.split(";").map(_.split(",").map(_.trim.toDouble).toVector).toVector
      require(
        rows.length == 6 && rows.forall(_.length == 6),
        s"LM_TRANSITION_MATRIX must be 6x6, got ${rows.length} rows",
      )
      rows
    case _ => sfc.engine.SectoralMobility.DefaultFrictionMatrix
  val LmFrictionDurationMult: Double = sys.env.get("LM_FRICTION_DURATION_MULT").map(_.trim.toDouble).getOrElse(1.0)
  val LmFrictionCostMult: Double = sys.env.get("LM_FRICTION_COST_MULT").map(_.trim.toDouble).getOrElse(0.5)
  val LmVoluntarySearchProb: Double = sys.env.get("LM_VOLUNTARY_SEARCH_PROB").map(_.trim.toDouble).getOrElse(0.02)
  val LmVoluntaryWageThreshold: Double = sys.env.get("LM_VOLUNTARY_WAGE_THRESHOLD").map(_.trim.toDouble).getOrElse(0.20)
  val LmVacancyWeight: Double = sys.env.get("LM_VACANCY_WEIGHT").map(_.trim.toDouble).getOrElse(2.0)
  val LmAdjacentFrictionMax: Double = sys.env.get("LM_ADJACENT_FRICTION_MAX").map(_.trim.toDouble).getOrElse(0.4)

  // ═══════════════════════════════════════════════════════════════════════
  // LABOR UNIONS
  // Polish unionization: ~12% overall (OECD 2023), but highly sector-specific.
  // Public sector ~30% (Solidarność, ZNP), Manufacturing ~15%, Retail ~3%.
  // Unions provide: wage premium (~8% over non-union, controlling for sector),
  // downward wage rigidity (50% of wage cuts blocked in unionized firms).
  // Sources: GUS "Związki zawodowe w Polsce" 2022, OECD/AIAS ICTWSS database.
  // ═══════════════════════════════════════════════════════════════════════
  val UnionEnabled: Boolean = sys.env.get("UNION_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val UnionDensity: Vector[Double] = sys.env.get("UNION_DENSITY") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 6, s"UNION_DENSITY must have 6 values, got ${v.length}")
      v
    case _ => Vector(0.02, 0.15, 0.03, 0.12, 0.30, 0.04)
  val UnionWagePremium: Double = sys.env.get("UNION_WAGE_PREMIUM").map(_.trim.toDouble).getOrElse(0.08)
  val UnionRigidity: Double = sys.env.get("UNION_RIGIDITY").map(_.trim.toDouble).getOrElse(0.50)

  // ═══════════════════════════════════════════════════════════════════════
  // FORWARD-LOOKING EXPECTATIONS
  // Hybrid expectations à la Branch & Evans (2006): weighted average of
  // backward-looking (adaptive) and forward-looking (model-consistent)
  // components. Central bank credibility evolves endogenously: rises when
  // inflation stays within target band, falls when it deviates.
  // Channels: wage-setting (Phillips curve augmented with expectations),
  // bond yields (Fisher equation with expected inflation).
  // ═══════════════════════════════════════════════════════════════════════
  val ExpEnabled: Boolean = sys.env.get("EXPECTATIONS_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val ExpLambda: Double = sys.env.get("EXPECTATIONS_LAMBDA").map(_.trim.toDouble).getOrElse(0.70)
  val ExpCredibilityInit: Double = sys.env.get("EXPECTATIONS_CREDIBILITY_INIT").map(_.trim.toDouble).getOrElse(0.80)
  val ExpCredibilitySpeed: Double = sys.env.get("EXPECTATIONS_CREDIBILITY_SPEED").map(_.trim.toDouble).getOrElse(0.05)
  val ExpCredibilityThreshold: Double =
    sys.env.get("EXPECTATIONS_CREDIBILITY_THRESHOLD").map(_.trim.toDouble).getOrElse(0.02)
  val ExpWagePassthrough: Double = sys.env.get("EXPECTATIONS_WAGE_PASSTHROUGH").map(_.trim.toDouble).getOrElse(0.50)
  val ExpBondSensitivity: Double = sys.env.get("EXPECTATIONS_BOND_SENSITIVITY").map(_.trim.toDouble).getOrElse(0.50)

  // ═══════════════════════════════════════════════════════════════════════
  // IMMIGRATION
  // Poland experienced massive immigration post-2015 (Ukrainian workers).
  // Stock: ~1-2M foreign workers (GUS/ZUS 2024). Endogenous mode: inflows
  // respond to wage differential between Poland and origin countries.
  // Immigrants have sector-specific distribution (Mfg 35%, Retail 25%, Agri 25%),
  // lower initial skills (mean 0.45), and wage discount (20%).
  // Remittance outflows: 15% of immigrant wages sent to origin countries.
  // Sources: GUS "Imigranci w Polsce", ZUS foreign workers registry, NBP BoP.
  // ═══════════════════════════════════════════════════════════════════════
  val ImmigEnabled: Boolean = sys.env.get("IMMIG_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val ImmigMonthlyRate: Double = sys.env.get("IMMIG_MONTHLY_RATE").map(_.trim.toDouble).getOrElse(0.001)
  val ImmigEndogenous: Boolean = sys.env.get("IMMIG_ENDOGENOUS").map(_.trim.toBoolean).getOrElse(false)
  val ImmigWageElasticity: Double = sys.env.get("IMMIG_WAGE_ELASTICITY").map(_.trim.toDouble).getOrElse(2.0)
  val ImmigForeignWage: Double = sys.env.get("IMMIG_FOREIGN_WAGE").map(_.trim.toDouble).getOrElse(4000.0)
  val ImmigRemittanceRate: Double = sys.env.get("IMMIG_REMITTANCE_RATE").map(_.trim.toDouble).getOrElse(0.15)
  val ImmigReturnRate: Double = sys.env.get("IMMIG_RETURN_RATE").map(_.trim.toDouble).getOrElse(0.005)
  val ImmigSectorShares: Vector[Double] = sys.env.get("IMMIG_SECTOR_SHARES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 6, s"IMMIG_SECTOR_SHARES must have 6 values, got ${v.length}")
      v
    case _ => Vector(0.05, 0.35, 0.25, 0.05, 0.05, 0.25)
  val ImmigSkillMean: Double = sys.env.get("IMMIG_SKILL_MEAN").map(_.trim.toDouble).getOrElse(0.45)
  val ImmigWageDiscount: Double = sys.env.get("IMMIG_WAGE_DISCOUNT").map(_.trim.toDouble).getOrElse(0.20)
  val ImmigInitStock: Int = sys.env.get("IMMIG_INIT_STOCK").map(_.trim.toInt).getOrElse(0)

  // ───────────────────────────────────────────────────────────────────────
  // Progressive PIT
  // Personal income tax: two brackets (Art. 27 Ustawa o PIT).
  // 12% up to 120 000 PLN/year, 32% above. Tax credit (kwota wolna):
  // 30 000 PLN/year → effective credit ~3 600 PLN. Effective average rate
  // ~9% (much lower than statutory due to kwota wolna + ZUS deductions).
  // Sources: MF PIT statistics, KAS (Krajowa Administracja Skarbowa).
  // Affects: Identity 3 (government revenue).
  // ───────────────────────────────────────────────────────────────────────
  val PitEnabled: Boolean = sys.env.get("PIT_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val PitRate1: Double = sys.env.get("PIT_RATE_1").map(_.trim.toDouble).getOrElse(0.12)
  val PitRate2: Double = sys.env.get("PIT_RATE_2").map(_.trim.toDouble).getOrElse(0.32)
  val PitBracket1Annual: Double = sys.env.get("PIT_BRACKET_1").map(_.trim.toDouble).getOrElse(120000.0)
  val PitTaxCreditAnnual: Double = sys.env.get("PIT_TAX_CREDIT").map(_.trim.toDouble).getOrElse(3600.0)
  val PitEffectiveRate: Double = sys.env.get("PIT_EFFECTIVE_RATE").map(_.trim.toDouble).getOrElse(0.09)

  // ───────────────────────────────────────────────────────────────────────
  // Social Transfers: 800+ child benefit
  // Świadczenie wychowawcze "800+" (formerly "500+"): universal child benefit,
  // 800 PLN/month per child regardless of income (Ustawa o pomocy państwa
  // w wychowywaniu dzieci, increased from 500 to 800 PLN in 2024).
  // Cost: ~60 bln PLN/year (~1.7% GDP). ~6.5M children eligible.
  // ChildrenPerHh ≈ 0.35: average children per household receiving benefit.
  // Sources: MRiPS, ZUS (disbursement since 2022), GUS demographics.
  // Affects: Identity 3 (gov expenditure ↑).
  // ───────────────────────────────────────────────────────────────────────
  val Social800Enabled: Boolean = sys.env.get("SOCIAL_800_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val Social800Rate: Double = sys.env.get("SOCIAL_800_RATE").map(_.trim.toDouble).getOrElse(800.0)
  val Social800ChildrenPerHh: Double = sys.env.get("SOCIAL_800_CHILDREN").map(_.trim.toDouble).getOrElse(0.35)
  val Social800ImmigrantEligible: Boolean = sys.env.get("SOCIAL_800_IMMIGRANT").map(_.trim.toBoolean).getOrElse(true)

  // ═══════════════════════════════════════════════════════════════════════
  // EDUCATION / HUMAN CAPITAL — always-on, no master toggle
  // Four education levels: Primary (podstawowe), Vocational (zasadnicze
  // zawodowe), Secondary (średnie), Tertiary (wyższe). Distribution from
  // GUS LFS 2024 (population 25-64). Wage premia follow Mincer (1974)
  // returns to education, calibrated to GUS wage data by education level.
  // Retraining success depends on education level (higher education →
  // faster skill acquisition; Heckman, Lochner & Todd 2006).
  // Sources: GUS LFS (BAEL), OECD Education at a Glance 2024 (Poland),
  // NBP 2023 immigrant education survey.
  // ═══════════════════════════════════════════════════════════════════════
  val EduShares: Vector[Double] = sys.env.get("EDU_SHARES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(
        v.length == 4 && Math.abs(v.sum - 1.0) < 0.01,
        s"EDU_SHARES must have 4 values summing to ~1.0, got ${v.length} summing to ${v.sum}",
      )
      v
    case _ => Vector(0.08, 0.25, 0.30, 0.37) // GUS LFS 2024 (25-64): Primary, Vocational, Secondary, Tertiary
  val EduSectorShares: Option[Vector[Vector[Double]]] = sys.env.get("EDU_SECTOR_SHARES") match
    case Some(s) if s.nonEmpty =>
      val vals = s.split(",").map(_.trim.toDouble).toVector
      require(
        vals.length == 24,
        s"EDU_SECTOR_SHARES must have 24 values (6 sectors × 4 edu levels), got ${vals.length}",
      )
      val rows = (0 until 6).map(i => vals.slice(i * 4, i * 4 + 4)).toVector
      rows.zipWithIndex.foreach { (row, i) =>
        require(Math.abs(row.sum - 1.0) < 0.01, s"EDU_SECTOR_SHARES row $i must sum to ~1.0, got ${row.sum}")
      }
      Some(rows)
    case _ => None
  val EduWagePreemia: Vector[Double] = sys.env.get("EDU_WAGE_PREMIA") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 4, s"EDU_WAGE_PREMIA must have 4 values, got ${v.length}")
      v
    case _ => Vector(0.70, 0.85, 1.00, 1.30) // Mincer returns (GUS 2022)
  val EduRetrainMult: Vector[Double] = sys.env.get("EDU_RETRAIN_MULT") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 4, s"EDU_RETRAIN_MULT must have 4 values, got ${v.length}")
      v
    case _ => Vector(0.67, 0.83, 1.00, 1.25)
  val EduSkillFloors: Vector[Double] = sys.env.get("EDU_SKILL_FLOORS") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 4, s"EDU_SKILL_FLOORS must have 4 values, got ${v.length}")
      v
    case _ => Vector(0.30, 0.35, 0.45, 0.55)
  val EduSkillCeilings: Vector[Double] = sys.env.get("EDU_SKILL_CEILINGS") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 4, s"EDU_SKILL_CEILINGS must have 4 values, got ${v.length}")
      v
    case _ => Vector(0.75, 0.85, 0.95, 1.00)
  val EduImmigShares: Vector[Double] = sys.env.get("EDU_IMMIG_SHARES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(
        v.length == 4 && Math.abs(v.sum - 1.0) < 0.01,
        s"EDU_IMMIG_SHARES must have 4 values summing to ~1.0, got ${v.length} summing to ${v.sum}",
      )
      v
    case _ => Vector(0.15, 0.40, 0.35, 0.10) // NBP 2023: immigrant edu distribution

  // Default sector-education distribution (GUS LFS 2024)
  private val DefaultEduSectorShares: Vector[Vector[Double]] = Vector(
    Vector(0.02, 0.10, 0.28, 0.60), // BPO
    Vector(0.08, 0.40, 0.32, 0.20), // Manufacturing
    Vector(0.06, 0.22, 0.38, 0.34), // Retail
    Vector(0.02, 0.15, 0.23, 0.60), // Healthcare
    Vector(0.03, 0.08, 0.25, 0.64), // Public
    Vector(0.15, 0.45, 0.30, 0.10), // Agriculture
  )

  /** Draw education level (0=Primary, 1=Vocational, 2=Secondary, 3=Tertiary) for a sector. */
  def drawEducation(sectorIdx: Int, rng: scala.util.Random): Int =
    val shares = EduSectorShares.getOrElse(DefaultEduSectorShares)(sectorIdx.max(0).min(5))
    cdfSample(shares, rng)

  /** Draw education level for immigrant from immigrant-specific distribution. */
  def drawImmigrantEducation(rng: scala.util.Random): Int =
    cdfSample(EduImmigShares, rng)

  /** Wage premium multiplier for education level. */
  def eduWagePremium(education: Int): Double =
    EduWagePreemia(education.max(0).min(3))

  /** Retraining success multiplier for education level. */
  def eduRetrainMultiplier(education: Int): Double =
    EduRetrainMult(education.max(0).min(3))

  /** Initial skill range (floor, ceiling) for education level. */
  def eduSkillRange(education: Int): (Double, Double) =
    val idx = education.max(0).min(3)
    (EduSkillFloors(idx), EduSkillCeilings(idx))

  /** CDF sampling from a discrete probability vector. Returns index 0..len-1. */
  private def cdfSample(shares: Vector[Double], rng: scala.util.Random): Int =
    val r = rng.nextDouble()
    var cum = 0.0
    var i = 0
    while i < shares.length - 1 do
      cum += shares(i)
      if r < cum then return i
      i += 1
    shares.length - 1

  // ═══════════════════════════════════════════════════════════════════════
  // STAGED DIGITALIZATION — always-on, no master toggle
  // Firms' digitalReadiness (DR) drifts upward over time (secular trend of
  // IT adoption). Firms can invest in discrete DR upgrades (DigiInvestCost)
  // for a DigiInvestBoost jump. Higher DR reduces AI/Hybrid CAPEX costs
  // (DigiCapexDiscount, up to 30%). Models Poland's digital transformation
  // trajectory (EC DESI 2024: Poland rank 24/27 in EU).
  // ═══════════════════════════════════════════════════════════════════════
  val DigiDrift: Double =
    sys.env.get("DIGI_DRIFT").map(_.trim.toDouble).getOrElse(0.001) // Monthly DR drift (secular digitalization)
  val DigiInvestCost: Double = sys.env.get("DIGI_INVEST_COST").map(_.trim.toDouble).getOrElse(50000.0)
  val DigiInvestBoost: Double = sys.env.get("DIGI_INVEST_BOOST").map(_.trim.toDouble).getOrElse(0.05)
  val DigiCapexDiscount: Double = sys.env.get("DIGI_CAPEX_DISCOUNT").map(_.trim.toDouble).getOrElse(0.30)
  val DigiInvestBaseProb: Double = sys.env.get("DIGI_INVEST_BASE_PROB").map(_.trim.toDouble).getOrElse(0.08)

  // ═══════════════════════════════════════════════════════════════════════
  // PHYSICAL CAPITAL & DEPRECIATION — always-on, no master toggle
  // Firm-level physical capital stock K with Cobb-Douglas production:
  // Y = A · K^α · L^(1-α) where α = PhysCapProdElast (0.30).
  // Sector-specific K/L ratios and depreciation rates calibrated to GUS
  // national accounts (GFCF structure by NACE) and Eurostat SBS.
  // Capital goods partially imported (PhysCapImportShare 35%, consistent
  // with Poland's machinery & equipment import structure).
  // Sources: GUS "Środki trwałe w gospodarce narodowej", Eurostat SBS.
  // ═══════════════════════════════════════════════════════════════════════
  val PhysCapEnabled: Boolean = sys.env.get("PHYS_CAPITAL_ENABLED").map(_.trim.toBoolean).getOrElse(true)
  val PhysCapKLRatios: Vector[Double] = sys.env.get("PHYS_CAPITAL_KL_RATIOS") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 6, s"PHYS_CAPITAL_KL_RATIOS must have 6 values, got ${v.length}")
      v
    case _ => Vector(120000.0, 250000.0, 80000.0, 200000.0, 150000.0, 180000.0)
  val PhysCapDepRates: Vector[Double] = sys.env.get("PHYS_CAPITAL_DEP_RATES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 6, s"PHYS_CAPITAL_DEP_RATES must have 6 values, got ${v.length}")
      v
    case _ => Vector(0.15, 0.08, 0.10, 0.07, 0.05, 0.08)
  val PhysCapImportShare: Double = sys.env.get("PHYS_CAPITAL_IMPORT_SHARE").map(_.trim.toDouble).getOrElse(0.35)
  val PhysCapAdjustSpeed: Double = sys.env.get("PHYS_CAPITAL_ADJUST_SPEED").map(_.trim.toDouble).getOrElse(0.10)
  val PhysCapProdElast: Double = sys.env.get("PHYS_CAPITAL_PROD_ELAST").map(_.trim.toDouble).getOrElse(0.30)
  val PhysCapCostReplace: Double = sys.env.get("PHYS_CAPITAL_COST_REPLACE").map(_.trim.toDouble).getOrElse(0.50)

  // ═══════════════════════════════════════════════════════════════════════
  // INVENTORIES
  // Firm-level inventory buffer: target ratio of inventories to monthly revenue,
  // differentiated by sector (Agriculture 30%, Manufacturing 25%, BPO 5%).
  // Spoilage: perishable sectors (Agri 10%/month, Retail 5%) lose inventory.
  // Carrying cost: 6% annual holding cost (warehousing, insurance, opportunity).
  // Stress liquidation: bankrupt firms sell inventory at discount.
  // GDP accounting: GDP += ΔInventories per SNA 2008 (expenditure approach).
  // Sources: GUS "Zapasy" (quarterly), Eurostat SBS, SNA 2008 §6.189-6.212.
  // ═══════════════════════════════════════════════════════════════════════
  val InventoryEnabled: Boolean = sys.env.get("INVENTORY_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val InventoryTargetRatios: Vector[Double] = sys.env.get("INVENTORY_TARGET_RATIOS") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(
        v.length == SECTORS.length,
        s"INVENTORY_TARGET_RATIOS must have ${SECTORS.length} values, got ${v.length}",
      )
      v
    case _ => Vector(0.05, 0.25, 0.15, 0.10, 0.02, 0.30)
  val InventoryAdjustSpeed: Double = sys.env.get("INVENTORY_ADJUST_SPEED").map(_.trim.toDouble).getOrElse(0.10)
  val InventoryCarryingCost: Double = sys.env.get("INVENTORY_CARRYING_COST").map(_.trim.toDouble).getOrElse(0.06)
  val InventorySpoilageRates: Vector[Double] = sys.env.get("INVENTORY_SPOILAGE_RATES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(
        v.length == SECTORS.length,
        s"INVENTORY_SPOILAGE_RATES must have ${SECTORS.length} values, got ${v.length}",
      )
      v
    case _ => Vector(0.0, 0.02, 0.05, 0.03, 0.0, 0.10)
  val InventoryCostFraction: Double = sys.env.get("INVENTORY_COST_FRACTION").map(_.trim.toDouble).getOrElse(0.50)
  val InventoryLiquidationDisc: Double = sys.env.get("INVENTORY_LIQUIDATION_DISC").map(_.trim.toDouble).getOrElse(0.50)
  val InventoryInitRatio: Double = sys.env.get("INVENTORY_INIT_RATIO").map(_.trim.toDouble).getOrElse(0.80)
  // Inventory cost replace: fraction of OtherCosts replaced by explicit inventory carrying cost.
  // Inventory carrying ≈ 1-2% of revenue (Eurostat SBS 2023).
  val InventoryCostReplace: Double = sys.env.get("INVENTORY_COST_REPLACE").map(_.trim.toDouble).getOrElse(0.10)

  // ═══════════════════════════════════════════════════════════════════════
  // INFORMAL ECONOMY
  // Shadow economy in Poland: ~15-17% of GDP (Schneider 2023 estimates).
  // Counter-cyclical: unemployment ↑ → informal activity ↑ (workers pushed
  // into shadow sector). Four-channel tax evasion: CIT (firm-level, 80%),
  // VAT (aggregate, 90%), PIT (aggregate, 85%), excise (aggregate, 70%).
  // Per-sector shadow shares: Agriculture 35%, Retail 30%, Healthcare 20%,
  // Manufacturing 15%, BPO 5%, Public 2% (NIK reports, Schneider & Buehn 2018).
  // Sources: Schneider (2023) shadow economy estimates, NIK (Najwyższa Izba
  // Kontroli) reports on tax gap, MF KAS enforcement data.
  // Affects: Identity 3 (tax revenue ↓).
  // ═══════════════════════════════════════════════════════════════════════
  val InformalEnabled: Boolean = sys.env.get("INFORMAL_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val InformalSectorShares: Vector[Double] = sys.env.get("INFORMAL_SECTOR_SHARES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == SECTORS.length, s"INFORMAL_SECTOR_SHARES must have ${SECTORS.length} values, got ${v.length}")
      v
    case _ => Vector(0.05, 0.15, 0.30, 0.20, 0.02, 0.35)
  val InformalCitEvasion: Double = sys.env.get("INFORMAL_CIT_EVASION").map(_.trim.toDouble).getOrElse(0.80)
  val InformalVatEvasion: Double = sys.env.get("INFORMAL_VAT_EVASION").map(_.trim.toDouble).getOrElse(0.90)
  val InformalPitEvasion: Double = sys.env.get("INFORMAL_PIT_EVASION").map(_.trim.toDouble).getOrElse(0.85)
  val InformalExciseEvasion: Double = sys.env.get("INFORMAL_EXCISE_EVASION").map(_.trim.toDouble).getOrElse(0.70)
  val InformalUnempThreshold: Double = sys.env.get("INFORMAL_UNEMP_THRESHOLD").map(_.trim.toDouble).getOrElse(0.05)
  val InformalCyclicalSens: Double = sys.env.get("INFORMAL_CYCLICAL_SENS").map(_.trim.toDouble).getOrElse(0.50)
  // Exponential smoothing for cyclical adjustment (Schneider 2023: informal economy adjusts over quarters, not months)
  val InformalSmoothing: Double = sys.env.get("INFORMAL_SMOOTHING").map(_.trim.toDouble).getOrElse(0.92)

  // ═══════════════════════════════════════════════════════════════════════
  // ENERGY / CLIMATE POLICY
  // Poland's energy sector: heavily coal-dependent (~70% electricity from
  // coal, URE 2024), undergoing green transition (Fit for 55, EU ETS Phase IV).
  // Per-sector energy costs as share of revenue (Eurostat/GUS SBS 2023):
  // Manufacturing highest (10%), Agriculture (6%), others lower.
  // EU ETS carbon pricing: €80/tonne base, 3% annual drift (MSR tightening).
  // Green capital investment: separate cash pool (GreenBudgetShare 20% of
  // CAPEX budget), decarbonization pathway reduces carbon cost by up to 30%.
  // Sources: URE (Urząd Regulacji Energetyki), Eurostat energy statistics,
  // EU ETS auction data (EEX), PEP 2040 (Polityka Energetyczna Polski).
  // ═══════════════════════════════════════════════════════════════════════
  val EnergyEnabled: Boolean = sys.env.get("ENERGY_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val EnergyCostShares: Vector[Double] = sys.env.get("ENERGY_COST_SHARES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == SECTORS.length, s"ENERGY_COST_SHARES must have ${SECTORS.length} values, got ${v.length}")
      v
    case _ => Vector(0.02, 0.10, 0.04, 0.05, 0.03, 0.06)
  val EnergyCarbonIntensity: Vector[Double] = sys.env.get("ENERGY_CARBON_INTENSITY") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(
        v.length == SECTORS.length,
        s"ENERGY_CARBON_INTENSITY must have ${SECTORS.length} values, got ${v.length}",
      )
      v
    case _ => Vector(0.01, 0.08, 0.02, 0.01, 0.02, 0.04)
  val EtsBasePrice: Double = sys.env.get("ETS_BASE_PRICE").map(_.trim.toDouble).getOrElse(80.0)
  val EtsPriceDrift: Double = sys.env.get("ETS_PRICE_DRIFT").map(_.trim.toDouble).getOrElse(0.03)
  val GreenKLRatios: Vector[Double] = sys.env.get("GREEN_KL_RATIOS") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == SECTORS.length, s"GREEN_KL_RATIOS must have ${SECTORS.length} values, got ${v.length}")
      v
    case _ => Vector(5000.0, 30000.0, 10000.0, 15000.0, 8000.0, 20000.0)
  val GreenDepRate: Double = sys.env.get("GREEN_DEP_RATE").map(_.trim.toDouble).getOrElse(0.04)
  val GreenAdjustSpeed: Double = sys.env.get("GREEN_ADJUST_SPEED").map(_.trim.toDouble).getOrElse(0.08)
  val GreenMaxDiscount: Double = sys.env.get("GREEN_MAX_DISCOUNT").map(_.trim.toDouble).getOrElse(0.30)
  val GreenImportShare: Double = sys.env.get("GREEN_IMPORT_SHARE").map(_.trim.toDouble).getOrElse(0.35)
  val GreenInitRatio: Double = sys.env.get("GREEN_INIT_RATIO").map(_.trim.toDouble).getOrElse(0.10)
  val GreenBudgetShare: Double = sys.env.get("GREEN_BUDGET_SHARE").map(_.trim.toDouble).getOrElse(0.20)
  // Energy cost replace: fraction of OtherCosts replaced by explicit energy cost.
  // Energy ≈ 5% of revenue ≈ 30% of non-labor operating costs (Eurostat SBS 2023).
  val EnergyCostReplace: Double = sys.env.get("ENERGY_COST_REPLACE").map(_.trim.toDouble).getOrElse(0.30)
  // Share of energy costs that stay in the domestic economy (domestic energy production).
  // Poland: ~60% domestic generation (PGE, Enea, Tauron, Orlen — URE 2024).
  val EnergyDomesticShare: Double = sys.env.get("ENERGY_DOMESTIC_SHARE").map(_.trim.toDouble).getOrElse(0.60)

  // ═══════════════════════════════════════════════════════════════════════
  // DIASPORA REMITTANCES
  // Poland receives ~18 bln PLN/year in personal transfers (NBP BoP 2024,
  // category "Personal transfers, credit"), primarily from UK, Germany, and
  // Netherlands. Inflows are counter-cyclical: when domestic unemployment
  // rises, diaspora sends more (Frankel 2011: remittances as informal insurance).
  // ER pass-through: PLN depreciation increases PLN-denominated inflows
  // (50% elasticity, Chami, Fullenkamp & Jahjah 2005).
  // Affects: Identity 2 (bank deposits ↑), Identity 4 (BoP current account).
  // ═══════════════════════════════════════════════════════════════════════
  val RemittanceEnabled: Boolean = sys.env.get("REMITTANCE_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  // Per-capita monthly inflow in PLN (NBP BoP 2024: 18 bln PLN/year ÷ ~38M population ÷ 12)
  val RemittancePerCapita: Double = sys.env.get("REMITTANCE_PER_CAPITA").map(_.trim.toDouble).getOrElse(40.0)
  // ER elasticity: 1% PLN depreciation → 0.5% higher PLN inflows (partial pass-through)
  val RemittanceErElasticity: Double = sys.env.get("REMITTANCE_ER_ELASTICITY").map(_.trim.toDouble).getOrElse(0.5)
  // Secular growth trend: 2%/year (emigrant wage growth in host countries)
  val RemittanceGrowthRate: Double = sys.env.get("REMITTANCE_GROWTH_RATE").map(_.trim.toDouble).getOrElse(0.02)
  // Counter-cyclical sensitivity: excess unemployment → higher remittances
  val RemittanceCyclicalSens: Double = sys.env.get("REMITTANCE_CYCLICAL_SENS").map(_.trim.toDouble).getOrElse(0.3)

  // ═══════════════════════════════════════════════════════════════════════
  // TOURISM
  // Inbound tourism: ~5% GDP (GUS Tourism Satellite Account 2023; ~95M
  // overnight stays). Outbound: ~3% GDP (NBP BoP 2023 travel debit).
  // Strong seasonality: peak July (±40% amplitude). ER-sensitive: PLN
  // depreciation boosts inbound, dampens outbound (elasticity 0.6).
  // COVID-type shock capability: TourismShockMonth triggers 80% drop
  // with gradual recovery (TourismShockRecovery 3%/month).
  // Sources: GUS TSA (Tourism Satellite Account), NBP BoP (travel), UNWTO.
  // Affects: Identity 2 (bank deposits), Identity 4 (BoP current account).
  // ═══════════════════════════════════════════════════════════════════════
  val TourismEnabled: Boolean = sys.env.get("TOURISM_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val TourismInboundShare: Double = sys.env.get("TOURISM_INBOUND_SHARE").map(_.trim.toDouble).getOrElse(0.05)
  val TourismOutboundShare: Double = sys.env.get("TOURISM_OUTBOUND_SHARE").map(_.trim.toDouble).getOrElse(0.03)
  val TourismErElasticity: Double = sys.env.get("TOURISM_ER_ELASTICITY").map(_.trim.toDouble).getOrElse(0.6)
  val TourismSeasonality: Double = sys.env.get("TOURISM_SEASONALITY").map(_.trim.toDouble).getOrElse(0.40)
  val TourismPeakMonth: Int = sys.env.get("TOURISM_PEAK_MONTH").map(_.trim.toInt).getOrElse(7)
  val TourismGrowthRate: Double = sys.env.get("TOURISM_GROWTH_RATE").map(_.trim.toDouble).getOrElse(0.03)
  val TourismShockMonth: Int = sys.env.get("TOURISM_SHOCK_MONTH").map(_.trim.toInt).getOrElse(0)
  val TourismShockSize: Double = sys.env.get("TOURISM_SHOCK_SIZE").map(_.trim.toDouble).getOrElse(0.80)
  val TourismShockRecovery: Double = sys.env.get("TOURISM_SHOCK_RECOVERY").map(_.trim.toDouble).getOrElse(0.03)

  // ═══════════════════════════════════════════════════════════════════════
  // HETEROGENEOUS HOUSEHOLDS
  // Individual household agents with heterogeneous savings, debt, MPC, skills,
  // and education. Savings: LogNormal (GUS BBGD 2023), median ~15K PLN,
  // matching wealth inequality. Debt: 40% of HH have debt (BIK 2023),
  // LogNormal among indebted. MPC: Beta distribution, mean ~0.82 (GUS BBGD).
  // Skill dynamics: decay during unemployment (hysteresis), health scarring
  // after 3 months (long-term unemployment effects; Arulampalam et al. 2001).
  // Retraining: 6-month duration, 60% base success, education-dependent.
  // Sources: GUS BBGD (Badanie Budżetów Gospodarstw Domowych), BIK,
  // NBP Household Wealth Survey 2022, OECD Wealth Distribution Database.
  // ═══════════════════════════════════════════════════════════════════════
  val HhCount = sys.env.get("HH_COUNT").map(_.trim.toInt).getOrElse(TotalPopulation)

  // Savings distribution (GUS BBGD 2023): LogNormal(mu, sigma) → median ~15K PLN
  val HhSavingsMu = sys.env.get("HH_SAVINGS_MU").map(_.trim.toDouble).getOrElse(9.6)
  val HhSavingsSigma = 1.2
  // Debt: 40% of households have debt; among those, LogNormal
  val HhDebtFraction = 0.40
  val HhDebtMu = 10.5 // median ~36K PLN for indebted
  val HhDebtSigma = 1.5
  // Rent: Normal(mean, std), floor at 800 PLN/month
  val HhRentMean = 1800.0
  val HhRentStd = 400.0
  val HhRentFloor = 800.0
  // MPC: Beta(alpha, beta) → mean ~0.82
  val HhMpcAlpha = sys.env.get("HH_MPC_ALPHA").map(_.trim.toDouble).getOrElse(8.2)
  val HhMpcBeta = sys.env.get("HH_MPC_BETA").map(_.trim.toDouble).getOrElse(1.8)
  // Skill decay and health scarring
  val HhSkillDecayRate = 0.02 // per month after onset
  val HhScarringRate = 0.02 // health penalty per month after onset
  val HhScarringCap = 0.50
  val HhScarringOnset = 3 // months before scarring/skill decay starts
  // Retraining
  val HhRetrainingCost = sys.env.get("HH_RETRAIN_COST").map(_.trim.toDouble).getOrElse(5000.0)
  val HhRetrainingDuration = sys.env.get("HH_RETRAIN_DUR").map(_.trim.toInt).getOrElse(6)
  val HhRetrainingBaseSuccess = 0.60
  val HhRetrainingProb = sys.env.get("HH_RETRAIN_PROB").map(_.trim.toDouble).getOrElse(0.15)
  val HhRetrainingEnabled = sys.env.get("HH_RETRAIN_ENABLED").map(_.trim.toBoolean).getOrElse(true)
  // Bankruptcy
  val HhBankruptcyThreshold = -3.0 // multiplied by monthlyRent
  // Social network (household-level WS)
  val HhSocialK = 10
  val HhSocialP = 0.15
  // Debt service
  val HhDebtServiceRate = 0.02 // monthly (2% of outstanding) — aggregate mode fallback
  val HhBaseAmortRate = 0.015 // monthly principal amortization (individual mode)
  val HhDepositSpread = 0.02 // annual: deposit rate = max(0, refRate - spread)
