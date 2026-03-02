package sfc.config

/** Monetary regime: determines central bank behavior and exchange rate dynamics. */
enum MonetaryRegime:
  case Pln   // NBP Taylor rule + floating PLN/EUR
  case Eur   // Exogenous ECB rate + fixed exchange rate (Eurozone membership)

/** Network topology selection for comparative experiments. */
enum Topology:
  case Ws, Er, Ba, Lattice

/** Household mode: aggregate (backward-compat) or individual agents (Paper-06). */
enum HhMode:
  case Aggregate, Individual

/**
 * Runtime configuration: values that depend on CLI arguments.
 * Passed through runSingle and Simulation.step.
 */
case class RunConfig(
  bdpAmount: Double,
  nSeeds: Int,
  outputPrefix: String,
  regime: MonetaryRegime = MonetaryRegime.Pln
):
  val isNoBdp: Boolean = bdpAmount == 0.0
  val isEurozone: Boolean = regime == MonetaryRegime.Eur

/**
 * 4-to-6 sector definition with heterogeneous sigma (CES elasticity of substitution).
 * sigma affects: decision threshold, automation efficiency, CAPEX costs.
 */
case class SectorDef(
  name: String,
  share: Double,        // Share of firm population (GUS BAEL 2024)
  sigma: Double,        // CES elasticity of substitution
  wageMultiplier: Double,        // Sector wage multiplier vs national average
  revenueMultiplier: Double,
  aiCapexMultiplier: Double,
  hybridCapexMultiplier: Double,
  baseDigitalReadiness: Double,  // Central tendency of digitalReadiness
  hybridRetainFrac: Double       // Fraction of workers RETAINED in hybrid mode (0.5 = halve)
)

// Calibration GUS/NBP 2024: 6 sectors of the Polish economy
private val BASE_SECTORS: Vector[SectorDef] = Vector(
  //                             share   sigma wage  rev   aiCpx hybCpx digiR  hybRet
  SectorDef("BPO/SSC",          0.03, 50.0, 1.35, 1.50, 0.70, 0.70,  0.50,  0.50),  // ~489k workers (ABSL), avg 11 154 PLN
  SectorDef("Manufacturing",    0.16, 10.0, 0.94, 1.05, 1.12, 1.05,  0.45,  0.60),  // ~2.8M workers, avg ~7 800 PLN
  SectorDef("Retail/Services",  0.45,  5.0, 0.79, 0.91, 0.85, 0.80,  0.40,  0.65),  // ~61% employment (services), avg ~6 500 PLN
  SectorDef("Healthcare",       0.06,  2.0, 0.97, 1.10, 1.38, 1.25,  0.25,  0.75),  // ~5.5%, nurses 6 890, doctors 16 300
  SectorDef("Public",           0.22,  1.0, 0.91, 1.08, 3.00, 2.50,  0.08,  0.90),  // ~22% employment (public sector), avg ~7 500 PLN
  SectorDef("Agriculture",      0.08,  3.0, 0.67, 0.80, 2.50, 2.00,  0.12,  0.85)   // ~8% BAEL, avg ~5 500 PLN
)

/** SECTORS with optional sigma override via SIGMAS env var.
  * Format: SIGMAS="3.5,2.0,2.5,0.8,0.5,1.2" (6 comma-separated values, one per sector).
  * When unset, uses calibrated values from BASE_SECTORS.
  * SIGMA_MULT env var multiplies all sector sigmas (applied after SIGMAS override). */
val SECTORS: Vector[SectorDef] =
  val afterSigmas = sys.env.get("SIGMAS") match
    case Some(s) if s.nonEmpty =>
      val sigmas = s.split(",").map(_.trim.toDouble)
      require(sigmas.length == BASE_SECTORS.length,
        s"SIGMAS env var must have ${BASE_SECTORS.length} values, got ${sigmas.length}")
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
    case _ => Config.WorkersPerFirm  // "uniform": all 10

object Config:
  val FirmsCount       = sys.env.get("FIRMS_COUNT").map(_.trim.toInt).getOrElse(10000)
  val WorkersPerFirm   = 10
  var TotalPopulation: Int = FirmsCount * WorkersPerFirm
  def setTotalPopulation(n: Int): Unit = TotalPopulation = n
  private val ScaleFactor = FirmsCount.toDouble / 10000.0

  // Firm size distribution (v6.0)
  val FirmSizeDist: String = sys.env.get("FIRM_SIZE_DIST").map(_.trim.toLowerCase).getOrElse("uniform")
  val FirmSizeMicroShare: Double = sys.env.get("FIRM_SIZE_MICRO_SHARE").map(_.trim.toDouble).getOrElse(0.962)
  val FirmSizeSmallShare: Double = sys.env.get("FIRM_SIZE_SMALL_SHARE").map(_.trim.toDouble).getOrElse(0.028)
  val FirmSizeMediumShare: Double = sys.env.get("FIRM_SIZE_MEDIUM_SHARE").map(_.trim.toDouble).getOrElse(0.008)
  val FirmSizeLargeShare: Double = sys.env.get("FIRM_SIZE_LARGE_SHARE").map(_.trim.toDouble).getOrElse(0.002)
  val FirmSizeLargeMax: Int = sys.env.get("FIRM_SIZE_LARGE_MAX").map(_.trim.toInt).getOrElse(1000)
  val Duration         = sys.env.get("DURATION").map(_.trim.toInt).getOrElse(120)
  val ShockMonth       = sys.env.get("SHOCK_MONTH").map(_.trim.toInt).getOrElse(30)

  // Firm (base -- sectors modify these values)
  val BaseRevenue      = 100000.0   // Scaled to GUS 2024 wage level
  val OtherCosts       = 16667.0
  val AiCapex          = 1200000.0
  val HybridCapex      = 350000.0
  val AiOpex           = 30000.0
  val HybridOpex       = 12000.0
  val AutoSkeletonCrew = 2
  val HybridReadinessMin = 0.20
  val FullAiReadinessMin = 0.35

  // Households (GUS 2024)
  val BaseWage              = 8266.0    // GUS average gross 2024
  val BaseReservationWage   = 4666.0    // Minimum wage 2025
  val ReservationBdpMult    = 0.5
  val Mpc                   = 0.82
  val LaborSupplySteepness  = 8.0
  val WageAdjSpeed          = 0.12

  // Government
  val CitRate          = 0.19
  // Reduced VAT rates (#38): per-sector effective rate (Ustawa o VAT / GUS COICOP 2024)
  // Standard 23%, food/hospitality 8%, basic goods 5%, medical exempt
  val VatRates: Vector[Double] = sys.env.get("VAT_RATES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 6, s"VAT_RATES must have 6 values, got ${v.length}")
      v
    case _ => Vector(0.23, 0.19, 0.12, 0.06, 0.10, 0.07)

  // Excise & customs (#39) — always-on
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

  val GovBaseSpending  = 100000000.0 * ScaleFactor

  // Public Investment vs Current Spending (#27)
  val GovInvestEnabled: Boolean = sys.env.get("GOV_INVEST_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val GovInvestShare: Double = sys.env.get("GOV_INVEST_SHARE").map(_.trim.toDouble).getOrElse(0.20)
  val GovCapitalMultiplier: Double = sys.env.get("GOV_CAPITAL_MULTIPLIER").map(_.trim.toDouble).getOrElse(1.5)
  val GovCurrentMultiplier: Double = sys.env.get("GOV_CURRENT_MULTIPLIER").map(_.trim.toDouble).getOrElse(0.8)
  val GovDepreciationRate: Double = sys.env.get("GOV_DEPRECIATION_RATE").map(_.trim.toDouble).getOrElse(0.06)
  val GovInitCapital: Double = sys.env.get("GOV_INIT_CAPITAL").map(_.trim.toDouble).getOrElse(0.0)

  // EU Funds Dynamics (#28)
  val EuFundsEnabled: Boolean = sys.env.get("EU_FUNDS_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val EuFundsTotalEur: Double = sys.env.get("EU_FUNDS_TOTAL_EUR").map(_.trim.toDouble).getOrElse(76e9)
  val EuFundsPeriodMonths: Int = sys.env.get("EU_FUNDS_PERIOD_MONTHS").map(_.trim.toInt).getOrElse(84)
  val EuFundsStartMonth: Int = sys.env.get("EU_FUNDS_START_MONTH").map(_.trim.toInt).getOrElse(1)
  val EuFundsAlpha: Double = sys.env.get("EU_FUNDS_ALPHA").map(_.trim.toDouble).getOrElse(2.0)
  val EuFundsBeta: Double = sys.env.get("EU_FUNDS_BETA").map(_.trim.toDouble).getOrElse(5.0)
  val EuCofinanceRate: Double = sys.env.get("EU_COFINANCE_RATE").map(_.trim.toDouble).getOrElse(0.15)
  val EuCapitalShare: Double = sys.env.get("EU_CAPITAL_SHARE").map(_.trim.toDouble).getOrElse(0.60)

  // Minimum Wage Dynamics (#29)
  val MinWageEnabled: Boolean = sys.env.get("MIN_WAGE_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val MinWageAdjustMonths: Int = sys.env.get("MIN_WAGE_ADJUST_MONTHS").map(_.trim.toInt).getOrElse(12)
  val MinWageInflationIndex: Boolean = sys.env.get("MIN_WAGE_INFLATION_INDEX").map(_.trim.toBoolean).getOrElse(true)
  val MinWageTargetRatio: Double = sys.env.get("MIN_WAGE_TARGET_RATIO").map(_.trim.toDouble).getOrElse(0.50)
  val MinWageConvergenceSpeed: Double = sys.env.get("MIN_WAGE_CONVERGENCE_SPEED").map(_.trim.toDouble).getOrElse(0.33)

  // Flow-of-Funds: sector-level demand multipliers (#30)
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
      require(v.length == 6 && Math.abs(v.sum - 1.0) < 0.01,
        s"FOF_CONS_WEIGHTS must have 6 values summing to ~1.0, got ${v.length} summing to ${v.sum}")
      v
    case _ => Vector(0.02, 0.22, 0.53, 0.06, 0.07, 0.10)
  val FofGovWeights: Vector[Double] = sys.env.get("FOF_GOV_WEIGHTS") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 6 && Math.abs(v.sum - 1.0) < 0.01,
        s"FOF_GOV_WEIGHTS must have 6 values summing to ~1.0, got ${v.length} summing to ${v.sum}")
      v
    case _ => Vector(0.04, 0.12, 0.08, 0.16, 0.50, 0.10)
  val FofExportShares: Vector[Double] = sys.env.get("FOF_EXPORT_SHARES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 6 && Math.abs(v.sum - 1.0) < 0.01,
        s"FOF_EXPORT_SHARES must have 6 values summing to ~1.0, got ${v.length} summing to ${v.sum}")
      v
    case _ => Vector(0.07, 0.52, 0.12, 0.02, 0.03, 0.24)

  // NBP (NBP data 2024)
  val NbpInitialRate   = 0.0575      // NBP reference rate 2024
  val NbpTargetInfl    = 0.025       // NBP target 2.5% +/- 1pp
  val NbpNeutralRate   = 0.04        // NBP neutral rate
  val TaylorAlpha      = 1.5
  val TaylorBeta       = 0.8
  val TaylorInertia    = 0.70
  val RateFloor        = sys.env.get("NBP_RATE_FLOOR").map(_.trim.toDouble).getOrElse(0.001)
  val RateCeiling      = 0.25

  // Symmetric Taylor rule (v2.0 — dual mandate)
  val NbpSymmetric: Boolean = sys.env.get("NBP_SYMMETRIC").map(_.trim.toBoolean).getOrElse(true)
  val NbpNairu: Double = sys.env.get("NBP_NAIRU").map(_.trim.toDouble).getOrElse(0.05)
  val TaylorDelta: Double = sys.env.get("NBP_DELTA").map(_.trim.toDouble).getOrElse(0.5)

  // ECB (Eurozone counterfactual)
  val EcbInitialRate   = 0.035       // ECB deposit facility rate
  val EcbNeutralRate   = 0.025       // ECB neutral rate (lower than NBP)
  val EcbTargetInfl    = 0.020       // ECB target 2.0% (symmetric)
  val EcbAlpha         = 1.5         // ECB inflation response
  val EcbInertia       = 0.85        // ECB smoother than NBP
  val EuroInflation    = 0.020       // Exogenous Eurozone-wide inflation (constant)

  // SGP fiscal constraint (EUR regime only)
  val SgpDeficitLimit  = 0.03        // Maastricht: annual deficit < 3% of GDP
  val SgpDebtLimit     = 0.60        // Maastricht: public debt < 60% of GDP
  val SgpAusterityRate = 2.0         // Austerity speed: BDP × max(0, 1 - (debtRatio - 0.60) × rate)

  // Unemployment benefits (v2.0 — automatic stabilizers)
  val GovUnempBenefitEnabled: Boolean = sys.env.get("GOV_UNEMP_BENEFIT").map(_.trim.toBoolean).getOrElse(true)
  val GovBenefitM1to3: Double = sys.env.get("GOV_BENEFIT_M1_3").map(_.trim.toDouble).getOrElse(1500.0)
  val GovBenefitM4to6: Double = sys.env.get("GOV_BENEFIT_M4_6").map(_.trim.toDouble).getOrElse(1200.0)
  val GovBenefitDuration: Int = sys.env.get("GOV_BENEFIT_DURATION").map(_.trim.toInt).getOrElse(6)

  // Bond market (Mechanism 3)
  val GovBondMarket: Boolean = sys.env.get("GOV_BOND_MARKET").map(_.trim.toBoolean).getOrElse(true)
  val GovFiscalRiskBeta: Double = sys.env.get("GOV_FISCAL_RISK_BETA").map(_.trim.toDouble).getOrElse(2.0)
  val GovTermPremium: Double = sys.env.get("GOV_TERM_PREMIUM").map(_.trim.toDouble).getOrElse(0.005)

  // QE (Mechanism 4)
  val NbpQe: Boolean = sys.env.get("NBP_QE").map(_.trim.toBoolean).getOrElse(false)
  val NbpQePace: Double = sys.env.get("NBP_QE_PACE").map(_.trim.toDouble).getOrElse(5e9) * ScaleFactor
  val NbpQeMaxGdpShare: Double = sys.env.get("NBP_QE_MAX_GDP_SHARE").map(_.trim.toDouble).getOrElse(0.30)

  // FX Intervention (Mechanism 5)
  val NbpFxIntervention: Boolean = sys.env.get("NBP_FX_INTERVENTION").map(_.trim.toBoolean).getOrElse(false)
  val NbpFxBand: Double = sys.env.get("NBP_FX_BAND").map(_.trim.toDouble).getOrElse(0.10)
  val NbpFxReserves: Double = sys.env.get("NBP_FX_RESERVES").map(_.trim.toDouble).getOrElse(185e9) * ScaleFactor  // EUR-equivalent total
  val NbpFxMaxMonthly: Double = sys.env.get("NBP_FX_MAX_MONTHLY").map(_.trim.toDouble).getOrElse(0.03)
  val NbpFxStrength: Double = sys.env.get("NBP_FX_STRENGTH").map(_.trim.toDouble).getOrElse(0.5)

  // Banking system
  val InitBankCapital  = 500000000.0 * ScaleFactor
  val BaseSpread       = 0.015       // NBP MIR corporate spread 2024
  val NplSpreadFactor  = 5.0
  val MinCar           = 0.08
  val LoanRecovery     = 0.30

  // Multi-bank
  val BankMulti: Boolean = sys.env.get("BANK_MODE").map(_.trim.toLowerCase).getOrElse("single") == "multi"
  val BankFailureEnabled: Boolean = sys.env.get("BANK_FAILURE").map(_.trim.toBoolean).getOrElse(false)
  val BankReserveReq: Double = sys.env.get("BANK_RESERVE_REQ").map(_.trim.toDouble).getOrElse(0.035)
  val BankStressThreshold: Double = sys.env.get("BANK_STRESS_THRESHOLD").map(_.trim.toDouble).getOrElse(0.05)

  // Credit diagnostics: M1/M2 monetary aggregates
  val CreditDiagnostics: Boolean = sys.env.get("CREDIT_DIAGNOSTICS").map(_.trim.toBoolean).getOrElse(false)

  // JST / Samorządy: two-tier fiscal system
  val JstEnabled: Boolean = sys.env.get("JST_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val JstPitShare: Double = sys.env.get("JST_PIT_SHARE").map(_.trim.toDouble).getOrElse(0.3846)    // Ustawa o dochodach JST, Art. 4
  val JstCitShare: Double = sys.env.get("JST_CIT_SHARE").map(_.trim.toDouble).getOrElse(0.0671)    // j.w.
  val JstPropertyTax: Double = sys.env.get("JST_PROPERTY_TAX").map(_.trim.toDouble).getOrElse(5000.0) // PLN/firm/year (GUS 2023)
  val JstSubventionShare: Double = sys.env.get("JST_SUBVENTION_SHARE").map(_.trim.toDouble).getOrElse(0.03)  // ~3% GDP
  val JstDotacjeShare: Double = sys.env.get("JST_DOTACJE_SHARE").map(_.trim.toDouble).getOrElse(0.01)  // ~1% GDP
  val JstSpendingMult: Double = sys.env.get("JST_SPENDING_MULT").map(_.trim.toDouble).getOrElse(1.02)  // ~2% deficit ratio

  // LCR/NSFR: maturity mismatch + liquidity ratios
  val BankLcrEnabled: Boolean = sys.env.get("BANK_LCR_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val BankLcrMin: Double = sys.env.get("BANK_LCR_MIN").map(_.trim.toDouble).getOrElse(1.0)      // Basel III minimum 100%
  val BankNsfrMin: Double = sys.env.get("BANK_NSFR_MIN").map(_.trim.toDouble).getOrElse(1.0)    // Basel III minimum 100%
  val BankDemandDepositRunoff: Double = sys.env.get("BANK_DEMAND_DEPOSIT_RUNOFF").map(_.trim.toDouble).getOrElse(0.10)  // Basel III retail
  val BankTermDepositFrac: Double = sys.env.get("BANK_TERM_DEPOSIT_FRAC").map(_.trim.toDouble).getOrElse(0.40)

  // Interbank term structure
  val InterbankTermStructure: Boolean = sys.env.get("INTERBANK_TERM_STRUCTURE").map(_.trim.toBoolean).getOrElse(false)

  // ZUS / Social Insurance
  val ZusEnabled: Boolean = sys.env.get("ZUS_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val ZusContribRate: Double = sys.env.get("ZUS_CONTRIB_RATE").map(_.trim.toDouble).getOrElse(0.1952)  // pension portion
  val ZusBasePension: Double = sys.env.get("ZUS_BASE_PENSION").map(_.trim.toDouble).getOrElse(3500.0)  // PLN/month after waloryzacja
  val ZusScale: Double = sys.env.get("ZUS_SCALE").map(_.trim.toDouble).getOrElse(1.0)  // gradual introduction

  // PPK / Capital Pension
  val PpkEnabled: Boolean = sys.env.get("PPK_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val PpkEmployeeRate: Double = sys.env.get("PPK_EMPLOYEE_RATE").map(_.trim.toDouble).getOrElse(0.02)  // 2%
  val PpkEmployerRate: Double = sys.env.get("PPK_EMPLOYER_RATE").map(_.trim.toDouble).getOrElse(0.015) // 1.5%
  val PpkBondAlloc: Double = sys.env.get("PPK_BOND_ALLOC").map(_.trim.toDouble).getOrElse(0.60)  // default TFI strategy

  // Demographics
  val DemEnabled: Boolean = sys.env.get("DEMOGRAPHICS_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val DemRetirementRate: Double = sys.env.get("DEM_RETIREMENT_RATE").map(_.trim.toDouble).getOrElse(0.001)  // 0.1%/month
  val DemWorkingAgeDecline: Double = sys.env.get("DEM_WORKING_AGE_DECLINE").map(_.trim.toDouble).getOrElse(0.002)  // 0.2%/year
  val DemInitialRetirees: Int = sys.env.get("DEM_INITIAL_RETIREES").map(_.trim.toInt).getOrElse(0)

  // Macroprudential: KNF/CCyB/OSII
  val MacropruEnabled: Boolean = sys.env.get("MACROPRU_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val CcybMax: Double = sys.env.get("CCYB_MAX").map(_.trim.toDouble).getOrElse(0.025)  // 2.5% max buffer
  val CcybActivationGap: Double = sys.env.get("CCYB_ACTIVATION_GAP").map(_.trim.toDouble).getOrElse(0.02)  // >2pp gap → build
  val CcybReleaseGap: Double = sys.env.get("CCYB_RELEASE_GAP").map(_.trim.toDouble).getOrElse(-0.02)  // <-2pp gap → release
  val OsiiPkoBp: Double = sys.env.get("OSII_PKO_BP").map(_.trim.toDouble).getOrElse(0.01)  // 1.0% for PKO BP
  val OsiiPekao: Double = sys.env.get("OSII_PEKAO").map(_.trim.toDouble).getOrElse(0.005)  // 0.5% for Pekao
  val ConcentrationLimit: Double = sys.env.get("CONCENTRATION_LIMIT").map(_.trim.toDouble).getOrElse(0.25)  // Art. 395 CRR

  // Reserve interest: NBP pays fraction of refRate on required reserves
  // Uchwała RPP nr 7/2003: oprocentowanie = 0.9 × stopa referencyjna (currently 0.5 in model for visibility)
  val NbpReserveRateMult: Double = sys.env.get("NBP_RESERVE_RATE_MULT").map(_.trim.toDouble).getOrElse(0.5)

  // Standing facilities: deposit/lombard facility actual flows
  val NbpStandingFacilities: Boolean = sys.env.get("NBP_STANDING_FACILITIES").map(_.trim.toBoolean).getOrElse(false)
  val NbpDepositFacilitySpread: Double = sys.env.get("NBP_DEPOSIT_FACILITY_SPREAD").map(_.trim.toDouble).getOrElse(0.01)
  val NbpLombardSpread: Double = sys.env.get("NBP_LOMBARD_SPREAD").map(_.trim.toDouble).getOrElse(0.01)

  // Foreign sector (NBP/ECB 2024)
  val BaseExRate       = 4.33        // NBP average PLN/EUR rate 2024
  val ForeignRate      = 0.04        // ECB rate 2024
  val ImportPropensity = 0.40
  val ExportBase       = 190000000.0 * ScaleFactor
  val TechImportShare  = 0.40
  val IrpSensitivity   = 0.15
  val ExRateAdjSpeed   = 0.02
  val ExportAutoBoost  = 0.15

  // Demand
  val DemandPassthrough = 0.40

  // Network
  val NetworkK          = 6      // Watts-Strogatz: neighbors
  val NetworkRewireP    = 0.10   // Watts-Strogatz: rewire probability
  val DemoEffectThresh  = sys.env.get("DEMO_THRESH").map(_.trim.toDouble).getOrElse(0.40)
  val DemoEffectBoost   = 0.15   // Modest boost to uncertainty discount from demonstration

  // Endogenous sigma dynamics (Paper-05)
  val SigmaLambda  = sys.env.get("SIGMA_LAMBDA").map(_.trim.toDouble).getOrElse(0.0)
  val SigmaCapMult = sys.env.get("SIGMA_CAP_MULT").map(_.trim.toDouble).getOrElse(3.0)

  // Dynamic network (Paper-05)
  val RewireRho    = sys.env.get("REWIRE_RHO").map(_.trim.toDouble).getOrElse(0.0)

  // I-O coupling (Paper-07)
  val IoEnabled: Boolean = sys.env.get("IO_MODE").map(_.trim.toLowerCase) match
    case Some("enabled" | "true" | "on") => true
    case _ => false

  val IoMatrix: Vector[Vector[Double]] = sys.env.get("IO_MATRIX") match
    case Some(s) if s.nonEmpty =>
      val rows = s.split(";").map(_.split(",").map(_.trim.toDouble).toVector).toVector
      require(rows.length == 6 && rows.forall(_.length == 6),
        s"IO_MATRIX must be 6x6, got ${rows.length} rows")
      rows
    // a_ij = input from sector i per unit gross output of sector j.
    // Calibrated from GUS symmetric I-O tables (2015/2019), cross-validated
    // against WIOD (2000-2014), OECD ICIO (2005-2020), Eurostat (2015/2019).
    // ISIC mapping: BPO←J62-63+N78-82, Mfg←C10-33, Ret←G-I+K-L+M69-75+R-S,
    //               Hlt←Q86-88, Pub←O84+P85, Agr←A01-03.
    case _ => Vector(
      Vector(0.05, 0.03, 0.04, 0.02, 0.03, 0.01),  // BPO purchases
      Vector(0.04, 0.35, 0.12, 0.15, 0.05, 0.18),  // Mfg purchases
      Vector(0.15, 0.10, 0.12, 0.08, 0.07, 0.08),  // Retail purchases
      Vector(0.01, 0.00, 0.01, 0.05, 0.02, 0.01),  // Healthcare purchases
      Vector(0.01, 0.01, 0.01, 0.01, 0.03, 0.01),  // Public purchases
      Vector(0.00, 0.08, 0.05, 0.01, 0.01, 0.12))  // Agriculture purchases

  val IoColumnSums: Vector[Double] =
    (0 until 6).map(j => IoMatrix.map(_(j)).sum).toVector

  // Pass-through fraction: 1.0 = full I-O, 0.0 = no I-O.
  // Models contractual rigidity, delivery lags, and supplier substitution.
  val IoScale: Double = sys.env.get("IO_SCALE").map(_.trim.toDouble).getOrElse(1.0)

  // Open economy (Paper-08)
  val OeEnabled: Boolean = sys.env.get("OPEN_ECON").map(_.trim.toLowerCase) match
    case Some("true" | "enabled" | "on" | "1") => true
    case _ => false

  val OeImportContent: Vector[Double] = sys.env.get("OE_IMPORT_CONTENT") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 6, s"OE_IMPORT_CONTENT must have 6 values, got ${v.length}")
      v
    case _ => Vector(0.15, 0.50, 0.20, 0.15, 0.05, 0.12) // BPO, Mfg, Ret, Hlt, Pub, Agr

  val OeErFloor: Double   = sys.env.get("OE_ER_FLOOR").map(_.trim.toDouble).getOrElse(2.5)
  val OeErCeiling: Double = sys.env.get("OE_ER_CEILING").map(_.trim.toDouble).getOrElse(10.0)

  // Export base for open economy.  Default 475M balances initial trade when
  // intermediate imports (~200M) are added to the existing consumption imports (~275M).
  // The legacy ExportBase (190M) was calibrated without intermediate imports.
  val OeExportBase: Double = sys.env.get("OE_EXPORT_BASE").map(_.trim.toDouble)
    .getOrElse(475000000.0) * ScaleFactor

  // Import-push inflation cap (monthly).  Prevents runaway ER→inflation→ER spiral.
  // At 3% cap, even a 100% ER deviation contributes at most 3% monthly inflation.
  val OeImportPushCap: Double = 0.03

  // Hardcoded calibration (NBP/GUS/WIOD)
  val OeForeignGdpGrowth      = 0.015   // EZ annual real GDP growth
  val OeExportPriceElasticity  = 0.8     // Marshall-Lerner
  val OeImportPriceElasticity  = 0.6
  val OeErElasticity           = 0.5     // ER pass-through to import prices
  val OeUlcExportBoost         = 0.15    // automation → ULC → export competitiveness
  val OeNfaReturnRate          = 0.03    // annual return on NFA
  val OeEuTransfers            = 5000000.0 * ScaleFactor  // monthly EU structural funds
  val OeFdiBase                = 2000000.0 * ScaleFactor  // baseline FDI
  val OePortfolioSensitivity   = 0.20    // rate differential → portfolio flows
  val OeRiskPremiumSensitivity = 0.10    // NFA/GDP → risk premium on ER

  // GPW Equity Market (v4.0)
  val GpwEnabled: Boolean = sys.env.get("GPW_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val GpwInitIndex: Double = sys.env.get("GPW_INIT_INDEX").map(_.trim.toDouble).getOrElse(2400.0)
  val GpwInitMcap: Double = sys.env.get("GPW_INIT_MCAP").map(_.trim.toDouble).getOrElse(1.4e12) * ScaleFactor
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

  // Real Estate / Housing Market (v4.0 Tier 2)
  val ReEnabled: Boolean = sys.env.get("RE_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val ReMortgage: Boolean = sys.env.get("RE_MORTGAGE").map(_.trim.toBoolean).getOrElse(true)
  val ReHhHousing: Boolean = sys.env.get("RE_HH_HOUSING").map(_.trim.toBoolean).getOrElse(true)
  val ReInitHpi: Double = sys.env.get("RE_INIT_HPI").map(_.trim.toDouble).getOrElse(100.0)
  val ReInitValue: Double = sys.env.get("RE_INIT_VALUE").map(_.trim.toDouble).getOrElse(3.0e12) * ScaleFactor
  val ReInitMortgage: Double = sys.env.get("RE_INIT_MORTGAGE").map(_.trim.toDouble).getOrElse(485e9) * ScaleFactor
  val RePriceIncomeElast: Double = sys.env.get("RE_PRICE_INCOME_ELAST").map(_.trim.toDouble).getOrElse(1.2)
  val RePriceRateElast: Double = sys.env.get("RE_PRICE_RATE_ELAST").map(_.trim.toDouble).getOrElse(-0.8)
  val RePriceReversion: Double = sys.env.get("RE_PRICE_REVERSION").map(_.trim.toDouble).getOrElse(0.05)
  val ReMortgageSpread: Double = sys.env.get("RE_MORTGAGE_SPREAD").map(_.trim.toDouble).getOrElse(0.025)
  val ReMortgageMaturity: Int = sys.env.get("RE_MORTGAGE_MATURITY").map(_.trim.toInt).getOrElse(300)
  val ReLtvMax: Double = sys.env.get("RE_LTV_MAX").map(_.trim.toDouble).getOrElse(0.80)
  val ReOriginationRate: Double = sys.env.get("RE_ORIGINATION_RATE").map(_.trim.toDouble).getOrElse(0.003)
  val ReDefaultBase: Double = sys.env.get("RE_DEFAULT_BASE").map(_.trim.toDouble).getOrElse(0.001)
  val ReDefaultUnempSens: Double = sys.env.get("RE_DEFAULT_UNEMP_SENS").map(_.trim.toDouble).getOrElse(0.05)
  val ReMortgageRecovery: Double = sys.env.get("RE_MORTGAGE_RECOVERY").map(_.trim.toDouble).getOrElse(0.70)
  val ReWealthMpc: Double = sys.env.get("RE_WEALTH_MPC").map(_.trim.toDouble).getOrElse(0.05)
  val ReRentalYield: Double = sys.env.get("RE_RENTAL_YIELD").map(_.trim.toDouble).getOrElse(0.045)

  // Regional Housing Market (v5.0 #21)
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

  // GVC / Deep External Sector (v5.0)
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
    case _ => Set.empty
  val GvcDisruptionRecovery: Double = sys.env.get("GVC_DISRUPTION_RECOVERY").map(_.trim.toDouble).getOrElse(0.05)

  // Sectoral labor mobility (v5.0)
  val LmSectoralMobility: Boolean = sys.env.get("LM_SECTORAL_MOBILITY").map(_.trim.toBoolean).getOrElse(false)
  val LmFrictionMatrix: Vector[Vector[Double]] = sys.env.get("LM_TRANSITION_MATRIX") match
    case Some(s) if s.nonEmpty =>
      val rows = s.split(";").map(_.split(",").map(_.trim.toDouble).toVector).toVector
      require(rows.length == 6 && rows.forall(_.length == 6),
        s"LM_TRANSITION_MATRIX must be 6x6, got ${rows.length} rows")
      rows
    case _ => sfc.engine.SectoralMobility.DefaultFrictionMatrix
  val LmFrictionDurationMult: Double = sys.env.get("LM_FRICTION_DURATION_MULT").map(_.trim.toDouble).getOrElse(1.0)
  val LmFrictionCostMult: Double = sys.env.get("LM_FRICTION_COST_MULT").map(_.trim.toDouble).getOrElse(0.5)
  val LmVoluntarySearchProb: Double = sys.env.get("LM_VOLUNTARY_SEARCH_PROB").map(_.trim.toDouble).getOrElse(0.02)
  val LmVoluntaryWageThreshold: Double = sys.env.get("LM_VOLUNTARY_WAGE_THRESHOLD").map(_.trim.toDouble).getOrElse(0.20)
  val LmVacancyWeight: Double = sys.env.get("LM_VACANCY_WEIGHT").map(_.trim.toDouble).getOrElse(2.0)
  val LmAdjacentFrictionMax: Double = sys.env.get("LM_ADJACENT_FRICTION_MAX").map(_.trim.toDouble).getOrElse(0.4)

  // Labor Unions (#44)
  val UnionEnabled: Boolean = sys.env.get("UNION_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val UnionDensity: Vector[Double] = sys.env.get("UNION_DENSITY") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 6, s"UNION_DENSITY must have 6 values, got ${v.length}")
      v
    case _ => Vector(0.02, 0.15, 0.03, 0.12, 0.30, 0.04)
  val UnionWagePremium: Double = sys.env.get("UNION_WAGE_PREMIUM").map(_.trim.toDouble).getOrElse(0.08)
  val UnionRigidity: Double = sys.env.get("UNION_RIGIDITY").map(_.trim.toDouble).getOrElse(0.50)

  // Forward-looking expectations (v5.0)
  val ExpEnabled: Boolean = sys.env.get("EXPECTATIONS_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val ExpLambda: Double = sys.env.get("EXPECTATIONS_LAMBDA").map(_.trim.toDouble).getOrElse(0.70)
  val ExpCredibilityInit: Double = sys.env.get("EXPECTATIONS_CREDIBILITY_INIT").map(_.trim.toDouble).getOrElse(0.80)
  val ExpCredibilitySpeed: Double = sys.env.get("EXPECTATIONS_CREDIBILITY_SPEED").map(_.trim.toDouble).getOrElse(0.05)
  val ExpCredibilityThreshold: Double = sys.env.get("EXPECTATIONS_CREDIBILITY_THRESHOLD").map(_.trim.toDouble).getOrElse(0.02)
  val ExpWagePassthrough: Double = sys.env.get("EXPECTATIONS_WAGE_PASSTHROUGH").map(_.trim.toDouble).getOrElse(0.50)
  val ExpBondSensitivity: Double = sys.env.get("EXPECTATIONS_BOND_SENSITIVITY").map(_.trim.toDouble).getOrElse(0.50)
  val NbpForwardGuidance: Boolean = sys.env.get("NBP_FORWARD_GUIDANCE").map(_.trim.toBoolean).getOrElse(false)

  // Immigration (#24)
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

  // Progressive PIT (#25)
  val PitEnabled: Boolean = sys.env.get("PIT_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val PitRate1: Double = sys.env.get("PIT_RATE_1").map(_.trim.toDouble).getOrElse(0.12)
  val PitRate2: Double = sys.env.get("PIT_RATE_2").map(_.trim.toDouble).getOrElse(0.32)
  val PitBracket1Annual: Double = sys.env.get("PIT_BRACKET_1").map(_.trim.toDouble).getOrElse(120000.0)
  val PitTaxCreditAnnual: Double = sys.env.get("PIT_TAX_CREDIT").map(_.trim.toDouble).getOrElse(3600.0)
  val PitEffectiveRate: Double = sys.env.get("PIT_EFFECTIVE_RATE").map(_.trim.toDouble).getOrElse(0.09)

  // Social Transfers: 800+ child benefit (#26)
  val Social800Enabled: Boolean = sys.env.get("SOCIAL_800_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val Social800Rate: Double = sys.env.get("SOCIAL_800_RATE").map(_.trim.toDouble).getOrElse(800.0)
  val Social800ChildrenPerHh: Double = sys.env.get("SOCIAL_800_CHILDREN").map(_.trim.toDouble).getOrElse(0.35)
  val Social800ImmigrantEligible: Boolean = sys.env.get("SOCIAL_800_IMMIGRANT").map(_.trim.toBoolean).getOrElse(true)

  // Consumer Credit
  val CcSpread: Double = sys.env.get("CC_SPREAD").map(_.trim.toDouble).getOrElse(0.04)
  val CcMaxDti: Double = sys.env.get("CC_MAX_DTI").map(_.trim.toDouble).getOrElse(0.40)
  val CcMaxLoan: Double = sys.env.get("CC_MAX_LOAN").map(_.trim.toDouble).getOrElse(50000.0)
  val CcAmortRate: Double = sys.env.get("CC_AMORT_RATE").map(_.trim.toDouble).getOrElse(0.025)
  val CcNplRecovery: Double = sys.env.get("CC_NPL_RECOVERY").map(_.trim.toDouble).getOrElse(0.15)
  val CcEligRate: Double = sys.env.get("CC_ELIG_RATE").map(_.trim.toDouble).getOrElse(0.30)

  // Education / Human Capital (#34) — always-on, no master toggle
  val EduShares: Vector[Double] = sys.env.get("EDU_SHARES") match
    case Some(s) if s.nonEmpty =>
      val v = s.split(",").map(_.trim.toDouble).toVector
      require(v.length == 4 && Math.abs(v.sum - 1.0) < 0.01,
        s"EDU_SHARES must have 4 values summing to ~1.0, got ${v.length} summing to ${v.sum}")
      v
    case _ => Vector(0.08, 0.25, 0.30, 0.37) // GUS LFS 2024 (25-64): Primary, Vocational, Secondary, Tertiary
  val EduSectorShares: Option[Vector[Vector[Double]]] = sys.env.get("EDU_SECTOR_SHARES") match
    case Some(s) if s.nonEmpty =>
      val vals = s.split(",").map(_.trim.toDouble).toVector
      require(vals.length == 24, s"EDU_SECTOR_SHARES must have 24 values (6 sectors × 4 edu levels), got ${vals.length}")
      val rows = (0 until 6).map(i => vals.slice(i * 4, i * 4 + 4)).toVector
      rows.zipWithIndex.foreach { (row, i) =>
        require(Math.abs(row.sum - 1.0) < 0.01,
          s"EDU_SECTOR_SHARES row $i must sum to ~1.0, got ${row.sum}")
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
      require(v.length == 4 && Math.abs(v.sum - 1.0) < 0.01,
        s"EDU_IMMIG_SHARES must have 4 values summing to ~1.0, got ${v.length} summing to ${v.sum}")
      v
    case _ => Vector(0.15, 0.40, 0.35, 0.10) // NBP 2023: immigrant edu distribution

  // Default sector-education distribution (GUS LFS 2024)
  private val DefaultEduSectorShares: Vector[Vector[Double]] = Vector(
    Vector(0.02, 0.10, 0.28, 0.60), // BPO
    Vector(0.08, 0.40, 0.32, 0.20), // Manufacturing
    Vector(0.06, 0.22, 0.38, 0.34), // Retail
    Vector(0.02, 0.15, 0.23, 0.60), // Healthcare
    Vector(0.03, 0.08, 0.25, 0.64), // Public
    Vector(0.15, 0.45, 0.30, 0.10)  // Agriculture
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

  // Staged Digitalization (#37) — always-on, no master toggle
  val DigiDrift: Double = sys.env.get("DIGI_DRIFT").map(_.trim.toDouble).getOrElse(0.001)
  val DigiInvestCost: Double = sys.env.get("DIGI_INVEST_COST").map(_.trim.toDouble).getOrElse(50000.0)
  val DigiInvestBoost: Double = sys.env.get("DIGI_INVEST_BOOST").map(_.trim.toDouble).getOrElse(0.05)
  val DigiCapexDiscount: Double = sys.env.get("DIGI_CAPEX_DISCOUNT").map(_.trim.toDouble).getOrElse(0.30)
  val DigiInvestBaseProb: Double = sys.env.get("DIGI_INVEST_BASE_PROB").map(_.trim.toDouble).getOrElse(0.08)

  // Physical Capital & Depreciation (#31) — always-on, no master toggle
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

  // Heterogeneous households (Paper-06)
  val HhCount = sys.env.get("HH_COUNT").map(_.trim.toInt).getOrElse(TotalPopulation)

  // Savings distribution (GUS BBGD 2023): LogNormal(mu, sigma) → median ~15K PLN
  val HhSavingsMu       = sys.env.get("HH_SAVINGS_MU").map(_.trim.toDouble).getOrElse(9.6)
  val HhSavingsSigma    = 1.2
  // Debt: 40% of households have debt; among those, LogNormal
  val HhDebtFraction    = 0.40
  val HhDebtMu          = 10.5     // median ~36K PLN for indebted
  val HhDebtSigma       = 1.5
  // Rent: Normal(mean, std), floor at 800 PLN/month
  val HhRentMean        = 1800.0
  val HhRentStd         = 400.0
  val HhRentFloor       = 800.0
  // MPC: Beta(alpha, beta) → mean ~0.82
  val HhMpcAlpha        = sys.env.get("HH_MPC_ALPHA").map(_.trim.toDouble).getOrElse(8.2)
  val HhMpcBeta         = sys.env.get("HH_MPC_BETA").map(_.trim.toDouble).getOrElse(1.8)
  // Skill decay and health scarring
  val HhSkillDecayRate  = 0.02     // per month after onset
  val HhScarringRate    = 0.02     // health penalty per month after onset
  val HhScarringCap     = 0.50
  val HhScarringOnset   = 3        // months before scarring/skill decay starts
  // Retraining
  val HhRetrainingCost  = sys.env.get("HH_RETRAIN_COST").map(_.trim.toDouble).getOrElse(5000.0)
  val HhRetrainingDuration = sys.env.get("HH_RETRAIN_DUR").map(_.trim.toInt).getOrElse(6)
  val HhRetrainingBaseSuccess = 0.60
  val HhRetrainingProb  = sys.env.get("HH_RETRAIN_PROB").map(_.trim.toDouble).getOrElse(0.15)
  val HhRetrainingEnabled = sys.env.get("HH_RETRAIN_ENABLED").map(_.trim.toBoolean).getOrElse(true)
  // Bankruptcy
  val HhBankruptcyThreshold = -3.0 // multiplied by monthlyRent
  // Social network (household-level WS)
  val HhSocialK         = 10
  val HhSocialP         = 0.15
  // Debt service
  val HhDebtServiceRate = 0.02     // monthly (2% of outstanding) — aggregate mode fallback
  val HhBaseAmortRate   = 0.015    // monthly principal amortization (individual mode)
  val HhDepositSpread   = 0.02     // annual: deposit rate = max(0, refRate - spread)

