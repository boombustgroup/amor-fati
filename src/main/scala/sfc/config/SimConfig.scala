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

object Config:
  val FirmsCount       = sys.env.get("FIRMS_COUNT").map(_.trim.toInt).getOrElse(10000)
  val WorkersPerFirm   = 10
  val TotalPopulation  = FirmsCount * WorkersPerFirm
  private val ScaleFactor = FirmsCount.toDouble / 10000.0
  val Duration         = 120
  val ShockMonth       = 30

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
  val VatRate          = 0.23
  val GovBaseSpending  = 100000000.0 * ScaleFactor

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

  // Multi-bank (Phase 4)
  val BankMulti: Boolean = sys.env.get("BANK_MODE").map(_.trim.toLowerCase).getOrElse("single") == "multi"
  val BankFailureEnabled: Boolean = sys.env.get("BANK_FAILURE").map(_.trim.toBoolean).getOrElse(false)
  val BankReserveReq: Double = sys.env.get("BANK_RESERVE_REQ").map(_.trim.toDouble).getOrElse(0.035)
  val BankStressThreshold: Double = sys.env.get("BANK_STRESS_THRESHOLD").map(_.trim.toDouble).getOrElse(0.05)

  // Credit diagnostics (Phase 7A): M1/M2 monetary aggregates
  val CreditDiagnostics: Boolean = sys.env.get("CREDIT_DIAGNOSTICS").map(_.trim.toBoolean).getOrElse(false)

  // JST / Samorządy (Phase 7B): two-tier fiscal system
  val JstEnabled: Boolean = sys.env.get("JST_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val JstPitShare: Double = sys.env.get("JST_PIT_SHARE").map(_.trim.toDouble).getOrElse(0.3846)    // Ustawa o dochodach JST, Art. 4
  val JstCitShare: Double = sys.env.get("JST_CIT_SHARE").map(_.trim.toDouble).getOrElse(0.0671)    // j.w.
  val JstPropertyTax: Double = sys.env.get("JST_PROPERTY_TAX").map(_.trim.toDouble).getOrElse(5000.0) // PLN/firm/year (GUS 2023)
  val JstSubventionShare: Double = sys.env.get("JST_SUBVENTION_SHARE").map(_.trim.toDouble).getOrElse(0.03)  // ~3% GDP
  val JstDotacjeShare: Double = sys.env.get("JST_DOTACJE_SHARE").map(_.trim.toDouble).getOrElse(0.01)  // ~1% GDP
  val JstSpendingMult: Double = sys.env.get("JST_SPENDING_MULT").map(_.trim.toDouble).getOrElse(1.02)  // ~2% deficit ratio

  // LCR/NSFR (Phase 7C): maturity mismatch + liquidity ratios
  val BankLcrEnabled: Boolean = sys.env.get("BANK_LCR_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val BankLcrMin: Double = sys.env.get("BANK_LCR_MIN").map(_.trim.toDouble).getOrElse(1.0)      // Basel III minimum 100%
  val BankNsfrMin: Double = sys.env.get("BANK_NSFR_MIN").map(_.trim.toDouble).getOrElse(1.0)    // Basel III minimum 100%
  val BankDemandDepositRunoff: Double = sys.env.get("BANK_DEMAND_DEPOSIT_RUNOFF").map(_.trim.toDouble).getOrElse(0.10)  // Basel III retail
  val BankTermDepositFrac: Double = sys.env.get("BANK_TERM_DEPOSIT_FRAC").map(_.trim.toDouble).getOrElse(0.40)

  // Interbank term structure (Phase 8A)
  val InterbankTermStructure: Boolean = sys.env.get("INTERBANK_TERM_STRUCTURE").map(_.trim.toBoolean).getOrElse(false)

  // ZUS / Social Insurance (Phase 8B)
  val ZusEnabled: Boolean = sys.env.get("ZUS_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val ZusContribRate: Double = sys.env.get("ZUS_CONTRIB_RATE").map(_.trim.toDouble).getOrElse(0.1952)  // pension portion
  val ZusBasePension: Double = sys.env.get("ZUS_BASE_PENSION").map(_.trim.toDouble).getOrElse(3500.0)  // PLN/month after waloryzacja
  val ZusScale: Double = sys.env.get("ZUS_SCALE").map(_.trim.toDouble).getOrElse(1.0)  // gradual introduction

  // PPK / Capital Pension (Phase 8B)
  val PpkEnabled: Boolean = sys.env.get("PPK_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val PpkEmployeeRate: Double = sys.env.get("PPK_EMPLOYEE_RATE").map(_.trim.toDouble).getOrElse(0.02)  // 2%
  val PpkEmployerRate: Double = sys.env.get("PPK_EMPLOYER_RATE").map(_.trim.toDouble).getOrElse(0.015) // 1.5%
  val PpkBondAlloc: Double = sys.env.get("PPK_BOND_ALLOC").map(_.trim.toDouble).getOrElse(0.60)  // default TFI strategy

  // Demographics (Phase 8B)
  val DemEnabled: Boolean = sys.env.get("DEMOGRAPHICS_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val DemRetirementRate: Double = sys.env.get("DEM_RETIREMENT_RATE").map(_.trim.toDouble).getOrElse(0.001)  // 0.1%/month
  val DemWorkingAgeDecline: Double = sys.env.get("DEM_WORKING_AGE_DECLINE").map(_.trim.toDouble).getOrElse(0.002)  // 0.2%/year
  val DemInitialRetirees: Int = sys.env.get("DEM_INITIAL_RETIREES").map(_.trim.toInt).getOrElse(0)

  // Macroprudential (Phase 8C): KNF/CCyB/OSII
  val MacropruEnabled: Boolean = sys.env.get("MACROPRU_ENABLED").map(_.trim.toBoolean).getOrElse(false)
  val CcybMax: Double = sys.env.get("CCYB_MAX").map(_.trim.toDouble).getOrElse(0.025)  // 2.5% max buffer
  val CcybActivationGap: Double = sys.env.get("CCYB_ACTIVATION_GAP").map(_.trim.toDouble).getOrElse(0.02)  // >2pp gap → build
  val CcybReleaseGap: Double = sys.env.get("CCYB_RELEASE_GAP").map(_.trim.toDouble).getOrElse(-0.02)  // <-2pp gap → release
  val OsiiPkoBp: Double = sys.env.get("OSII_PKO_BP").map(_.trim.toDouble).getOrElse(0.01)  // 1.0% for PKO BP
  val OsiiPekao: Double = sys.env.get("OSII_PEKAO").map(_.trim.toDouble).getOrElse(0.005)  // 0.5% for Pekao
  val ConcentrationLimit: Double = sys.env.get("CONCENTRATION_LIMIT").map(_.trim.toDouble).getOrElse(0.25)  // Art. 395 CRR

  // Reserve interest (Phase 6A): NBP pays fraction of refRate on required reserves
  // Uchwała RPP nr 7/2003: oprocentowanie = 0.9 × stopa referencyjna (currently 0.5 in model for visibility)
  val NbpReserveRateMult: Double = sys.env.get("NBP_RESERVE_RATE_MULT").map(_.trim.toDouble).getOrElse(0.5)

  // Standing facilities (Phase 6B): deposit/lombard facility actual flows
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

