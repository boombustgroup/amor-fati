package sfc.config

/** Monetary regime: determines central bank behavior and exchange rate dynamics. */
enum MonetaryRegime:
  case Pln   // NBP Taylor rule + floating PLN/EUR
  case Eur   // Exogenous ECB rate + fixed exchange rate (Eurozone membership)

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
val SECTORS: Vector[SectorDef] = Vector(
  //                             share   sigma wage  rev   aiCpx hybCpx digiR  hybRet
  SectorDef("BPO/SSC",          0.03, 50.0, 1.35, 1.50, 0.70, 0.70,  0.50,  0.50),  // ~489k workers (ABSL), avg 11 154 PLN
  SectorDef("Manufacturing",    0.16, 10.0, 0.94, 1.05, 1.12, 1.05,  0.45,  0.60),  // ~2.8M workers, avg ~7 800 PLN
  SectorDef("Retail/Services",  0.45,  5.0, 0.79, 0.91, 0.85, 0.80,  0.40,  0.65),  // ~61% employment (services), avg ~6 500 PLN
  SectorDef("Healthcare",       0.06,  2.0, 0.97, 1.10, 1.38, 1.25,  0.25,  0.75),  // ~5.5%, nurses 6 890, doctors 16 300
  SectorDef("Public",           0.22,  1.0, 0.91, 1.08, 3.00, 2.50,  0.08,  0.90),  // ~22% employment (public sector), avg ~7 500 PLN
  SectorDef("Agriculture",      0.08,  3.0, 0.67, 0.80, 2.50, 2.00,  0.12,  0.85)   // ~8% BAEL, avg ~5 500 PLN
)

object Config:
  val FirmsCount       = 10000
  val WorkersPerFirm   = 10
  val TotalPopulation  = FirmsCount * WorkersPerFirm
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
  val GovBaseSpending  = 100000000.0

  // NBP (NBP data 2024)
  val NbpInitialRate   = 0.0575      // NBP reference rate 2024
  val NbpTargetInfl    = 0.025       // NBP target 2.5% +/- 1pp
  val NbpNeutralRate   = 0.04        // NBP neutral rate
  val TaylorAlpha      = 1.5
  val TaylorBeta       = 0.8
  val TaylorInertia    = 0.70
  val RateFloor        = 0.005
  val RateCeiling      = 0.25

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

  // Banking system
  val InitBankCapital  = 500000000.0
  val BaseSpread       = 0.015       // NBP MIR corporate spread 2024
  val NplSpreadFactor  = 5.0
  val MinCar           = 0.08
  val LoanRecovery     = 0.30

  // Foreign sector (NBP/ECB 2024)
  val BaseExRate       = 4.33        // NBP average PLN/EUR rate 2024
  val ForeignRate      = 0.04        // ECB rate 2024
  val ImportPropensity = 0.40
  val ExportBase       = 190000000.0
  val TechImportShare  = 0.40
  val IrpSensitivity   = 0.15
  val ExRateAdjSpeed   = 0.02
  val ExportAutoBoost  = 0.15

  // Demand
  val DemandPassthrough = 0.40

  // Network
  val NetworkK          = 6      // Watts-Strogatz: neighbors
  val NetworkRewireP    = 0.10   // Watts-Strogatz: rewire probability
  val DemoEffectThresh  = 0.40   // If >40% neighbors automated -> demonstration effect
  val DemoEffectBoost   = 0.15   // Modest boost to uncertainty discount from demonstration
