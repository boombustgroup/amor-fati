package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Network topology selection for comparative experiments. */
enum Topology(val label: String):
  case Ws      extends Topology("Watts-Strogatz")
  case Er      extends Topology("Erdos-Renyi")
  case Ba      extends Topology("Barabasi-Albert")
  case Lattice extends Topology("Lattice")

/** 4-to-6 sector definition with heterogeneous sigma (CES elasticity of
  * substitution). sigma affects: decision threshold, automation efficiency,
  * CAPEX costs.
  */
case class SectorDef(
    name: String,
    share: Ratio,                // Share of firm population (GUS BAEL 2024)
    sigma: Double,               // CES elasticity of substitution
    wageMultiplier: Double,      // Sector wage multiplier vs national average
    revenueMultiplier: Double,
    aiCapexMultiplier: Double,
    hybridCapexMultiplier: Double,
    baseDigitalReadiness: Ratio, // Central tendency of digitalReadiness
    hybridRetainFrac: Ratio,     // Fraction of workers RETAINED in hybrid mode (0.5 = halve)
)

/** Complete parameterization of a 48-mechanism SFC-ABM model of the Polish
  * economy.
  *
  * Hierarchical, immutable, and testable configuration threaded via Scala 3
  * `using` context parameters. Constructor is private — use
  * `SimParams.defaults`.
  *
  * Sub-configs are grouped by economic domain:
  *   - `flags` — 50 mechanism toggles
  *   - `pop`, `timeline` — simulation structure
  *   - `firm`, `household` — agent parameters
  *   - `fiscal`, `monetary`, `banking` — government, central bank, commercial
  *     banks
  *   - `forex`, `openEcon`, `fdi`, `immigration`, `remittance`, `tourism`,
  *     `gvc` — external sector
  *   - `equity`, `corpBond`, `ins`, `nbfi`, `housing` — financial markets
  *   - `social`, `io`, `labor`, `capital`, `climate`, `informal` — structural
  *     mechanisms
  *   - `sectorDefs`, `topology`, `gdpRatio` — simulation infrastructure
  *
  * Stock values in sub-configs use raw PLN amounts. `SimParams.defaults`
  * applies `gdpRatio` scaling so that agent-level flows map correctly to real
  * Polish GDP (~3.5 bln PLN). Do NOT construct SimParams directly with unscaled
  * values — always start from `defaults` and use `.copy()`.
  */
@annotation.nowarn("msg=unused private member") // Scala 3.8 false positive: defaults used via copy()
case class SimParams private (
    flags: FeatureFlags = FeatureFlags(),
    pop: PopulationConfig = PopulationConfig(),
    timeline: TimelineConfig = TimelineConfig(),
    firm: FirmConfig = FirmConfig(),
    household: HouseholdConfig = HouseholdConfig(),
    fiscal: FiscalConfig = FiscalConfig(),
    monetary: MonetaryConfig = MonetaryConfig(),
    banking: BankingConfig = BankingConfig(),
    forex: ForexConfig = ForexConfig(),
    openEcon: OpenEconConfig = OpenEconConfig(),
    fdi: FdiConfig = FdiConfig(),
    immigration: ImmigrationConfig = ImmigrationConfig(),
    remittance: RemittanceConfig = RemittanceConfig(),
    tourism: TourismConfig = TourismConfig(),
    gvc: GvcConfig = GvcConfig(),
    equity: EquityConfig = EquityConfig(),
    corpBond: CorpBondConfig = CorpBondConfig(),
    quasiFiscal: QuasiFiscalConfig = QuasiFiscalConfig(),
    ins: InsuranceConfig = InsuranceConfig(),
    nbfi: NbfiConfig = NbfiConfig(),
    housing: HousingConfig = HousingConfig(),
    social: SocialConfig = SocialConfig(),
    earmarked: EarmarkedConfig = EarmarkedConfig(),
    soe: SoeConfig = SoeConfig(),
    pricing: PricingConfig = PricingConfig(),
    regional: RegionalConfig = RegionalConfig(),
    io: IoConfig = IoConfig(),
    labor: LaborConfig = LaborConfig(),
    capital: CapitalConfig = CapitalConfig(),
    climate: ClimateConfig = ClimateConfig(),
    informal: InformalConfig = InformalConfig(),
    sectorDefs: Vector[SectorDef] = SimParams.DefaultSectorDefs,
    topology: Topology = Topology.Ws,
    gdpRatio: Double = SimParams.DefaultGdpRatio,
)

object SimParams:

  // ── Sector definitions (6-sector Polish economy, GUS 2024) ──

  import com.boombustgroup.amorfati.types.*

  /** Default 6-sector definitions calibrated to GUS 2024.
    *
    * Sectors: BPO/SSC, Manufacturing, Retail/Services, Healthcare, Public,
    * Agriculture. Each sector has: employment share, revenue multiplier, wage
    * multiplier, cost multiplier, capital intensity, automation cost
    * multiplier, export propensity, and import propensity.
    */
  val DefaultSectorDefs: Vector[SectorDef] = Vector(
    SectorDef("BPO/SSC", Ratio(0.03), 50.0, 1.35, 1.50, 0.70, 0.70, Ratio(0.50), Ratio(0.50)),
    SectorDef("Manufacturing", Ratio(0.16), 10.0, 0.94, 1.05, 1.12, 1.05, Ratio(0.45), Ratio(0.60)),
    SectorDef("Retail/Services", Ratio(0.45), 5.0, 0.79, 0.91, 0.85, 0.80, Ratio(0.40), Ratio(0.65)),
    SectorDef("Healthcare", Ratio(0.06), 2.0, 0.97, 1.10, 1.38, 1.25, Ratio(0.25), Ratio(0.75)),
    SectorDef("Public", Ratio(0.22), 1.0, 0.91, 1.08, 3.00, 2.50, Ratio(0.08), Ratio(0.90)),
    SectorDef("Agriculture", Ratio(0.08), 3.0, 0.67, 0.80, 2.50, 2.00, Ratio(0.12), Ratio(0.85)),
  )

  // ── GdpRatio computation ──

  /** Compute the scaling factor that maps agent-level monthly flows to real
    * Polish GDP.
    *
    * Formula:
    * `(firmsCount * avgWorkers / workersPerFirm * baseRevenue * 12) / realGdp`
    */
  def computeGdpRatio(pop: PopulationConfig, baseRevenue: Double): Double =
    val expectedAvgWorkers = pop.firmSizeDist match
      case FirmSizeDist.Gus     =>
        val microMean   = 5.0; val smallMean = 29.5; val mediumMean = 149.5
        val largeMean   = (250.0 + pop.firmSizeLargeMax.toDouble) / 2.0
        val mediumShare =
          1.0 - pop.firmSizeMicroShare.toDouble - pop.firmSizeSmallShare.toDouble - pop.firmSizeLargeShare.toDouble
        pop.firmSizeMicroShare.toDouble * microMean + pop.firmSizeSmallShare.toDouble * smallMean +
          mediumShare * mediumMean + pop.firmSizeLargeShare.toDouble * largeMean
      case FirmSizeDist.Uniform => pop.workersPerFirm.toDouble
    (pop.firmsCount.toDouble * expectedAvgWorkers / pop.workersPerFirm.toDouble * baseRevenue * 12.0) / pop.realGdp.toDouble

  private val DefaultGdpRatio: Double = computeGdpRatio(PopulationConfig(), FirmConfig().baseRevenue.toDouble)

  /** All hardcoded defaults with gdpRatio-scaled stock variables.
    *
    * Hardcoded calibration defaults (no env vars). Stock values (bank balance
    * sheets, government debt, market caps, reserves, etc.) are scaled by
    * `gdpRatio` to map agent-level flows to real Polish GDP.
    */
  val defaults: SimParams =
    val pop      = PopulationConfig()
    val firm     = FirmConfig()
    val r        = DefaultGdpRatio
    val totalPop = pop.firmsCount * pop.workersPerFirm

    SimParams(
      pop = pop,
      firm = firm,
      household = HouseholdConfig(count = totalPop),
      fiscal = FiscalConfig(
        govBaseSpending = PLN(58.3e9) * r,
        initGovDebt = PLN(1600e9) * r,
      ),
      monetary = MonetaryConfig(
        qePace = PLN(5e9) * r,
        fxReserves = PLN(185e9) * r,
      ),
      banking = BankingConfig(
        initCapital = PLN(270e9) * r,
        initDeposits = PLN(1900e9) * r,
        initLoans = PLN(700e9) * r,
        initGovBonds = PLN(400e9) * r,
        initNbpGovBonds = PLN(300e9) * r,
        initConsumerLoans = PLN(200e9) * r,
      ),
      forex = ForexConfig(
        exportBase = PLN(55.4e9) * r,
      ),
      openEcon = OpenEconConfig(
        exportBase = PLN(138.5e9) * r,
        euTransfers = PLN(1.458e9) * r,
        fdiBase = PLN(583.1e6) * r,
      ),
      equity = EquityConfig(
        initMcap = PLN(1.4e12) * r,
      ),
      corpBond = CorpBondConfig(
        initStock = PLN(90e9) * r,
      ),
      ins = InsuranceConfig(
        lifeReserves = PLN(110e9) * r,
        nonLifeReserves = PLN(90e9) * r,
      ),
      nbfi = NbfiConfig(
        tfiInitAum = PLN(380e9) * r,
        creditInitStock = PLN(231e9) * r,
      ),
      housing = HousingConfig(
        initValue = PLN(3.0e12) * r,
        initMortgage = PLN(485e9) * r,
      ),
      social = SocialConfig(demInitialRetirees = 0),
      gdpRatio = r,
    )

/** Firm size distribution: stratified draw from GUS 2024 Polish enterprise
  * data.
  */
object FirmSizeDistribution:
  import scala.util.Random

  def draw(rng: Random)(using p: SimParams): Int = p.pop.firmSizeDist match
    case FirmSizeDist.Gus     =>
      val r = rng.nextDouble()
      if r < p.pop.firmSizeMicroShare.toDouble then rng.between(1, 10)
      else if r < p.pop.firmSizeMicroShare.toDouble + p.pop.firmSizeSmallShare.toDouble then rng.between(10, 50)
      else if r < 1.0 - p.pop.firmSizeLargeShare.toDouble then rng.between(50, 250)
      else rng.between(250, p.pop.firmSizeLargeMax + 1)
    case FirmSizeDist.Uniform => p.pop.workersPerFirm
