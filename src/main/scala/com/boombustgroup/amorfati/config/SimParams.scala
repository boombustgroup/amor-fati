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
    share: Share,                // Share of firm population (GUS BAEL 2024)
    sigma: Sigma,                // CES elasticity of substitution
    wageMultiplier: Multiplier,  // Sector wage multiplier vs national average
    revenueMultiplier: Multiplier,
    aiCapexMultiplier: Multiplier,
    hybridCapexMultiplier: Multiplier,
    baseDigitalReadiness: Share, // Central tendency of digitalReadiness
    hybridRetainFrac: Share,     // Fraction of workers RETAINED in hybrid mode (0.5 = halve)
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
    gdpRatio: Double = SimParams.DefaultGdpRatio, // scaling coefficient — computation boundary, not SFC flow
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
    SectorDef("BPO/SSC", Share(0.03), Sigma(50.0), Multiplier(1.35), Multiplier(1.50), Multiplier(0.70), Multiplier(0.70), Share(0.50), Share(0.50)),
    SectorDef("Manufacturing", Share(0.16), Sigma(10.0), Multiplier(0.94), Multiplier(1.05), Multiplier(1.12), Multiplier(1.05), Share(0.45), Share(0.60)),
    SectorDef("Retail/Services", Share(0.45), Sigma(5.0), Multiplier(0.79), Multiplier(0.91), Multiplier(0.85), Multiplier(0.80), Share(0.40), Share(0.65)),
    SectorDef("Healthcare", Share(0.06), Sigma(2.0), Multiplier(0.97), Multiplier(1.10), Multiplier(1.38), Multiplier(1.25), Share(0.25), Share(0.75)),
    SectorDef("Public", Share(0.22), Sigma(1.0), Multiplier(0.91), Multiplier(1.08), Multiplier(3.00), Multiplier(2.50), Share(0.08), Share(0.90)),
    SectorDef("Agriculture", Share(0.08), Sigma(3.0), Multiplier(0.67), Multiplier(0.80), Multiplier(2.50), Multiplier(2.00), Share(0.12), Share(0.85)),
  )

  // ── GdpRatio computation ──

  /** Compute the scaling factor that maps agent-level monthly flows to real
    * Polish GDP.
    *
    * Formula:
    * `(firmsCount * avgWorkers / workersPerFirm * baseRevenue * 12) / realGdp`
    */
  @boundaryEscape
  def computeGdpRatio(pop: PopulationConfig, baseRevenue: PLN): Double =
    import ComputationBoundary.toDouble
    val expectedAvgWorkers = pop.firmSizeDist match
      case FirmSizeDist.Gus     =>
        val microMean   = 5.0; val smallMean = 29.5; val mediumMean = 149.5
        val largeMean   = (250.0 + pop.firmSizeLargeMax.toDouble) / 2.0
        val mediumShare =
          1.0 - toDouble(pop.firmSizeMicroShare) - toDouble(pop.firmSizeSmallShare) - toDouble(pop.firmSizeLargeShare)
        toDouble(pop.firmSizeMicroShare) * microMean + toDouble(pop.firmSizeSmallShare) * smallMean +
          mediumShare * mediumMean + toDouble(pop.firmSizeLargeShare) * largeMean
      case FirmSizeDist.Uniform => pop.workersPerFirm.toDouble
    (pop.firmsCount.toDouble * expectedAvgWorkers / pop.workersPerFirm.toDouble * toDouble(baseRevenue) * 12.0) / toDouble(pop.realGdp)

  private val DefaultGdpRatio: Double = computeGdpRatio(PopulationConfig(), FirmConfig().baseRevenue)

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
        govBaseSpending = PLN(58.3e9 * r),
        initGovDebt = PLN(1600e9 * r),
      ),
      monetary = MonetaryConfig(
        qePace = PLN(5e9 * r),
        fxReserves = PLN(185e9 * r),
      ),
      banking = BankingConfig(
        initCapital = PLN(270e9 * r),
        initDeposits = PLN(1900e9 * r),
        initLoans = PLN(700e9 * r),
        initGovBonds = PLN(400e9 * r),
        initNbpGovBonds = PLN(300e9 * r),
        initConsumerLoans = PLN(200e9 * r),
      ),
      forex = ForexConfig(
        exportBase = PLN(55.4e9 * r),
      ),
      openEcon = OpenEconConfig(
        exportBase = PLN(138.5e9 * r),
        euTransfers = PLN(1.458e9 * r),
        fdiBase = PLN(583.1e6 * r),
      ),
      equity = EquityConfig(
        initMcap = PLN(1.4e12 * r),
      ),
      corpBond = CorpBondConfig(
        initStock = PLN(90e9 * r),
      ),
      ins = InsuranceConfig(
        lifeReserves = PLN(110e9 * r),
        nonLifeReserves = PLN(90e9 * r),
      ),
      nbfi = NbfiConfig(
        tfiInitAum = PLN(380e9 * r),
        creditInitStock = PLN(231e9 * r),
      ),
      housing = HousingConfig(
        initValue = PLN(3.0e12 * r),
        initMortgage = PLN(485e9 * r),
      ),
      social = SocialConfig(demInitialRetirees = 0),
      gdpRatio = r,
    )

/** Firm size distribution: stratified draw from GUS 2024 Polish enterprise
  * data.
  */
object FirmSizeDistribution:
  import scala.util.Random

  @boundaryEscape
  def draw(rng: Random)(using p: SimParams): Int =
    import ComputationBoundary.toDouble
    p.pop.firmSizeDist match
      case FirmSizeDist.Gus     =>
        val r     = rng.nextDouble()
        val micro = toDouble(p.pop.firmSizeMicroShare)
        val small = toDouble(p.pop.firmSizeSmallShare)
        val large = toDouble(p.pop.firmSizeLargeShare)
        if r < micro then rng.between(1, 10)
        else if r < micro + small then rng.between(10, 50)
        else if r < 1.0 - large then rng.between(50, 250)
        else rng.between(250, p.pop.firmSizeLargeMax + 1)
      case FirmSizeDist.Uniform => p.pop.workersPerFirm
