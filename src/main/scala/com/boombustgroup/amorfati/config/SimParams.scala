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
  * `using` context parameters. Constructor remains package-scoped for config
  * tests; production code should use `SimParams.defaults`.
  *
  * Sub-configs are grouped by economic domain:
  *   - `pop` — simulation structure
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
case class SimParams private[config] (
    pop: PopulationConfig = PopulationConfig(),
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
    gdpRatio: Scalar = SimParams.DefaultGdpRatio, // scaling coefficient — computation boundary, not SFC flow
):
  SimParams.validateSectorSchema(sectorDefs)

object SimParams:

  // ── Sector definitions (6-sector Polish economy, GUS 2024) ──

  import com.boombustgroup.amorfati.types.*

  private[amorfati] final case class SchemaSector(name: String, outputStem: String)

  private[amorfati] val SchemaSectors: Vector[SchemaSector] =
    Vector(
      SchemaSector("BPO/SSC", "BPO"),
      SchemaSector("Manufacturing", "Manuf"),
      SchemaSector("Retail/Services", "Retail"),
      SchemaSector("Healthcare", "Health"),
      SchemaSector("Public", "Public"),
      SchemaSector("Agriculture", "Agri"),
    )

  private[amorfati] val SchemaSectorNames: Vector[String] = SchemaSectors.map(_.name)
  private[amorfati] val SchemaSectorCount: Int            = SchemaSectors.length

  private[amorfati] def validateSectorSchema(sectorDefs: Vector[SectorDef]): Unit =
    require(
      sectorDefs.length == SchemaSectorCount,
      s"sectorDefs must have $SchemaSectorCount schema sectors in order ${SchemaSectorNames.mkString(" -> ")}, got ${sectorDefs.length}",
    )
    require(
      sectorDefs.map(_.name) == SchemaSectorNames,
      s"sectorDefs must preserve schema order ${SchemaSectorNames.mkString(" -> ")}, got ${sectorDefs.map(_.name).mkString(" -> ")}",
    )

  /** Default 6-sector definitions calibrated to GUS 2024.
    *
    * Sectors: BPO/SSC, Manufacturing, Retail/Services, Healthcare, Public,
    * Agriculture. Each sector has: employment share, revenue multiplier, wage
    * multiplier, cost multiplier, capital intensity, automation cost
    * multiplier, export propensity, and import propensity.
    */
  val DefaultSectorDefs: Vector[SectorDef] = Vector(
    SectorDef(
      "BPO/SSC",
      Share.decimal(3, 2),
      Sigma(50),
      Multiplier.decimal(135, 2),
      Multiplier.decimal(150, 2),
      Multiplier.decimal(70, 2),
      Multiplier.decimal(70, 2),
      Share.decimal(50, 2),
      Share.decimal(50, 2),
    ),
    SectorDef(
      "Manufacturing",
      Share.decimal(16, 2),
      Sigma(10),
      Multiplier.decimal(94, 2),
      Multiplier.decimal(105, 2),
      Multiplier.decimal(112, 2),
      Multiplier.decimal(105, 2),
      Share.decimal(45, 2),
      Share.decimal(60, 2),
    ),
    SectorDef(
      "Retail/Services",
      Share.decimal(45, 2),
      Sigma(5),
      Multiplier.decimal(79, 2),
      Multiplier.decimal(91, 2),
      Multiplier.decimal(85, 2),
      Multiplier.decimal(80, 2),
      Share.decimal(40, 2),
      Share.decimal(65, 2),
    ),
    SectorDef(
      "Healthcare",
      Share.decimal(6, 2),
      Sigma(2),
      Multiplier.decimal(97, 2),
      Multiplier.decimal(110, 2),
      Multiplier.decimal(138, 2),
      Multiplier.decimal(125, 2),
      Share.decimal(25, 2),
      Share.decimal(75, 2),
    ),
    SectorDef(
      "Public",
      Share.decimal(22, 2),
      Sigma(1),
      Multiplier.decimal(91, 2),
      Multiplier.decimal(108, 2),
      Multiplier(3),
      Multiplier.decimal(250, 2),
      Share.decimal(8, 2),
      Share.decimal(90, 2),
    ),
    SectorDef(
      "Agriculture",
      Share.decimal(8, 2),
      Sigma(3),
      Multiplier.decimal(67, 2),
      Multiplier.decimal(80, 2),
      Multiplier.decimal(250, 2),
      Multiplier(2),
      Share.decimal(12, 2),
      Share.decimal(85, 2),
    ),
  )

  // ── GdpRatio computation ──

  /** Compute the scaling factor that maps agent-level monthly flows to real
    * Polish GDP.
    *
    * Formula:
    * `(firmsCount * avgWorkers / workersPerFirm * baseRevenue * 12) / realGdp`
    */
  def computeGdpRatio(pop: PopulationConfig, baseRevenue: PLN): Scalar =
    val expectedAvgWorkers = pop.firmSizeDist match
      case FirmSizeDist.Gus     =>
        val microMean   = Scalar(5)
        val smallMean   = Scalar.decimal(295, 1)
        val mediumMean  = Scalar.decimal(1495, 1)
        val largeMean   = Scalar(250 + pop.firmSizeLargeMax) / 2
        val mediumShare =
          Share.One - pop.firmSizeMicroShare - pop.firmSizeSmallShare - pop.firmSizeLargeShare
        pop.firmSizeMicroShare.toScalar * microMean + pop.firmSizeSmallShare.toScalar * smallMean +
          mediumShare.toScalar * mediumMean + pop.firmSizeLargeShare.toScalar * largeMean
      case FirmSizeDist.Uniform => Scalar(pop.workersPerFirm)
    (((baseRevenue * pop.firmsCount) / pop.workersPerFirm) * expectedAvgWorkers * 12) / pop.realGdp

  private val DefaultGdpRatio: Scalar = computeGdpRatio(PopulationConfig(), FirmConfig().baseRevenue)

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
        govBaseSpending = PLN(58300000000L) * r,
        initGovDebt = PLN(1600000000000L) * r,
      ),
      monetary = MonetaryConfig(
        qePace = PLN(5000000000L) * r,
        fxReserves = PLN(185000000000L) * r,
      ),
      banking = BankingConfig(
        initCapital = PLN(270000000000L) * r,
        initDeposits = PLN(1900000000000L) * r,
        initLoans = PLN(700000000000L) * r,
        initGovBonds = PLN(400000000000L) * r,
        initNbpGovBonds = PLN(300000000000L) * r,
        initConsumerLoans = PLN(200000000000L) * r,
      ),
      forex = ForexConfig(),
      openEcon = OpenEconConfig(
        exportBase = PLN(138500000000L) * r,
        euTransfers = PLN(1458000000) * r,
        fdiBase = PLN(583100000) * r,
      ),
      equity = EquityConfig(
        initMcap = PLN(1400000000000L) * r,
      ),
      corpBond = CorpBondConfig(
        initStock = PLN(90000000000L) * r,
      ),
      ins = InsuranceConfig(
        lifeReserves = PLN(110000000000L) * r,
        nonLifeReserves = PLN(90000000000L) * r,
      ),
      nbfi = NbfiConfig(
        tfiInitAum = PLN(380000000000L) * r,
        creditInitStock = PLN(231000000000L) * r,
      ),
      housing = HousingConfig(
        initValue = PLN(3000000000000L) * r,
        initMortgage = PLN(485000000000L) * r,
      ),
      social = SocialConfig(demInitialRetirees = 0),
      gdpRatio = r,
    )

/** Firm size distribution: stratified draw from GUS 2024 Polish enterprise
  * data.
  */
object FirmSizeDistribution:
  import com.boombustgroup.amorfati.random.RandomStream

  def draw(rng: RandomStream)(using p: SimParams): Int =
    p.pop.firmSizeDist match
      case FirmSizeDist.Gus     =>
        val micro    = p.pop.firmSizeMicroShare.toLong
        val small    = p.pop.firmSizeSmallShare.toLong
        val large    = p.pop.firmSizeLargeShare.toLong
        val draw     = Share.random(rng).toLong
        val microCut = micro
        val smallCut = micro + small
        val largeCut = Share.One.toLong - large
        if draw < microCut then rng.between(1, 10)
        else if draw < smallCut then rng.between(10, 50)
        else if draw < largeCut then rng.between(50, 250)
        else rng.between(250, p.pop.firmSizeLargeMax + 1)
      case FirmSizeDist.Uniform => p.pop.workersPerFirm
