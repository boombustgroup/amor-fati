package sfc

import java.io.{File, PrintWriter}
import scala.util.Random

import _root_.sfc.config.{Config, SECTORS, TOPOLOGY, Topology, HH_MODE, HhMode, RunConfig, MonetaryRegime, FirmSizeDistribution}
import _root_.sfc.agents.*
import _root_.sfc.sfc.*
import _root_.sfc.engine.*
import _root_.sfc.engine.{BankingSector, BankingSectorState}
import _root_.sfc.engine.KahanSum.*
import _root_.sfc.networks.Network

/** Result of a single simulation run. */
case class RunResult(
  timeSeries: Array[Array[Double]],
  terminalHhAgg: Option[HhAggregates]
)

/** Run one simulation with given seed. Returns time-series array + optional household aggregates. */
def runSingle(seed: Int, rc: RunConfig): RunResult =
  Random.setSeed(seed.toLong)

  // Generate network based on TOPOLOGY env var
  val adjList = TOPOLOGY match
    case Topology.Ws      => Network.wattsStrogatz(Config.FirmsCount, Config.NetworkK, Config.NetworkRewireP)
    case Topology.Er      => Network.erdosRenyi(Config.FirmsCount, Config.NetworkK, Random)
    case Topology.Ba      => Network.barabasiAlbert(Config.FirmsCount, Config.NetworkK / 2, Random)
    case Topology.Lattice => Network.lattice(Config.FirmsCount, Config.NetworkK)

  // Assign sectors
  val sectorCounts = SECTORS.map(s => (s.share * Config.FirmsCount).toInt)
  // Adjust last sector to fill remaining
  val totalAssigned = sectorCounts.sum
  val sectorAssignments =
    val arr = new Array[Int](Config.FirmsCount)
    var idx = 0
    for
      s <- SECTORS.indices
      _ <- 0 until sectorCounts(s)
    do
      if idx < Config.FirmsCount then { arr(idx) = s; idx += 1 }
    while idx < Config.FirmsCount do { arr(idx) = SECTORS.length - 1; idx += 1 }
    // Shuffle to avoid spatial sector clustering
    val shuffled = Random.shuffle(arr.toList).toArray
    shuffled

  // Initialize firms
  var firms = (0 until Config.FirmsCount).map { i =>
    val sec = SECTORS(sectorAssignments(i))
    val firmSize = FirmSizeDistribution.draw(Random)
    val sizeMult = firmSize.toDouble / Config.WorkersPerFirm
    Firm(
      id = i,
      cash = (Random.between(10000.0, 80000.0) + (if Random.nextDouble() < 0.1 then 200000.0 else 0.0)) * sizeMult,
      debt = 0.0,
      tech = TechState.Traditional(firmSize),
      riskProfile = Random.between(0.1, 0.9),
      innovationCostFactor = Random.between(0.8, 1.5),
      digitalReadiness = Math.max(0.02, Math.min(0.98,
        sec.baseDigitalReadiness + (Random.nextGaussian() * 0.20))),
      sector = sectorAssignments(i),
      neighbors = adjList(i),
      initialSize = firmSize
    )
  }.toArray

  // Compute actual total population from firm sizes and set Config
  val actualTotalPop = firms.map(f => FirmOps.workers(f)).sum
  Config.setTotalPopulation(actualTotalPop)

  // Multi-bank: assign firms to banks
  if Config.BankMulti then
    firms = firms.map(f =>
      f.copy(bankId = BankingSector.assignBank(f.sector, BankingSector.DefaultConfigs, Random)))

  // Initialize households (individual mode only)
  var households: Option[Vector[Household]] = HH_MODE match
    case HhMode.Individual =>
      val hhCount = Config.TotalPopulation
      val hhNetwork = Network.wattsStrogatz(hhCount, Config.HhSocialK, Config.HhSocialP)
      val hhs = HouseholdInit.initialize(hhCount, Config.FirmsCount, firms, hhNetwork, Random)
      // Multi-bank: assign households to same bank as their employer
      val assigned = if Config.BankMulti then
        hhs.map { h =>
          h.status match
            case HhStatus.Employed(fid, _, _) if fid < firms.length => h.copy(bankId = firms(fid).bankId)
            case _ => h
        }
      else hhs
      Some(assigned)
    case HhMode.Aggregate => None

  // Initial immigrants (IMMIG_INIT_STOCK > 0, individual mode only)
  if Config.ImmigEnabled && Config.ImmigInitStock > 0 && households.isDefined then
    val startId = Config.TotalPopulation
    val immigrants = ImmigrationLogic.spawnImmigrants(Config.ImmigInitStock, startId, Random)
    households = households.map(hhs => hhs ++ immigrants)
    Config.setTotalPopulation(Config.TotalPopulation + Config.ImmigInitStock)

  val initCash = firms.kahanSumBy(_.cash)
  val initRate = if rc.isEurozone then Config.EcbInitialRate else Config.NbpInitialRate
  val initBankingSector = if Config.BankMulti then
    Some(BankingSector.initialize(initCash, Config.InitBankCapital, BankingSector.DefaultConfigs))
  else None

  // Initialize demographics with configured initial retirees
  val initDemographics = if Config.DemEnabled then
    DemographicsState(Config.DemInitialRetirees, Config.TotalPopulation, 0)
  else DemographicsState.zero

  var world = World(0, 0.02, 1.0,
    GovState(false, 0, 0, 0, 0, 0), NbpState(initRate),
    BankState(0, 0, Config.InitBankCapital, initCash),
    ForexState(Config.BaseExRate, 0, Config.ExportBase, 0, 0),
    HhState(Config.TotalPopulation, Config.BaseWage, Config.BaseReservationWage, 0, 0, 0, 0),
    0, 0, Config.BaseRevenue * Config.FirmsCount,
    SECTORS.map(_.sigma).toVector,
    bankingSector = initBankingSector,
    demographics = initDemographics,
    equity = if Config.GpwEnabled then
      val initEq = EquityMarket.initial
      // Aggregate mode: initial HH equity wealth = participation × avg savings × equity fraction
      val initHhEq = if Config.GpwHhEquity then
        Config.TotalPopulation * Config.GpwHhEquityFrac *
          Math.exp(Config.HhSavingsMu) * 0.05
      else 0.0
      initEq.copy(hhEquityWealth = initHhEq)
    else EquityMarket.zero,
    housing = if Config.ReEnabled then HousingMarket.initial
              else HousingMarket.zero,
    gvc = if Config.GvcEnabled && Config.OeEnabled then ExternalSector.initial
          else ExternalSector.zero,
    expectations = if Config.ExpEnabled then Expectations.initial
                   else Expectations.zero,
    immigration = if Config.ImmigEnabled then
      ImmigrationState(Config.ImmigInitStock, 0, 0, 0.0)
    else ImmigrationState.zero)

  // Collect time-series: 120 rows x N columns
  // Columns: Month, Inflation, Unemployment, AutoRatio+HybridRatio, ExRate, MarketWage,
  //          GovDebt, NPL, RefRate, PriceLevel, AutoRatio, HybridRatio,
  //          SectorAutoRatio(0..5): BPO, Manuf, Retail, Health, Public, Agri,
  //          EffectiveBDP (per-capita BDP actually delivered after fiscal constraints),
  //          SectorSigma(0..5): per-sector current sigma (evolves when SIGMA_LAMBDA>0),
  //          MeanDegree: average network degree (changes when REWIRE_RHO>0),
  //          IoFlows: total intermediate market payments (Paper-07),
  //          IoGdpRatio: intermediate flows / GDP,
  //          InterbankRate, MinBankCAR, MaxBankNPL, BankFailures,
  //          Housing: HPI, MarketValue, MortgageStock, MortgageRate, Origination,
  //                   Repayment, Default, MortgageInterest, HhHousingWealth,
  //                   HousingWealthEffect, MortgageToGdp
  val nCols = 136
  val results = Array.ofDim[Double](Config.Duration, nCols)

  for t <- 0 until Config.Duration do
    val (newW, newF, newHh) = Simulation.step(world, firms, rc, households)
    world = newW
    firms = newF
    households = newHh

    val unemployPct = 1.0 - world.hh.employed.toDouble / Config.TotalPopulation
    val living = firms.filter(FirmOps.isAlive)
    val nLiving = living.length.toDouble

    // Per-sector automation ratios
    val sectorAuto = SECTORS.indices.map { s =>
      val secFirms = living.filter(_.sector == s)
      if secFirms.isEmpty then 0.0
      else secFirms.count(f =>
        f.tech.isInstanceOf[TechState.Automated] || f.tech.isInstanceOf[TechState.Hybrid]
      ).toDouble / secFirms.length
    }

    // Effective BDP: actual per-capita BDP delivered (may be < legislated under EUR/SGP)
    val effectiveBdp = if world.gov.bdpActive then
      world.gov.bdpSpending / Config.TotalPopulation.toDouble
    else 0.0

    results(t) = Array(
      (t + 1).toDouble,          // 0: Month
      world.inflation,            // 1: Inflation
      unemployPct,                // 2: Unemployment
      world.automationRatio + world.hybridRatio, // 3: TotalAdoption
      world.forex.exchangeRate,   // 4: ExRate
      world.hh.marketWage,        // 5: MarketWage
      world.gov.cumulativeDebt,   // 6: GovDebt
      world.bank.nplRatio,        // 7: NPL
      world.nbp.referenceRate,    // 8: RefRate
      world.priceLevel,           // 9: PriceLevel
      world.automationRatio,      // 10: AutoRatio
      world.hybridRatio,          // 11: HybridRatio
      sectorAuto(0),              // 12: BPO auto
      sectorAuto(1),              // 13: Manuf auto
      sectorAuto(2),              // 14: Retail auto
      sectorAuto(3),              // 15: Health auto
      sectorAuto(4),              // 16: Public auto
      sectorAuto(5),              // 17: Agri auto
      effectiveBdp,               // 18: EffectiveBDP
      world.currentSigmas(0),     // 19: BPO_Sigma
      world.currentSigmas(1),     // 20: Manuf_Sigma
      world.currentSigmas(2),     // 21: Retail_Sigma
      world.currentSigmas(3),     // 22: Health_Sigma
      world.currentSigmas(4),     // 23: Public_Sigma
      world.currentSigmas(5),     // 24: Agri_Sigma
      firms.kahanSumBy(_.neighbors.length.toDouble) / firms.length, // 25: MeanDegree
      world.ioFlows,                    // 26: IoFlows
      if world.gdpProxy > 0 then world.ioFlows / world.gdpProxy else 0.0,  // 27: IoGdpRatio
      world.bop.nfa,                    // 28: NFA
      world.bop.currentAccount,         // 29: CurrentAccount
      world.bop.capitalAccount,         // 30: CapitalAccount
      world.bop.tradeBalance,           // 31: TradeBalance_OE
      world.bop.exports,                // 32: Exports_OE
      world.bop.totalImports,           // 33: TotalImports_OE
      world.bop.importedIntermediates,  // 34: ImportedInterm
      world.bop.fdi,                    // 35: FDI
      world.gov.unempBenefitSpend,      // 36: UnempBenefitSpend
      (1.0 - world.hh.employed.toDouble / Config.TotalPopulation - Config.NbpNairu) / Config.NbpNairu,  // 37: OutputGap
      world.gov.bondYield,                // 38: BondYield
      world.gov.bondsOutstanding,         // 39: BondsOutstanding
      world.bank.govBondHoldings,         // 40: BankBondHoldings
      world.nbp.govBondHoldings,          // 41: NbpBondHoldings
      (if world.nbp.qeActive then 1.0 else 0.0),  // 42: QeActive
      world.gov.debtServiceSpend,         // 43: DebtService
      world.nbp.govBondHoldings * world.gov.bondYield / 12.0,  // 44: NbpRemittance
      world.nbp.fxReserves,                                      // 45: FxReserves
      world.nbp.lastFxTraded,                                    // 46: FxInterventionAmt
      (if Config.NbpFxIntervention then 1.0 else 0.0),           // 47: FxInterventionActive
      world.bankingSector.map(_.interbankRate).getOrElse(world.nbp.referenceRate),  // 48: InterbankRate
      world.bankingSector.map { bs =>
        val alive = bs.banks.filterNot(_.failed)
        if alive.isEmpty then 0.0 else alive.map(_.car).min
      }.getOrElse(world.bank.car),  // 49: MinBankCAR
      world.bankingSector.map { bs =>
        val alive = bs.banks.filterNot(_.failed)
        if alive.isEmpty then 0.0 else alive.map(_.nplRatio).max
      }.getOrElse(world.bank.nplRatio),  // 50: MaxBankNPL
      world.bankingSector.map(_.banks.count(_.failed).toDouble).getOrElse(0.0),  // 51: BankFailures
      // Monetary plumbing
      world.bankingSector.map { bs =>
        val (_, total) = BankingSector.computeReserveInterest(bs.banks, world.nbp.referenceRate)
        total
      }.getOrElse(0.0),  // 52: ReserveInterest
      world.bankingSector.map { bs =>
        val (perBank, _) = BankingSector.computeStandingFacilities(bs.banks, world.nbp.referenceRate)
        perBank.kahanSum
      }.getOrElse(0.0),  // 53: StandingFacilityNet
      world.bankingSector.map { bs =>
        // Deposit facility usage: sum of reserves parked at deposit facility
        bs.banks.filter(b => !b.failed && b.reservesAtNbp > 0).kahanSumBy(_.reservesAtNbp)
      }.getOrElse(0.0),  // 54: DepositFacilityUsage
      world.bankingSector.map { bs =>
        val (_, total) = BankingSector.interbankInterestFlows(bs.banks, bs.interbankRate)
        total
      }.getOrElse(0.0),  // 55: InterbankInterestNet
      // Credit diagnostics
      world.monetaryAgg.map(_.m1).getOrElse(world.bank.deposits),     // 56: M1
      world.monetaryAgg.map(_.monetaryBase).getOrElse(0.0),           // 57: MonetaryBase
      world.monetaryAgg.map(_.creditMultiplier).getOrElse(0.0),       // 58: CreditMultiplier
      // JST (local government)
      world.jst.revenue,           // 59: JstRevenue
      world.jst.spending,          // 60: JstSpending
      world.jst.debt,              // 61: JstDebt
      world.jst.deposits,          // 62: JstDeposits
      world.jst.deficit,           // 63: JstDeficit
      // LCR/NSFR
      world.bankingSector.map { bs =>
        val alive = bs.banks.filterNot(_.failed)
        if alive.isEmpty then 0.0 else alive.map(_.lcr).min
      }.getOrElse(0.0),  // 64: MinBankLCR
      world.bankingSector.map { bs =>
        val alive = bs.banks.filterNot(_.failed)
        if alive.isEmpty then 0.0 else alive.map(_.nsfr).min
      }.getOrElse(0.0),  // 65: MinBankNSFR
      world.bankingSector.map { bs =>
        val alive = bs.banks.filterNot(_.failed)
        if alive.isEmpty then 0.0
        else alive.map(b => if b.deposits > 0 then b.termDeposits / b.deposits else 0.0).sum / alive.length
      }.getOrElse(Config.BankTermDepositFrac),  // 66: AvgTermDepositFrac
      // Term structure
      world.bankingSector.flatMap(_.interbankCurve).map(_.wibor1m).getOrElse(0.0),  // 67: WIBOR_1M
      world.bankingSector.flatMap(_.interbankCurve).map(_.wibor3m).getOrElse(0.0),  // 68: WIBOR_3M
      world.bankingSector.flatMap(_.interbankCurve).map(_.wibor6m).getOrElse(0.0),  // 69: WIBOR_6M
      // Full public sector
      world.zus.contributions,           // 70: ZusContributions
      world.zus.pensionPayments,         // 71: ZusPensionPayments
      world.zus.govSubvention,           // 72: ZusGovSubvention
      world.zus.fusBalance,              // 73: FusBalance
      world.ppk.contributions,           // 74: PpkContributions
      world.ppk.bondHoldings,            // 75: PpkBondHoldings
      world.demographics.retirees.toDouble,           // 76: NRetirees
      world.demographics.workingAgePop.toDouble,      // 77: WorkingAgePop
      world.demographics.monthlyRetirements.toDouble,  // 78: MonthlyRetirements
      // Macroprudential
      world.macropru.ccyb,                             // 79: CCyB
      world.macropru.creditToGdpGap,                   // 80: CreditToGdpGap
      world.bankingSector.map { bs =>
        val alive = bs.banks.filterNot(_.failed)
        if alive.isEmpty then 0.0
        else alive.map(b => Macroprudential.effectiveMinCar(b.id, world.macropru.ccyb)).max
      }.getOrElse(Macroprudential.effectiveMinCar(0, world.macropru.ccyb)),  // 81: EffectiveMinCar
      // GPW Equity Market
      world.equity.index,           // 82: GpwIndex
      world.equity.marketCap,       // 83: GpwMarketCap
      (if world.equity.earningsYield > 0 then 1.0 / world.equity.earningsYield else 0.0),  // 84: GpwPE
      world.equity.dividendYield,   // 85: GpwDivYield
      // GPW Firm Issuance
      world.equity.lastIssuance,    // 86: EquityIssuanceTotal
      living.kahanSumBy(_.equityRaised) / Math.max(1.0, living.kahanSumBy(f => f.debt + f.equityRaised)),  // 87: EquityFinancedFrac
      // GPW Household Equity
      world.equity.hhEquityWealth,  // 88: HhEquityWealth
      world.equity.lastWealthEffect, // 89: EquityWealthEffect
      // GPW Dividends
      world.equity.lastDomesticDividends,  // 90: DomesticDividends
      world.equity.lastForeignDividends,   // 91: ForeignDividendOutflow
      // Housing Market
      world.housing.priceIndex,            // 92: HousingPriceIndex
      world.housing.totalValue,            // 93: HousingMarketValue
      world.housing.mortgageStock,         // 94: MortgageStock
      world.housing.avgMortgageRate,       // 95: AvgMortgageRate
      world.housing.lastOrigination,       // 96: MortgageOrigination
      world.housing.lastRepayment,         // 97: MortgageRepayment
      world.housing.lastDefault,           // 98: MortgageDefault
      world.housing.mortgageInterestIncome, // 99: MortgageInterestIncome
      world.housing.hhHousingWealth,       // 100: HhHousingWealth
      world.housing.lastWealthEffect,      // 101: HousingWealthEffect
      (if world.gdpProxy > 0 && world.housing.mortgageStock > 0
       then world.housing.mortgageStock / (world.gdpProxy * 12.0) else 0.0),  // 102: MortgageToGdp
      // Sectoral Labor Mobility
      world.sectoralMobility.sectorMobilityRate,    // 103: SectorMobilityRate
      world.sectoralMobility.crossSectorHires.toDouble,  // 104: CrossSectorHires
      world.sectoralMobility.voluntaryQuits.toDouble, // 105: VoluntaryQuits
      // GVC / Deep External Sector
      world.gvc.disruptionIndex,        // 106: GvcDisruptionIndex
      world.gvc.foreignPriceIndex,      // 107: ForeignPriceIndex
      world.gvc.tradeConcentration,     // 108: GvcTradeConcentration
      world.gvc.exportDemandShockMag,   // 109: GvcExportDemandShock
      world.gvc.importCostIndex,         // 110: GvcImportCostIndex
      // Forward-Looking Expectations
      world.expectations.expectedInflation,   // 111: ExpectedInflation
      world.expectations.credibility,         // 112: NbpCredibility
      world.expectations.forwardGuidanceRate, // 113: ForwardGuidanceRate
      world.expectations.forecastError,       // 114: InflationForecastError
      // Regional Housing Market
      world.housing.regions.map(_(0).priceIndex).getOrElse(0.0),  // 115: WawHpi
      world.housing.regions.map(_(1).priceIndex).getOrElse(0.0),  // 116: KrkHpi
      world.housing.regions.map(_(2).priceIndex).getOrElse(0.0),  // 117: WroHpi
      world.housing.regions.map(_(3).priceIndex).getOrElse(0.0),  // 118: GdnHpi
      world.housing.regions.map(_(4).priceIndex).getOrElse(0.0),  // 119: LdzHpi
      world.housing.regions.map(_(5).priceIndex).getOrElse(0.0),  // 120: PozHpi
      world.housing.regions.map(_(6).priceIndex).getOrElse(0.0),  // 121: RestHpi
      // Immigration
      world.immigration.immigrantStock.toDouble,     // 122: ImmigrantStock
      world.immigration.monthlyInflow.toDouble,      // 123: MonthlyImmigInflow
      world.immigration.remittanceOutflow,            // 124: RemittanceOutflow
      (if world.immigration.immigrantStock > 0 then  // 125: ImmigrantUnempRate
        households.map { hhs =>
          val immigrants = hhs.filter(_.isImmigrant)
          if immigrants.nonEmpty then
            immigrants.count(h => !h.status.isInstanceOf[HhStatus.Employed]).toDouble / immigrants.length
          else 0.0
        }.getOrElse(0.0)
      else 0.0),
      // PIT
      world.hhAgg.map { agg =>                      // 126: EffectivePitRate
        val gross = agg.totalIncome + agg.totalPit   // totalIncome is net-of-PIT, add back
        if gross > 0 then agg.totalPit / gross else 0.0
      }.getOrElse(if Config.PitEnabled then Config.PitEffectiveRate else 0.0),
      // Social Transfers
      world.gov.socialTransferSpend,                 // 127: SocialTransferSpend
      // Public Investment
      world.gov.govCurrentSpend,                     // 128: GovCurrentSpend
      world.gov.govCapitalSpend,                     // 129: GovCapitalSpend
      world.gov.publicCapitalStock,                   // 130: PublicCapitalStock
      // EU Funds Dynamics
      world.gov.euCofinancing,                         // 131: EuCofinancing
      world.bop.euFundsMonthly,                        // 132: EuFundsMonthly
      world.bop.euCumulativeAbsorption,                // 133: EuCumulativeAbsorption
      world.hh.minWageLevel,                             // 134: MinWageLevel
      world.fofResidual                                    // 135: FofResidual
    )

  RunResult(results, world.hhAgg)

// ---- Monte Carlo main entry point ----

@main def sfcMonteCarlo(bdpAmountStr: String, nSeedsStr: String, outputPrefix: String,
    regimeStr: String = "pln"): Unit =
  val bdpAmount = bdpAmountStr.toDouble
  val nSeeds    = nSeedsStr.toInt
  val regime = regimeStr.toLowerCase match
    case "eur" | "euro" | "ecb" => MonetaryRegime.Eur
    case _                      => MonetaryRegime.Pln
  val rc = RunConfig(bdpAmount, nSeeds, outputPrefix, regime)
  val regimeLabel = if rc.isEurozone then "EUR (ECB)" else "PLN (NBP)"

  val topoLabel = TOPOLOGY.toString.toUpperCase
  val firmsLabel = f"${Config.FirmsCount}%,d"
  val hhLabel = HH_MODE match
    case HhMode.Individual => s" | HH=individual (${Config.HhCount})"
    case HhMode.Aggregate  => ""
  val bankLabel = if Config.BankMulti then " | BANK=multi (7)" else ""
  println(s"+" + "=" * 68 + "+")
  println(s"|  SFC-ABM v8: BDP=${bdpAmount.toInt} PLN, N=${nSeeds} seeds, ${regimeLabel}${hhLabel}${bankLabel}")
  println(s"|  ${firmsLabel} firms x 6 sectors (GUS 2024) x ${topoLabel} network x 120m")
  println(s"+" + "=" * 68 + "+")

  val outDir = new File("mc")
  if !outDir.exists() then outDir.mkdirs()

  // Aggregation arrays
  val nMonths = Config.Duration
  val nCols   = 136
  val allRuns = Array.ofDim[Double](nSeeds, nMonths, nCols)
  val allHhAgg = new Array[Option[HhAggregates]](nSeeds)

  val startTime = System.currentTimeMillis()

  for seed <- 1 to nSeeds do
    val t0 = System.currentTimeMillis()
    val result = runSingle(seed, rc)
    allRuns(seed - 1) = result.timeSeries
    allHhAgg(seed - 1) = result.terminalHhAgg
    val dt = System.currentTimeMillis() - t0

    if seed <= 3 || seed % 10 == 0 || seed == nSeeds then
      val adoption = result.timeSeries(nMonths - 1)(3)
      val inflation = result.timeSeries(nMonths - 1)(1)
      val unemp = result.timeSeries(nMonths - 1)(2)
      println(f"  Seed $seed%3d/${nSeeds} (${dt}ms) | " +
        f"Adopt=${adoption * 100}%5.1f%% | pi=${inflation * 100}%5.1f%% | " +
        f"Unemp=${unemp * 100}%5.1f%%")

  val totalTime = (System.currentTimeMillis() - startTime) / 1000.0
  println(f"\nTotal time: ${totalTime}%.1f seconds")

  // -- Write per-seed terminal values --
  val termPw = new PrintWriter(new File(s"mc/${outputPrefix}_terminal.csv"))
  termPw.write("Seed;Inflation;Unemployment;TotalAdoption;ExRate;MarketWage;" +
    "GovDebt;NPL;RefRate;PriceLevel;AutoRatio;HybridRatio;" +
    "BPO_Auto;Manuf_Auto;Retail_Auto;Health_Auto;Public_Auto;Agri_Auto;EffectiveBDP;" +
    "BPO_Sigma;Manuf_Sigma;Retail_Sigma;Health_Sigma;Public_Sigma;Agri_Sigma;MeanDegree;" +
    "IoFlows;IoGdpRatio;" +
    "NFA;CurrentAccount;CapitalAccount;TradeBalance_OE;Exports_OE;TotalImports_OE;ImportedInterm;FDI;" +
    "UnempBenefitSpend;OutputGap;" +
    "BondYield;BondsOutstanding;BankBondHoldings;NbpBondHoldings;QeActive;DebtService;NbpRemittance;" +
    "FxReserves;FxInterventionAmt;FxInterventionActive;" +
    "InterbankRate;MinBankCAR;MaxBankNPL;BankFailures;" +
    "ReserveInterest;StandingFacilityNet;DepositFacilityUsage;InterbankInterestNet;" +
    "M1;MonetaryBase;CreditMultiplier;" +
    "JstRevenue;JstSpending;JstDebt;JstDeposits;JstDeficit;" +
    "MinBankLCR;MinBankNSFR;AvgTermDepositFrac;" +
    "WIBOR_1M;WIBOR_3M;WIBOR_6M;" +
    "ZusContributions;ZusPensionPayments;ZusGovSubvention;FusBalance;" +
    "PpkContributions;PpkBondHoldings;NRetirees;WorkingAgePop;MonthlyRetirements;" +
    "CCyB;CreditToGdpGap;EffectiveMinCar;" +
    "GpwIndex;GpwMarketCap;GpwPE;GpwDivYield;" +
    "EquityIssuanceTotal;EquityFinancedFrac;" +
    "HhEquityWealth;EquityWealthEffect;" +
    "DomesticDividends;ForeignDividendOutflow;" +
    "HousingPriceIndex;HousingMarketValue;MortgageStock;AvgMortgageRate;" +
    "MortgageOrigination;MortgageRepayment;MortgageDefault;MortgageInterestIncome;" +
    "HhHousingWealth;HousingWealthEffect;MortgageToGdp;" +
    "SectorMobilityRate;CrossSectorHires;VoluntaryQuits;" +
    "GvcDisruptionIndex;ForeignPriceIndex;GvcTradeConcentration;GvcExportDemandShock;GvcImportCostIndex;" +
    "ExpectedInflation;NbpCredibility;ForwardGuidanceRate;InflationForecastError;" +
    "WawHpi;KrkHpi;WroHpi;GdnHpi;LdzHpi;PozHpi;RestHpi;" +
    "ImmigrantStock;MonthlyImmigInflow;RemittanceOutflow;ImmigrantUnempRate;" +
    "EffectivePitRate;SocialTransferSpend;" +
    "GovCurrentSpend;GovCapitalSpend;PublicCapitalStock;" +
    "EuCofinancing;EuFundsMonthly;EuCumulativeAbsorption;MinWageLevel;FofResidual\n")
  for seed <- 0 until nSeeds do
    val last = allRuns(seed)(nMonths - 1)
    termPw.write(s"${seed + 1}")
    for c <- 1 until nCols do
      termPw.write(f";${last(c)}%.6f")
    termPw.write("\n")
  termPw.close()

  // -- Write household terminal CSV (individual mode only) --
  if HH_MODE == HhMode.Individual then
    val hhPw = new PrintWriter(new File(s"mc/${outputPrefix}_hh_terminal.csv"))
    hhPw.write("Seed;HH_Employed;HH_Unemployed;HH_Retraining;HH_Bankrupt;" +
      "MeanSavings;MedianSavings;P10Savings;P90Savings;" +
      "MeanDebt;TotalDebt;" +
      "Gini_Individual;Gini_Wealth;" +
      "MeanSkill;MeanHealthPenalty;" +
      "RetrainingAttempts;RetrainingSuccesses;" +
      "ConsumptionP10;ConsumptionP50;ConsumptionP90;" +
      "BankruptcyRate;MeanMonthsToRuin;" +
      "PovertyRate_50pct;PovertyRate_30pct\n")
    for seed <- 0 until nSeeds do
      allHhAgg(seed) match
        case Some(agg) =>
          hhPw.write(s"${seed + 1}")
          hhPw.write(f";${agg.employed};${agg.unemployed};${agg.retraining};${agg.bankrupt}")
          hhPw.write(f";${agg.meanSavings}%.2f;${agg.medianSavings}%.2f")
          // P10/P90 savings not tracked in agg — use mean as placeholder
          hhPw.write(f";${agg.meanSavings * 0.3}%.2f;${agg.meanSavings * 2.0}%.2f")
          hhPw.write(f";0.00;0.00")  // MeanDebt, TotalDebt — would need household vector
          hhPw.write(f";${agg.giniIndividual}%.6f;${agg.giniWealth}%.6f")
          hhPw.write(f";${agg.meanSkill}%.6f;${agg.meanHealthPenalty}%.6f")
          hhPw.write(f";${agg.retrainingAttempts};${agg.retrainingSuccesses}")
          hhPw.write(f";${agg.consumptionP10}%.2f;${agg.consumptionP50}%.2f;${agg.consumptionP90}%.2f")
          hhPw.write(f";${agg.bankruptcyRate}%.6f;${agg.meanMonthsToRuin}%.2f")
          hhPw.write(f";${agg.povertyRate50}%.6f;${agg.povertyRate30}%.6f")
          hhPw.write("\n")
        case None => ()
    hhPw.close()
    println(s"Saved: mc/${outputPrefix}_hh_terminal.csv")

  // -- Write bank terminal CSV (multi-bank mode only) --
  if Config.BankMulti then
    val bankPw = new PrintWriter(new File(s"mc/${outputPrefix}_banks_terminal.csv"))
    bankPw.write("Seed;BankId;BankName;Deposits;Loans;Capital;NPL;CAR;GovBonds;InterbankNet;Failed\n")
    for seed <- 0 until nSeeds do
      // Re-run to get final banking sector state (already in allRuns terminal row)
      val lastRow = allRuns(seed)(nMonths - 1)
      // Columns 48-51 are aggregate; for per-bank detail we need the actual state
      // Use a simplified approach: write aggregate-level data per bank from the terminal values
      bankPw.write(f"${seed + 1};0;Aggregate;0;0;0;${lastRow(50)}%.6f;${lastRow(49)}%.6f;0;0;${lastRow(51)}%.0f\n")
    bankPw.close()
    println(s"Saved: mc/${outputPrefix}_banks_terminal.csv")

  // -- Write aggregated time-series (mean, std, p5, p95) --
  val aggPw = new PrintWriter(new File(s"mc/${outputPrefix}_timeseries.csv"))
  val colNames = Array("Month", "Inflation", "Unemployment", "TotalAdoption", "ExRate",
    "MarketWage", "GovDebt", "NPL", "RefRate", "PriceLevel",
    "AutoRatio", "HybridRatio", "BPO_Auto", "Manuf_Auto", "Retail_Auto", "Health_Auto",
    "Public_Auto", "Agri_Auto", "EffectiveBDP",
    "BPO_Sigma", "Manuf_Sigma", "Retail_Sigma", "Health_Sigma", "Public_Sigma", "Agri_Sigma",
    "MeanDegree", "IoFlows", "IoGdpRatio",
    "NFA", "CurrentAccount", "CapitalAccount", "TradeBalance_OE", "Exports_OE",
    "TotalImports_OE", "ImportedInterm", "FDI",
    "UnempBenefitSpend", "OutputGap",
    "BondYield", "BondsOutstanding", "BankBondHoldings", "NbpBondHoldings",
    "QeActive", "DebtService", "NbpRemittance",
    "FxReserves", "FxInterventionAmt", "FxInterventionActive",
    "InterbankRate", "MinBankCAR", "MaxBankNPL", "BankFailures",
    "ReserveInterest", "StandingFacilityNet", "DepositFacilityUsage", "InterbankInterestNet",
    "M1", "MonetaryBase", "CreditMultiplier",
    "JstRevenue", "JstSpending", "JstDebt", "JstDeposits", "JstDeficit",
    "MinBankLCR", "MinBankNSFR", "AvgTermDepositFrac",
    "WIBOR_1M", "WIBOR_3M", "WIBOR_6M",
    "ZusContributions", "ZusPensionPayments", "ZusGovSubvention", "FusBalance",
    "PpkContributions", "PpkBondHoldings", "NRetirees", "WorkingAgePop", "MonthlyRetirements",
    "CCyB", "CreditToGdpGap", "EffectiveMinCar",
    "GpwIndex", "GpwMarketCap", "GpwPE", "GpwDivYield",
    "EquityIssuanceTotal", "EquityFinancedFrac",
    "HhEquityWealth", "EquityWealthEffect",
    "DomesticDividends", "ForeignDividendOutflow",
    "HousingPriceIndex", "HousingMarketValue", "MortgageStock", "AvgMortgageRate",
    "MortgageOrigination", "MortgageRepayment", "MortgageDefault", "MortgageInterestIncome",
    "HhHousingWealth", "HousingWealthEffect", "MortgageToGdp",
    "SectorMobilityRate", "CrossSectorHires", "VoluntaryQuits",
    "GvcDisruptionIndex", "ForeignPriceIndex", "GvcTradeConcentration",
    "GvcExportDemandShock", "GvcImportCostIndex",
    "ExpectedInflation", "NbpCredibility", "ForwardGuidanceRate", "InflationForecastError",
    "WawHpi", "KrkHpi", "WroHpi", "GdnHpi", "LdzHpi", "PozHpi", "RestHpi",
    "ImmigrantStock", "MonthlyImmigInflow", "RemittanceOutflow", "ImmigrantUnempRate",
    "EffectivePitRate", "SocialTransferSpend",
    "GovCurrentSpend", "GovCapitalSpend", "PublicCapitalStock",
    "EuCofinancing", "EuFundsMonthly", "EuCumulativeAbsorption",
    "MinWageLevel", "FofResidual")
  // Header: Month, then for each metric: mean, std, p05, p95
  aggPw.write("Month")
  for c <- 1 until nCols do
    aggPw.write(s";${colNames(c)}_mean;${colNames(c)}_std;${colNames(c)}_p05;${colNames(c)}_p95")
  aggPw.write("\n")

  for t <- 0 until nMonths do
    aggPw.write(s"${t + 1}")
    for c <- 1 until nCols do
      val vals = (0 until nSeeds).map(s => allRuns(s)(t)(c)).sorted.toArray
      val mean = vals.kahanSum / vals.length
      val variance = vals.kahanSumBy(v => (v - mean) * (v - mean)) / vals.length
      val std  = Math.sqrt(variance)
      val p05  = vals((vals.length * 0.05).toInt)
      val p95  = vals(Math.min(vals.length - 1, (vals.length * 0.95).toInt))
      aggPw.write(f";$mean%.6f;$std%.6f;$p05%.6f;$p95%.6f")
    aggPw.write("\n")
  aggPw.close()

  // -- Summary statistics --
  println("\n" + "=" * 54)
  println(s"MONTE CARLO SUMMARY: ${outputPrefix} (BDP=${bdpAmount.toInt}, N=${nSeeds})")
  println("=" * 54)

  def statsSummary(name: String, colIdx: Int, mult: Double = 1.0): Unit =
    val vals = (0 until nSeeds).map(s => allRuns(s)(nMonths - 1)(colIdx) * mult).sorted.toArray
    val mean = vals.kahanSum / vals.length
    val std  = Math.sqrt(vals.kahanSumBy(v => (v - mean) * (v - mean)) / vals.length)
    val p05  = vals((vals.length * 0.05).toInt)
    val p95  = vals(Math.min(vals.length - 1, (vals.length * 0.95).toInt))
    println(f"  $name%-25s mean=${mean}%8.2f +/- ${std}%6.2f  [${p05}%8.2f, ${p95}%8.2f]")

  statsSummary("Inflation (%)", 1, 100.0)
  statsSummary("Unemployment (%)", 2, 100.0)
  statsSummary("Total Adoption (%)", 3, 100.0)
  statsSummary("Exchange Rate", 4)
  statsSummary("Market Wage (PLN)", 5)
  statsSummary("Gov Debt (mld PLN)", 6, 1.0 / 1e9)
  statsSummary("NPL Ratio (%)", 7, 100.0)

  // Household summary (individual mode only)
  if HH_MODE == HhMode.Individual then
    println("\nHousehold aggregates at M120:")
    val hhAggs = allHhAgg.flatten
    if hhAggs.nonEmpty then
      val avgGini = hhAggs.kahanSumBy(_.giniIndividual) / hhAggs.length
      val avgWealth = hhAggs.kahanSumBy(_.giniWealth) / hhAggs.length
      val avgBankr = hhAggs.kahanSumBy(_.bankruptcyRate) / hhAggs.length
      val avgPov50 = hhAggs.kahanSumBy(_.povertyRate50) / hhAggs.length
      val avgSkill = hhAggs.kahanSumBy(_.meanSkill) / hhAggs.length
      println(f"  Gini (income)        mean=${avgGini * 100}%8.2f%%")
      println(f"  Gini (wealth)        mean=${avgWealth * 100}%8.2f%%")
      println(f"  Bankruptcy rate      mean=${avgBankr * 100}%8.2f%%")
      println(f"  Poverty rate (50%%)  mean=${avgPov50 * 100}%8.2f%%")
      println(f"  Mean skill           mean=${avgSkill}%8.4f")

  println("\nPer-sector adoption at M120:")
  val secNames = SECTORS.map(_.name)
  for s <- SECTORS.indices do
    statsSummary(f"  ${secNames(s)}%-22s", 12 + s, 100.0)

  println(s"\nSaved: mc/${outputPrefix}_terminal.csv")
  println(s"Saved: mc/${outputPrefix}_timeseries.csv")
