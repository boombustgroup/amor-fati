package sfc

import java.io.{File, PrintWriter}
import scala.util.Random

import _root_.sfc.config.{Config, SECTORS, RunConfig, MonetaryRegime}
import _root_.sfc.agents.*
import _root_.sfc.sfc.*
import _root_.sfc.engine.*
import _root_.sfc.networks.Network

/** Run one simulation with given seed. Returns time-series array. */
def runSingle(seed: Int, rc: RunConfig): Array[Array[Double]] =
  Random.setSeed(seed.toLong)

  // Generate Watts-Strogatz network
  val adjList = Network.wattsStrogatz(Config.FirmsCount, Config.NetworkK, Config.NetworkRewireP)

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
    Firm(
      id = i,
      cash = Random.between(10000.0, 80000.0) + (if Random.nextDouble() < 0.1 then 200000.0 else 0.0),
      debt = 0.0,
      tech = TechState.Traditional(Config.WorkersPerFirm),
      riskProfile = Random.between(0.1, 0.9),
      innovationCostFactor = Random.between(0.8, 1.5),
      digitalReadiness = Math.max(0.02, Math.min(0.98,
        sec.baseDigitalReadiness + (Random.nextGaussian() * 0.20))),
      sector = sectorAssignments(i),
      neighbors = adjList(i)
    )
  }.toArray

  val initCash = firms.map(_.cash).sum
  val initRate = if rc.isEurozone then Config.EcbInitialRate else Config.NbpInitialRate
  var world = World(0, 0.02, 1.0, 1.0,
    GovState(false, 0, 0, 0, 0), NbpState(initRate),
    BankState(0, 0, Config.InitBankCapital, initCash),
    ForexState(Config.BaseExRate, 0, Config.ExportBase, 0, 0),
    HhState(Config.TotalPopulation, Config.BaseWage, Config.BaseReservationWage, 0, 0, 0, 0),
    0, 0, Config.BaseRevenue * Config.FirmsCount)

  // Collect time-series: 120 rows x N columns
  // Columns: Month, Inflation, Unemployment, AutoRatio+HybridRatio, ExRate, MarketWage,
  //          GovDebt, NPL, RefRate, PriceLevel, AutoRatio, HybridRatio,
  //          SectorAutoRatio(0..5): BPO, Manuf, Retail, Health, Public, Agri,
  //          EffectiveBDP (per-capita BDP actually delivered after fiscal constraints)
  val nCols = 19
  val results = Array.ofDim[Double](Config.Duration, nCols)

  for t <- 0 until Config.Duration do
    val (newW, newF) = Simulation.step(world, firms, rc)
    world = newW
    firms = newF

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
      effectiveBdp                // 18: EffectiveBDP
    )

  results

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

  println(s"+" + "=" * 62 + "+")
  println(s"|  SFC-ABM v6: BDP=${bdpAmount.toInt} PLN, N=${nSeeds} seeds, ${regimeLabel}  ")
  println(s"|  10 000 firm x 6 sectors (GUS 2024) x WS network x 120m   |")
  println(s"+" + "=" * 62 + "+")

  val outDir = new File("mc")
  if !outDir.exists() then outDir.mkdirs()

  // Aggregation arrays
  val nMonths = Config.Duration
  val nCols   = 19
  val allRuns = Array.ofDim[Double](nSeeds, nMonths, nCols)

  val startTime = System.currentTimeMillis()

  for seed <- 1 to nSeeds do
    val t0 = System.currentTimeMillis()
    val results = runSingle(seed, rc)
    allRuns(seed - 1) = results
    val dt = System.currentTimeMillis() - t0

    if seed <= 3 || seed % 10 == 0 || seed == nSeeds then
      val adoption = results(nMonths - 1)(3)
      val inflation = results(nMonths - 1)(1)
      val unemp = results(nMonths - 1)(2)
      println(f"  Seed $seed%3d/${nSeeds} (${dt}ms) | " +
        f"Adopt=${adoption * 100}%5.1f%% | pi=${inflation * 100}%5.1f%% | " +
        f"Unemp=${unemp * 100}%5.1f%%")

  val totalTime = (System.currentTimeMillis() - startTime) / 1000.0
  println(f"\nTotal time: ${totalTime}%.1f seconds")

  // -- Write per-seed terminal values --
  val termPw = new PrintWriter(new File(s"mc/${outputPrefix}_terminal.csv"))
  termPw.write("Seed;Inflation;Unemployment;TotalAdoption;ExRate;MarketWage;" +
    "GovDebt;NPL;RefRate;PriceLevel;AutoRatio;HybridRatio;" +
    "BPO_Auto;Manuf_Auto;Retail_Auto;Health_Auto;Public_Auto;Agri_Auto;EffectiveBDP\n")
  for seed <- 0 until nSeeds do
    val last = allRuns(seed)(nMonths - 1)
    termPw.write(s"${seed + 1}")
    for c <- 1 until nCols do
      termPw.write(f";${last(c)}%.6f")
    termPw.write("\n")
  termPw.close()

  // -- Write aggregated time-series (mean, std, p5, p95) --
  val aggPw = new PrintWriter(new File(s"mc/${outputPrefix}_timeseries.csv"))
  val colNames = Array("Month", "Inflation", "Unemployment", "TotalAdoption", "ExRate",
    "MarketWage", "GovDebt", "NPL", "RefRate", "PriceLevel",
    "AutoRatio", "HybridRatio", "BPO_Auto", "Manuf_Auto", "Retail_Auto", "Health_Auto",
    "Public_Auto", "Agri_Auto", "EffectiveBDP")
  // Header: Month, then for each metric: mean, std, p05, p95
  aggPw.write("Month")
  for c <- 1 until nCols do
    aggPw.write(s";${colNames(c)}_mean;${colNames(c)}_std;${colNames(c)}_p05;${colNames(c)}_p95")
  aggPw.write("\n")

  for t <- 0 until nMonths do
    aggPw.write(s"${t + 1}")
    for c <- 1 until nCols do
      val vals = (0 until nSeeds).map(s => allRuns(s)(t)(c)).sorted.toArray
      val mean = vals.sum / vals.length
      val variance = vals.map(v => (v - mean) * (v - mean)).sum / vals.length
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
    val mean = vals.sum / vals.length
    val std  = Math.sqrt(vals.map(v => (v - mean) * (v - mean)).sum / vals.length)
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

  println("\nPer-sector adoption at M120:")
  val secNames = SECTORS.map(_.name)
  for s <- SECTORS.indices do
    statsSummary(f"  ${secNames(s)}%-22s", 12 + s, 100.0)

  println(s"\nSaved: mc/${outputPrefix}_terminal.csv")
  println(s"Saved: mc/${outputPrefix}_timeseries.csv")
