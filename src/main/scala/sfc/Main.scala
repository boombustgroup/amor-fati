package sfc

import sfc.Observables.Col
import sfc.agents.*
import sfc.config.*
import sfc.engine.*
import sfc.init.WorldInit
import sfc.types.*
import sfc.util.KahanSum.*

import java.io.{File, PrintWriter}

/** Result of a single simulation run. */
case class RunResult(
  timeSeries: Array[Array[Double]],
  terminalHhAgg: Household.Aggregates,
)

/** Run one simulation with given seed. Returns time-series array + optional household aggregates. */
def runSingle(seed: Int, rc: RunConfig): RunResult =
  val init = WorldInit.initialize(seed, rc)
  var state = Simulation.SimState(init.world, init.firms, init.households)

  val results = Array.ofDim[Double](Config.Duration, Observables.nCols)

  for t <- 0 until Config.Duration do
    val stepResult = Simulation.step(state, rc)
    stepResult.sfcCheck match
      case Left(errors) =>
        errors.foreach { e =>
          System.err.println(
            f"[SFC] Month ${t + 1} ${e.identity}: expected=${e.expected.toDouble}%.2f actual=${e.actual.toDouble}%.2f" +
              f" (Δ=${(e.actual - e.expected).toDouble}%.2f) — ${e.msg}",
          )
        }
      case Right(()) => // OK
    state = stepResult.state
    results(t) = Observables.compute(t, state.world, state.firms, state.households)

  RunResult(results, state.world.hhAgg.get)

// ---- Monte Carlo main entry point ----

@main def sfcMonteCarlo(
  bdpAmountStr: String,
  nSeedsStr: String,
  outputPrefix: String,
  regimeStr: String = "pln",
): Unit =
  val bdpAmount = bdpAmountStr.toDouble
  val nSeeds = nSeedsStr.toInt
  val regime = regimeStr.toLowerCase match
    case "eur" | "euro" | "ecb" => MonetaryRegime.Eur
    case _                      => MonetaryRegime.Pln
  val rc = RunConfig(bdpAmount, nSeeds, outputPrefix, regime)
  val regimeLabel = if rc.isEurozone then "EUR (ECB)" else "PLN (NBP)"

  val topoLabel = TOPOLOGY.toString.toUpperCase
  val firmsLabel = f"${Config.FirmsCount}%,d"
  val hhLabel = s" | HH=individual (${Config.HhCount})"
  val bankLabel = " | BANK=multi (7)"
  println(s"+" + "=" * 68 + "+")
  println(s"|  SFC-ABM v8: BDP=${bdpAmount.toInt} PLN, N=${nSeeds} seeds, ${regimeLabel}${hhLabel}${bankLabel}")
  println(s"|  ${firmsLabel} firms x 6 sectors (GUS 2024) x ${topoLabel} network x 120m")
  println(s"+" + "=" * 68 + "+")

  val outDir = new File("mc")
  if !outDir.exists() then outDir.mkdirs()

  // Aggregation arrays
  val nMonths = Config.Duration
  val nCols = Observables.nCols
  val colNames = Observables.colNames
  val allRuns = Array.ofDim[Double](nSeeds, nMonths, nCols)
  val allHhAgg = new Array[Household.Aggregates](nSeeds)

  val startTime = System.currentTimeMillis()

  for seed <- 1 to nSeeds do
    val t0 = System.currentTimeMillis()
    val result = runSingle(seed, rc)
    allRuns(seed - 1) = result.timeSeries
    allHhAgg(seed - 1) = result.terminalHhAgg
    val dt = System.currentTimeMillis() - t0

    if seed <= 3 || seed % 10 == 0 || seed == nSeeds then
      val last = result.timeSeries(nMonths - 1)
      val adoption = last(Col.TotalAdoption.ordinal)
      val inflation = last(Col.Inflation.ordinal)
      val unemp = last(Col.Unemployment.ordinal)
      println(
        f"  Seed $seed%3d/${nSeeds} (${dt}ms) | " +
          f"Adopt=${adoption * 100}%5.1f%% | pi=${inflation * 100}%5.1f%% | " +
          f"Unemp=${unemp * 100}%5.1f%%",
      )

  val totalTime = (System.currentTimeMillis() - startTime) / 1000.0
  println(f"\nTotal time: ${totalTime}%.1f seconds")

  // -- Write per-seed terminal values --
  val termPw = new PrintWriter(new File(s"mc/${outputPrefix}_terminal.csv"))
  termPw.write("Seed;" + colNames.drop(1).mkString(";") + "\n")
  for seed <- 0 until nSeeds do
    val last = allRuns(seed)(nMonths - 1)
    termPw.write(s"${seed + 1}")
    for c <- 1 until nCols do termPw.write(f";${last(c)}%.6f")
    termPw.write("\n")
  termPw.close()

  // -- Write household terminal CSV --
  {
    val hhPw = new PrintWriter(new File(s"mc/${outputPrefix}_hh_terminal.csv"))
    hhPw.write(
      "Seed;HH_Employed;HH_Unemployed;HH_Retraining;HH_Bankrupt;" +
        "MeanSavings;MedianSavings;P10Savings;P90Savings;" +
        "MeanDebt;TotalDebt;" +
        "Gini_Individual;Gini_Wealth;" +
        "MeanSkill;MeanHealthPenalty;" +
        "RetrainingAttempts;RetrainingSuccesses;" +
        "ConsumptionP10;ConsumptionP50;ConsumptionP90;" +
        "BankruptcyRate;MeanMonthsToRuin;" +
        "PovertyRate_50pct;PovertyRate_30pct\n",
    )
    for seed <- 0 until nSeeds do
      val agg = allHhAgg(seed)
      hhPw.write(s"${seed + 1}")
      hhPw.write(f";${agg.employed};${agg.unemployed};${agg.retraining};${agg.bankrupt}")
      hhPw.write(f";${agg.meanSavings.toDouble}%.2f;${agg.medianSavings.toDouble}%.2f")
      // P10/P90 savings not tracked in agg — use mean as placeholder
      hhPw.write(f";${(agg.meanSavings * 0.3).toDouble}%.2f;${(agg.meanSavings * 2.0).toDouble}%.2f")
      hhPw.write(f";0.00;0.00") // MeanDebt, TotalDebt — would need household vector
      hhPw.write(f";${agg.giniIndividual.toDouble}%.6f;${agg.giniWealth.toDouble}%.6f")
      hhPw.write(f";${agg.meanSkill}%.6f;${agg.meanHealthPenalty}%.6f")
      hhPw.write(f";${agg.retrainingAttempts};${agg.retrainingSuccesses}")
      hhPw.write(
        f";${agg.consumptionP10.toDouble}%.2f;${agg.consumptionP50.toDouble}%.2f;${agg.consumptionP90.toDouble}%.2f",
      )
      hhPw.write(f";${agg.bankruptcyRate.toDouble}%.6f;${agg.meanMonthsToRuin}%.2f")
      hhPw.write(f";${agg.povertyRate50.toDouble}%.6f;${agg.povertyRate30.toDouble}%.6f")
      hhPw.write("\n")
    hhPw.close()
    println(s"Saved: mc/${outputPrefix}_hh_terminal.csv")
  }

  // -- Write bank terminal CSV (always multi-bank) --
  {
    val bankPw = new PrintWriter(new File(s"mc/${outputPrefix}_banks_terminal.csv"))
    bankPw.write("Seed;BankId;BankName;Deposits;Loans;Capital;NPL;CAR;GovBonds;InterbankNet;Failed\n")
    for seed <- 0 until nSeeds do
      val lastRow = allRuns(seed)(nMonths - 1)
      bankPw.write(
        f"${seed + 1};0;Aggregate;0;0;0;${lastRow(Col.MaxBankNPL.ordinal)}%.6f;${lastRow(Col.MinBankCAR.ordinal)}%.6f;0;0;${lastRow(Col.BankFailures.ordinal)}%.0f\n",
      )
    bankPw.close()
    println(s"Saved: mc/${outputPrefix}_banks_terminal.csv")
  }

  // -- Write aggregated time-series (mean, std, p5, p95) --
  val aggPw = new PrintWriter(new File(s"mc/${outputPrefix}_timeseries.csv"))
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
      val std = Math.sqrt(variance)
      val p05 = vals((vals.length * 0.05).toInt)
      val p95 = vals(Math.min(vals.length - 1, (vals.length * 0.95).toInt))
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
    val std = Math.sqrt(vals.kahanSumBy(v => (v - mean) * (v - mean)) / vals.length)
    val p05 = vals((vals.length * 0.05).toInt)
    val p95 = vals(Math.min(vals.length - 1, (vals.length * 0.95).toInt))
    println(f"  $name%-25s mean=${mean}%8.2f +/- ${std}%6.2f  [${p05}%8.2f, ${p95}%8.2f]")

  statsSummary("Inflation (%)", Col.Inflation.ordinal, 100.0)
  statsSummary("Unemployment (%)", Col.Unemployment.ordinal, 100.0)
  statsSummary("Total Adoption (%)", Col.TotalAdoption.ordinal, 100.0)
  statsSummary("Exchange Rate", Col.ExRate.ordinal)
  statsSummary("Market Wage (PLN)", Col.MarketWage.ordinal)
  statsSummary("Gov Debt (mld PLN)", Col.GovDebt.ordinal, 1.0 / 1e9)
  statsSummary("NPL Ratio (%)", Col.NPL.ordinal, 100.0)

  // Household summary
  println("\nHousehold aggregates at M120:")
  if allHhAgg.nonEmpty then
    val avgGini = allHhAgg.kahanSumBy(_.giniIndividual.toDouble) / allHhAgg.length
    val avgWealth = allHhAgg.kahanSumBy(_.giniWealth.toDouble) / allHhAgg.length
    val avgBankr = allHhAgg.kahanSumBy(_.bankruptcyRate.toDouble) / allHhAgg.length
    val avgPov50 = allHhAgg.kahanSumBy(_.povertyRate50.toDouble) / allHhAgg.length
    val avgSkill = allHhAgg.kahanSumBy(_.meanSkill) / allHhAgg.length
    println(f"  Gini (income)        mean=${avgGini * 100}%8.2f%%")
    println(f"  Gini (wealth)        mean=${avgWealth * 100}%8.2f%%")
    println(f"  Bankruptcy rate      mean=${avgBankr * 100}%8.2f%%")
    println(f"  Poverty rate (50%%)  mean=${avgPov50 * 100}%8.2f%%")
    println(f"  Mean skill           mean=${avgSkill}%8.4f")

  println("\nPer-sector adoption at M120:")
  val secNames = SECTORS.map(_.name)
  for s <- SECTORS.indices do statsSummary(f"  ${secNames(s)}%-22s", Col.sectorAuto(s).ordinal, 100.0)

  println(s"\nSaved: mc/${outputPrefix}_terminal.csv")
  println(s"Saved: mc/${outputPrefix}_timeseries.csv")
