package com.boombustgroup.amorfati.diagnostics

import com.boombustgroup.amorfati.config.RobustnessScenarios
import com.boombustgroup.amorfati.config.RobustnessScenarios.{Scenario, ScenarioSet}
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.montecarlo.{McRunner, McTimeseriesSchema}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Locale
import scala.util.Try

object SensitivityRobustnessExport:

  final case class Config(
      seedStart: Long = 1L,
      seeds: Int = 2,
      months: Int = 24,
      out: Path = Path.of("target/robustness"),
      scenarioSet: ScenarioSet = ScenarioSet.default,
  ):
    def seedRange: Vector[Long] = Vector.range(seedStart, seedStart + seeds.toLong)

  final case class ExportResult(paths: Vector[Path])

  private final case class MetricDef(id: String, ordinal: Int, description: String)

  private final case class SeedMetric(
      scenarioId: String,
      seed: Long,
      metric: MetricDef,
      terminal: Double,
      pathMin: Double,
      pathMax: Double,
      pathMean: Double,
  )

  private final case class EnvelopeMetric(
      scenarioId: String,
      metric: MetricDef,
      terminalMean: Double,
      terminalMin: Double,
      terminalMax: Double,
      pathMin: Double,
      pathMax: Double,
      pathMean: Double,
  )

  private final case class SensitivityMetric(
      scenarioId: String,
      metric: MetricDef,
      baselineMean: Double,
      scenarioMean: Double,
      delta: Double,
      deltaPct: Option[Double],
  )

  def main(args: Array[String]): Unit =
    parseArgs(args.toVector) match
      case Left(err)     =>
        Console.err.println(err)
        Console.err.println(usage)
        sys.exit(2)
      case Right(config) =>
        run(config) match
          case Left(err)     =>
            Console.err.println(err)
            sys.exit(1)
          case Right(result) =>
            result.paths.foreach(path => println(path.toString))

  def run(config: Config): Either[String, ExportResult] =
    validate(config).flatMap: validConfig =>
      val scenarios = RobustnessScenarios.scenarios(validConfig.scenarioSet)
      val attempts  =
        for
          scenario <- scenarios
          seed     <- validConfig.seedRange
        yield runSeed(scenario, seed, validConfig.months)

      attempts.collectFirst { case Left(err) => err } match
        case Some(err) => Left(err)
        case None      =>
          val seedMetrics        = attempts.collect { case Right(metrics) => metrics }.flatten
          val envelopeMetrics    = summarizeEnvelope(seedMetrics)
          val sensitivityMetrics = summarizeSensitivity(envelopeMetrics)
          val paths              = writeArtifacts(validConfig, scenarios, seedMetrics, envelopeMetrics, sensitivityMetrics)
          Right(ExportResult(paths))

  def parseArgs(args: Vector[String]): Either[String, Config] =
    def missingValue(flag: String): Left[String, Config] = Left(s"Missing value for $flag")

    def loop(rest: Seq[String], config: Config): Either[String, Config] =
      rest match
        case Seq()                                  => Right(config)
        case Seq("--help", _*)                      => Left(usage)
        case Seq(flag, tail*) if knownFlag(flag)    =>
          tail match
            case Seq()                                         => missingValue(flag)
            case Seq(value, _*) if value.startsWith("--")      => missingValue(flag)
            case Seq(value, next*) if flag == "--seed-start"   =>
              parseLong(value, flag).flatMap(seedStart => loop(next, config.copy(seedStart = seedStart)))
            case Seq(value, next*) if flag == "--seeds"        =>
              parseInt(value, flag).flatMap(seeds => loop(next, config.copy(seeds = seeds)))
            case Seq(value, next*) if flag == "--months"       =>
              parseInt(value, flag).flatMap(months => loop(next, config.copy(months = months)))
            case Seq(value, next*) if flag == "--out"          =>
              loop(next, config.copy(out = Path.of(value)))
            case Seq(value, next*) if flag == "--scenario-set" =>
              ScenarioSet.parse(value).flatMap(scenarioSet => loop(next, config.copy(scenarioSet = scenarioSet)))
            case Seq(_, _*)                                    => Left(s"Unknown argument: $flag")
        case Seq(flag, _*) if flag.startsWith("--") => Left(s"Unknown argument: $flag")
        case Seq(value, _*)                         => Left(s"Unexpected positional argument: $value")

    loop(args, Config())

  private def validate(config: Config): Either[String, Config] =
    Either
      .cond(config.seeds > 0, config, "--seeds must be a positive integer")
      .flatMap(valid => Either.cond(valid.months > 0, valid, "--months must be a positive integer"))

  private def runSeed(scenario: Scenario, seed: Long, months: Int): Either[String, Vector[SeedMetric]] =
    given SimParams = scenario.params
    McRunner
      .runSingle(seed, months)
      .left
      .map(err => s"Scenario ${scenario.id}, seed $seed failed: $err")
      .map: result =>
        Metrics.map: metric =>
          val values = result.timeSeries.map(row => row(metric.ordinal))
          SeedMetric(
            scenario.id,
            seed,
            metric,
            terminal = values.last,
            pathMin = values.min,
            pathMax = values.max,
            pathMean = values.sum / values.length.toDouble,
          )

  private def summarizeEnvelope(seedMetrics: Vector[SeedMetric]): Vector[EnvelopeMetric] =
    seedMetrics
      .groupBy(metric => metric.scenarioId -> metric.metric.id)
      .toVector
      .sortBy { case ((scenarioId, metricId), _) => scenarioId -> metricId }
      .map { case ((scenarioId, _), metrics) =>
        val metric    = metrics.head.metric
        val terminals = metrics.map(_.terminal)
        EnvelopeMetric(
          scenarioId = scenarioId,
          metric = metric,
          terminalMean = terminals.sum / terminals.length.toDouble,
          terminalMin = terminals.min,
          terminalMax = terminals.max,
          pathMin = metrics.map(_.pathMin).min,
          pathMax = metrics.map(_.pathMax).max,
          pathMean = metrics.map(_.pathMean).sum / metrics.length.toDouble,
        )
      }

  private def summarizeSensitivity(envelopeMetrics: Vector[EnvelopeMetric]): Vector[SensitivityMetric] =
    val baseline = envelopeMetrics.filter(_.scenarioId == "baseline").map(metric => metric.metric.id -> metric).toMap
    envelopeMetrics
      .filterNot(_.scenarioId == "baseline")
      .flatMap: metric =>
        baseline
          .get(metric.metric.id)
          .map: base =>
            val delta = metric.terminalMean - base.terminalMean
            SensitivityMetric(
              scenarioId = metric.scenarioId,
              metric = metric.metric,
              baselineMean = base.terminalMean,
              scenarioMean = metric.terminalMean,
              delta = delta,
              deltaPct = if Math.abs(base.terminalMean) > 1e-12 then Some(delta / Math.abs(base.terminalMean)) else None,
            )

  private def writeArtifacts(
      config: Config,
      scenarios: Vector[Scenario],
      seedMetrics: Vector[SeedMetric],
      envelopeMetrics: Vector[EnvelopeMetric],
      sensitivityMetrics: Vector[SensitivityMetric],
  ): Vector[Path] =
    Files.createDirectories(config.out)
    val seedMetricsPath = config.out.resolve("seed-metrics.csv")
    val envelopePath    = config.out.resolve("envelope-summary.csv")
    val sensitivityPath = config.out.resolve("sensitivity-summary.csv")
    val reportPath      = config.out.resolve("robustness-report.md")

    Files.writeString(seedMetricsPath, renderSeedMetrics(seedMetrics), StandardCharsets.UTF_8)
    Files.writeString(envelopePath, renderEnvelopeMetrics(envelopeMetrics), StandardCharsets.UTF_8)
    Files.writeString(sensitivityPath, renderSensitivityMetrics(sensitivityMetrics), StandardCharsets.UTF_8)
    Files.writeString(reportPath, renderReport(config, scenarios, envelopeMetrics, sensitivityMetrics), StandardCharsets.UTF_8)

    Vector(seedMetricsPath, envelopePath, sensitivityPath, reportPath)

  private def renderSeedMetrics(seedMetrics: Vector[SeedMetric]): String =
    val header = "Scenario;Seed;Metric;Terminal;PathMin;PathMax;PathMean"
    val rows   = seedMetrics.map: row =>
      Vector(row.scenarioId, row.seed.toString, row.metric.id, fmt(row.terminal), fmt(row.pathMin), fmt(row.pathMax), fmt(row.pathMean)).mkString(";")
    (header +: rows).mkString("\n") + "\n"

  private def renderEnvelopeMetrics(envelopeMetrics: Vector[EnvelopeMetric]): String =
    val header = "Scenario;Metric;TerminalMean;TerminalMin;TerminalMax;PathMin;PathMax;PathMean"
    val rows   = envelopeMetrics.map: row =>
      Vector(
        row.scenarioId,
        row.metric.id,
        fmt(row.terminalMean),
        fmt(row.terminalMin),
        fmt(row.terminalMax),
        fmt(row.pathMin),
        fmt(row.pathMax),
        fmt(row.pathMean),
      ).mkString(";")
    (header +: rows).mkString("\n") + "\n"

  private def renderSensitivityMetrics(sensitivityMetrics: Vector[SensitivityMetric]): String =
    val header = "Scenario;Metric;BaselineTerminalMean;ScenarioTerminalMean;Delta;DeltaPct"
    val rows   = sensitivityMetrics.map: row =>
      Vector(
        row.scenarioId,
        row.metric.id,
        fmt(row.baselineMean),
        fmt(row.scenarioMean),
        fmt(row.delta),
        row.deltaPct.map(fmt).getOrElse("NA"),
      ).mkString(";")
    (header +: rows).mkString("\n") + "\n"

  private def renderReport(
      config: Config,
      scenarios: Vector[Scenario],
      envelopeMetrics: Vector[EnvelopeMetric],
      sensitivityMetrics: Vector[SensitivityMetric],
  ): String =
    val baselineRows = envelopeMetrics
      .filter(_.scenarioId == "baseline")
      .map: row =>
        markdownRow(Vector(row.metric.id, fmt(row.terminalMean), fmt(row.terminalMin), fmt(row.terminalMax), fmt(row.pathMin), fmt(row.pathMax)))

    val sensitivityRows = sensitivityMetrics.map: row =>
      markdownRow(
        Vector(
          row.scenarioId,
          row.metric.id,
          fmt(row.baselineMean),
          fmt(row.scenarioMean),
          signed(row.delta),
          row.deltaPct.map(p => signed(p * 100.0) + "%").getOrElse("NA"),
        ),
      )

    val scenarioRows = scenarios.map: scenario =>
      markdownRow(Vector(scenario.id, scenario.label, scenario.category, scenario.variedParameter, scenario.variation, scenario.rationale))

    val metricRows = Metrics.map: metric =>
      markdownRow(Vector(metric.id, metric.description))

    val lines =
      Vector(
        "# Sensitivity And Robustness Report",
        "",
        "Generated by `SensitivityRobustnessExport`.",
        "",
        "## Run Configuration",
        "",
        s"- Scenario set: `${config.scenarioSet.cliName}`",
        s"- Seeds: `${config.seedRange.head}` to `${config.seedRange.last}` (${config.seeds} seeds)",
        s"- Months: `${config.months}`",
        s"- Output directory: `${config.out}`",
        "",
        "## Scenario Sweep",
        "",
        markdownRow(Vector("Scenario", "Label", "Category", "Varied parameter", "Variation", "Rationale")),
        markdownRow(Vector("---", "---", "---", "---", "---", "---")),
      ) ++ scenarioRows ++ Vector(
        "",
        "## Metrics",
        "",
        markdownRow(Vector("Metric", "Description")),
        markdownRow(Vector("---", "---")),
      ) ++ metricRows ++ Vector(
        "",
        "## Stochastic Uncertainty",
        "",
        "Baseline seed variation is summarized below. `TerminalMin` and",
        "`TerminalMax` are cross-seed terminal envelopes; `PathMin` and `PathMax`",
        "are the full within-run path envelope across all baseline seeds.",
        "",
        markdownRow(Vector("Metric", "TerminalMean", "TerminalMin", "TerminalMax", "PathMin", "PathMax")),
        markdownRow(Vector("---", "---", "---", "---", "---", "---")),
      ) ++ baselineRows ++ Vector(
        "",
        "## Parameter Sensitivity",
        "",
        "The table compares each non-baseline scenario's terminal cross-seed mean",
        "against the baseline terminal cross-seed mean. This is one-at-a-time",
        "sensitivity, not a full global sensitivity design.",
        "",
        markdownRow(Vector("Scenario", "Metric", "BaselineMean", "ScenarioMean", "Delta", "DeltaPct")),
        markdownRow(Vector("---", "---", "---", "---", "---", "---")),
      ) ++ sensitivityRows ++ Vector(
        "",
        "## Runtime Guidance",
        "",
        """- Smoke check: `sbt "robustnessReport --scenario-set smoke --seeds 1 --months 6 --out target/robustness-smoke"`""",
        """- Local review default: `sbt "robustnessReport --scenario-set core --seeds 2 --months 24 --out target/robustness"`""",
        "- Heavier review run: increase to 5-10 seeds and 60-120 months after checking runtime locally.",
        "",
        "## Output Files",
        "",
        "- `seed-metrics.csv`: per scenario, seed, and metric terminal/path statistics.",
        "- `envelope-summary.csv`: cross-seed envelopes by scenario and metric.",
        "- `sensitivity-summary.csv`: terminal mean deltas versus baseline.",
        "- `robustness-report.md`: this human-readable summary.",
      )

    lines.mkString("\n") + "\n"

  private def knownFlag(flag: String): Boolean =
    flag == "--seed-start" || flag == "--seeds" || flag == "--months" || flag == "--out" || flag == "--scenario-set"

  private def parseLong(value: String, name: String): Either[String, Long] =
    Try(value.toLong).toEither.left.map(_ => s"$name must be a long integer")

  private def parseInt(value: String, name: String): Either[String, Int] =
    Try(value.toInt).toEither.left.map(_ => s"$name must be an integer")

  private def markdownRow(values: Vector[String]): String =
    values.map(escapeMarkdown).mkString("| ", " | ", " |")

  private def escapeMarkdown(value: String): String =
    value.replace("|", "\\|")

  private def fmt(value: Double): String =
    String.format(Locale.US, "%.8f", Double.box(value))

  private def signed(value: Double): String =
    String.format(Locale.US, "%+.8f", Double.box(value))

  private def metric(column: String, description: String): MetricDef =
    val ordinal = McTimeseriesSchema.colNames.indexOf(column)
    require(ordinal >= 0, s"Unknown Monte Carlo output column: $column")
    MetricDef(column, ordinal, description)

  private val Metrics: Vector[MetricDef] = Vector(
    metric("Inflation", "annualized inflation rate"),
    metric("Unemployment", "unemployment share"),
    metric("MarketWage", "aggregate market wage"),
    metric("DebtToGdp", "government debt to annualized GDP"),
    metric("DeficitToGdp", "government deficit to annualized GDP"),
    metric("CreditToGdpGap", "macroprudential credit-to-GDP gap"),
    metric("MinBankCAR", "minimum bank capital adequacy ratio"),
    metric("MinBankLCR", "minimum bank liquidity coverage ratio"),
    metric("CurrentAccount", "current account flow"),
    metric("ExRate", "PLN/EUR exchange rate"),
    metric("TotalAdoption", "automation plus hybrid adoption share"),
    metric("FirmDeaths", "monthly firm deaths"),
    metric("BankFailures", "failed bank count"),
  )

  private val usage: String =
    "Usage: SensitivityRobustnessExport [--seed-start <long>] [--seeds <int>] [--months <int>] [--out <path>] [--scenario-set smoke|core]"

end SensitivityRobustnessExport
