package com.boombustgroup.amorfati.diagnostics

import com.boombustgroup.amorfati.config.ScenarioRegistry
import com.boombustgroup.amorfati.config.ScenarioRegistry.ScenarioSpec
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.montecarlo.{McRunner, McTimeseriesSchema, MetricValue}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Try

object ScenarioRunExport:

  final case class Config(
      scenarios: Vector[ScenarioSpec] = ScenarioRegistry.defaultScenarioIds.flatMap(id => ScenarioRegistry.get(id).toOption),
      seedStart: Long = 1L,
      seeds: Int = 1,
      months: Int = 12,
      runId: String = "scenario-registry",
      out: Path = Path.of("target/scenarios"),
  ):
    def seedRange: Vector[Long] = Vector.range(seedStart, seedStart + seeds.toLong)
    def runRoot: Path           = out.resolve(runId)

  final case class ExportResult(paths: Vector[Path])

  private final case class MetricDef(id: String, ordinal: Int)

  private final case class TerminalMetric(
      scenarioId: String,
      seed: Long,
      metric: MetricDef,
      value: MetricValue,
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
      Files.createDirectories(validConfig.runRoot)

      val registryPath  = validConfig.runRoot.resolve("scenario-registry.md")
      val deltasPath    = validConfig.runRoot.resolve("scenario-deltas.csv")
      Files.writeString(registryPath, renderRegistry(validConfig.scenarios), StandardCharsets.UTF_8)
      Files.writeString(deltasPath, renderDeltas(validConfig.scenarios), StandardCharsets.UTF_8)
      val metadataPaths = validConfig.scenarios.map: scenario =>
        val scenarioDir  = validConfig.runRoot.resolve(scenario.id)
        val metadataPath = scenarioDir.resolve("metadata.md")
        Files.createDirectories(scenarioDir)
        Files.writeString(metadataPath, renderScenarioMetadata(validConfig, scenario), StandardCharsets.UTF_8)
        metadataPath

      val runAttempts =
        for
          scenario <- validConfig.scenarios
          seed     <- validConfig.seedRange
        yield runSeed(validConfig, scenario, seed)

      runAttempts.collectFirst { case Left(err) => err } match
        case Some(err) => Left(err)
        case None      =>
          val outputs = runAttempts.collect { case Right(paths, metrics) => paths -> metrics }
          val paths   = Vector(registryPath, deltasPath) ++ metadataPaths ++ outputs.flatMap(_._1)
          val metrics = outputs.flatMap(_._2)
          val summary = validConfig.runRoot.resolve("run-summary.csv")
          Files.writeString(summary, renderRunSummary(metrics), StandardCharsets.UTF_8)
          Right(ExportResult(paths :+ summary))

  def parseArgs(args: Vector[String]): Either[String, Config] =
    def missingValue(flag: String): Left[String, Config] = Left(s"Missing value for $flag")

    def loop(rest: Seq[String], config: Config): Either[String, Config] =
      rest match
        case Seq()                                  => Right(config)
        case Seq("--help", _*)                      => Left(usage)
        case Seq(flag, tail*) if knownFlag(flag)    =>
          tail match
            case Seq()                                                              => missingValue(flag)
            case Seq(value, _*) if value.startsWith("--")                           => missingValue(flag)
            case Seq(value, next*) if flag == "--scenario" || flag == "--scenarios" =>
              ScenarioRegistry.select(value).flatMap(scenarios => loop(next, config.copy(scenarios = scenarios)))
            case Seq(value, next*) if flag == "--seed-start"                        =>
              parseLong(value, flag).flatMap(seedStart => loop(next, config.copy(seedStart = seedStart)))
            case Seq(value, next*) if flag == "--seeds"                             =>
              parseInt(value, flag).flatMap(seeds => loop(next, config.copy(seeds = seeds)))
            case Seq(value, next*) if flag == "--months"                            =>
              parseInt(value, flag).flatMap(months => loop(next, config.copy(months = months)))
            case Seq(value, next*) if flag == "--run-id"                            =>
              loop(next, config.copy(runId = value))
            case Seq(value, next*) if flag == "--out"                               =>
              loop(next, config.copy(out = Path.of(value)))
            case Seq(_, _*)                                                         => Left(s"Unknown argument: $flag")
        case Seq(flag, _*) if flag.startsWith("--") => Left(s"Unknown argument: $flag")
        case Seq(value, _*)                         => Left(s"Unexpected positional argument: $value")

    loop(args, Config())

  private def validate(config: Config): Either[String, Config] =
    Either
      .cond(config.scenarios.nonEmpty, config, "--scenarios must contain at least one scenario")
      .flatMap(valid => Either.cond(valid.seeds > 0, valid, "--seeds must be a positive integer"))
      .flatMap(valid => Either.cond(valid.months > 0, valid, "--months must be a positive integer"))
      .flatMap(valid => Either.cond(valid.runId.trim.nonEmpty, valid, "--run-id must be non-empty"))

  private def runSeed(config: Config, scenario: ScenarioSpec, seed: Long): Either[String, (Vector[Path], Vector[TerminalMetric])] =
    given SimParams = scenario.params

    McRunner
      .runSingle(seed, config.months)
      .left
      .map(err => s"Scenario ${scenario.id}, seed $seed failed: $err")
      .map: result =>
        val scenarioDir = config.runRoot.resolve(scenario.id)
        Files.createDirectories(scenarioDir)

        val csvPath = scenarioDir.resolve(f"${config.runId}_${scenario.id}_${config.months}m_seed$seed%03d.csv")
        val rows    = result.timeSeries.map(row => row)
        Files.writeString(csvPath, renderTimeSeriesCsv(rows), StandardCharsets.UTF_8)

        val terminal = rows.last
        val metrics  = Metrics.map(metric => TerminalMetric(scenario.id, seed, metric, terminal(metric.ordinal)))
        Vector(csvPath) -> metrics

  private def renderRegistry(scenarios: Vector[ScenarioSpec]): String =
    val rows = scenarios.map: scenario =>
      markdownRow(
        Vector(
          scenario.id,
          scenario.label,
          scenario.category,
          scenario.recommendedMonths.toString,
          scenario.seedPolicy,
          scenario.outputFolder,
        ),
      )

    val lines = Vector(
      "# Scenario Registry",
      "",
      "Generated by `ScenarioRunExport` from `ScenarioRegistry`.",
      "",
      markdownRow(Vector("Scenario", "Label", "Category", "Recommended months", "Seed policy", "Output folder")),
      markdownRow(Vector("---", "---", "---", "---", "---", "---")),
    ) ++ rows

    lines.mkString("\n") + "\n"

  private def renderDeltas(scenarios: Vector[ScenarioSpec]): String =
    val header = "Scenario;Parameter;Baseline;ScenarioValue;Note"
    val rows   =
      for
        scenario <- scenarios
        delta    <- scenario.deltas
      yield Vector(scenario.id, delta.parameter, delta.baseline, delta.scenario, delta.note).map(csv).mkString(";")
    (header +: rows).mkString("\n") + "\n"

  private def renderRunSummary(metrics: Vector[TerminalMetric]): String =
    val header = "Scenario;Seed;Metric;TerminalValue"
    val rows   = metrics.map: row =>
      Vector(row.scenarioId, row.seed.toString, row.metric.id, fmt(row.value)).mkString(";")
    (header +: rows).mkString("\n") + "\n"

  private def renderScenarioMetadata(config: Config, scenario: ScenarioSpec): String =
    val channelRows = scenario.expectedChannels.map(channel => s"- `$channel`")
    val deltaRows   = scenario.deltas.map: delta =>
      markdownRow(Vector(delta.parameter, delta.baseline, delta.scenario, delta.note))
    val lines       =
      Vector(
        s"# ${scenario.label}",
        "",
        s"- Scenario id: `${scenario.id}`",
        s"- Category: `${scenario.category}`",
        s"- Purpose: ${scenario.purpose}",
        s"- Run id: `${config.runId}`",
        s"- Months in this run: `${config.months}`",
        s"- Recommended months: `${scenario.recommendedMonths}`",
        s"- Seed policy: ${scenario.seedPolicy}",
        s"- Output folder: `${config.runRoot.resolve(scenario.id)}`",
        "",
        "## Expected Channels",
        "",
      ) ++ channelRows ++ Vector(
        "",
        "## Parameter Deltas",
        "",
        markdownRow(Vector("Parameter", "Baseline", "Scenario", "Note")),
        markdownRow(Vector("---", "---", "---", "---")),
      ) ++ deltaRows

    lines.mkString("\n") + "\n"

  private def renderTimeSeriesCsv(rows: Vector[Array[MetricValue]]): String =
    val header = McTimeseriesSchema.colNames.mkString(";")
    val body   = rows.map: row =>
      row.indices
        .map: idx =>
          if idx == 0 then row(idx).format(0)
          else fmt(row(idx))
        .mkString(";")
    (header +: body).mkString("\n") + "\n"

  private def knownFlag(flag: String): Boolean =
    flag == "--scenario" || flag == "--scenarios" || flag == "--seed-start" || flag == "--seeds" || flag == "--months" || flag == "--run-id" || flag == "--out"

  private def parseLong(value: String, name: String): Either[String, Long] =
    Try(value.toLong).toEither.left.map(_ => s"$name must be a long integer")

  private def parseInt(value: String, name: String): Either[String, Int] =
    Try(value.toInt).toEither.left.map(_ => s"$name must be an integer")

  private def markdownRow(values: Vector[String]): String =
    values.map(_.replace("|", "\\|")).mkString("| ", " | ", " |")

  private def csv(value: String): String =
    if value.exists(ch => ch == ';' || ch == '"' || ch == '\n') then "\"" + value.replace("\"", "\"\"") + "\""
    else value

  private def fmt(value: MetricValue): String =
    value.format(8)

  private def metric(column: String): MetricDef =
    val ordinal = McTimeseriesSchema.colNames.indexOf(column)
    require(ordinal >= 0, s"Unknown Monte Carlo output column: $column")
    MetricDef(column, ordinal)

  private val Metrics: Vector[MetricDef] = Vector(
    metric("Inflation"),
    metric("Unemployment"),
    metric("MarketWage"),
    metric("DebtToGdp"),
    metric("DeficitToGdp"),
    metric("CurrentAccount"),
    metric("ExRate"),
    metric("CreditToGdpGap"),
    metric("MinBankCAR"),
    metric("MinBankLCR"),
    metric("BankFailures"),
    metric("FirmDeaths"),
  )

  private val usage: String =
    "Usage: ScenarioRunExport [--scenarios baseline,monetary-tightening|all] [--seed-start <long>] [--seeds <int>] [--months <int>] [--run-id <id>] [--out <path>]"

end ScenarioRunExport
