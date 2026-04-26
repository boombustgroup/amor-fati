package com.boombustgroup.amorfati.diagnostics

import com.boombustgroup.amorfati.accounting.matrix.SfcMatrixEvidence.MatrixEvidenceBundle
import com.boombustgroup.amorfati.accounting.matrix.SfcMatrixRenderers
import com.boombustgroup.amorfati.accounting.matrix.SfcMatrixRenderers.OutputFormat
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.{MonthDriver, MonthRandomness}
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}

import java.nio.file.Path
import scala.util.Try

object SfcMatrixExport:

  final case class Config(
      seed: Long = 1L,
      months: Int = 12,
      out: Path = Path.of("target/sfc-matrices"),
      formats: Vector[OutputFormat] = OutputFormat.Default,
  )

  final case class ExportResult(
      bundle: MatrixEvidenceBundle,
      paths: Vector[Path],
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
            if !result.bundle.validation.isValid then sys.exit(1)

  def run(config: Config): Either[String, ExportResult] =
    given SimParams = SimParams.defaults

    if config.months <= 0 then Left("--months must be a positive integer")
    else
      val initState = FlowSimulation.SimState.fromInit(WorldInit.initialize(InitRandomness.Contract.fromSeed(config.seed)))
      val steps     = MonthDriver
        .unfoldSteps(initState): state =>
          Some(MonthRandomness.Contract.fromSeed(config.seed * 1000L + state.completedMonth.toLong + 1L))
        .take(config.months)
        .toVector

      steps.lastOption match
        case None       => Left("No simulation step was produced")
        case Some(step) =>
          val bundle = MatrixEvidenceBundle.fromStep(config.seed, step)
          val paths  = SfcMatrixRenderers.writeSymbolicBundle(bundle, config.out, config.formats)
          val status =
            if bundle.validation.isValid then Right(ExportResult(bundle, paths))
            else Left(s"SFC matrix validation failed; artifacts were written to ${config.out}")
          status

  def parseArgs(args: Vector[String]): Either[String, Config] =
    def missingValue(flag: String): Left[String, Config] =
      Left(s"Missing value for ${if flag == "--formats" then "--format" else flag}")

    def loop(rest: Vector[String], config: Config): Either[String, Config] =
      rest match
        case Vector()                           => Right(config)
        case "--help" +: _                      => Left(usage)
        case flag +: tail if knownFlag(flag)    =>
          tail match
            case Vector()                                                   => missingValue(flag)
            case value +: _ if value.startsWith("--")                       => missingValue(flag)
            case value +: next if flag == "--seed"                          =>
              parseLong(value, flag).flatMap(seed => loop(next, config.copy(seed = seed)))
            case value +: next if flag == "--months"                        =>
              parseInt(value, flag).flatMap(months => loop(next, config.copy(months = months)))
            case value +: next if flag == "--out"                           =>
              loop(next, config.copy(out = Path.of(value)))
            case value +: next if flag == "--format" || flag == "--formats" =>
              OutputFormat.parseList(value).flatMap(formats => loop(next, config.copy(formats = formats)))
            case _                                                          => Left(s"Unknown argument: $flag")
        case flag +: _ if flag.startsWith("--") => Left(s"Unknown argument: $flag")
        case value +: _                         => Left(s"Unexpected positional argument: $value")
        case invalid                            => Left(s"Invalid argument list: ${invalid.mkString(" ")}")

    loop(args, Config())

  private def knownFlag(flag: String): Boolean =
    flag == "--seed" || flag == "--months" || flag == "--out" || flag == "--format" || flag == "--formats"

  private def parseLong(value: String, name: String): Either[String, Long] =
    Try(value.toLong).toEither.left.map(_ => s"$name must be a long integer")

  private def parseInt(value: String, name: String): Either[String, Int] =
    Try(value.toInt).toEither.left.map(_ => s"$name must be an integer")

  private val usage: String =
    "Usage: SfcMatrixExport [--seed <long>] [--months <int>] [--out <path>] [--format tex,md]"

end SfcMatrixExport
