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
          Right(ExportResult(bundle, paths))

  def parseArgs(args: Vector[String]): Either[String, Config] =
    def missingValue(flag: String): Left[String, Config] =
      Left(s"Missing value for ${if flag == "--formats" then "--format" else flag}")

    def loop(rest: Vector[String], config: Config): Either[String, Config] =
      rest match
        case Vector()                                  => Right(config)
        case Vector("--help", _*)                      => Left(usage)
        case Vector(flag, tail*) if knownFlag(flag)    =>
          tail.toVector match
            case Vector()                                    => missingValue(flag)
            case Vector(value, _*) if value.startsWith("--") => missingValue(flag)
            case Vector(value, next*) if flag == "--seed"    =>
              parseLong(value, flag).flatMap(seed => loop(next.toVector, config.copy(seed = seed)))
            case Vector(value, next*) if flag == "--months"  =>
              parseInt(value, flag).flatMap(months => loop(next.toVector, config.copy(months = months)))
            case Vector(value, next*) if flag == "--out"     =>
              loop(next.toVector, config.copy(out = Path.of(value)))
            case Vector(value, next*)                        =>
              OutputFormat.parseList(value).flatMap(formats => loop(next.toVector, config.copy(formats = formats)))
        case Vector(flag, _*) if flag.startsWith("--") => Left(s"Unknown argument: $flag")
        case Vector(value, _*)                         => Left(s"Unexpected positional argument: $value")

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
