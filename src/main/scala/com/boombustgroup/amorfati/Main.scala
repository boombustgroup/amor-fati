package com.boombustgroup.amorfati

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.montecarlo.{McRunConfig, McRunner}
import com.boombustgroup.amorfati.util.BuildInfo
import zio.*

// $COVERAGE-OFF$ entry point only
object Main extends ZIOAppDefault:
  override def run: ZIO[ZIOAppArgs, Any, Any] =
    for
      args           <- getArgs
      rc             <- parseArgs(args)
      given SimParams = SimParams.defaults
      _              <- Console.printLine(prepareBanner(rc))
      _              <- McRunner
        .runZIO(rc)
        .catchAll: err =>
          Console.printLineError(err.toString) *> ZIO.fail(err)
    yield ()

  private def prepareBanner(rc: McRunConfig)(using p: SimParams): String =
    val commit   = BuildInfo.gitCommit
    val firms    = String.format(java.util.Locale.US, "%,d", p.pop.firmsCount: Integer)
    val asciiArt = com.github.lalyos.jfiglet.FigletFont.convertOneLine("AMOR-FATI").stripTrailing()
    s"""
       |$asciiArt
       |
       |  SFC-ABM  |  commit: $commit
       |
       |  N=${rc.nSeeds} seeds  |  PLN (NBP)  |  HH=${p.household.count}  |  BANK=multi (7)
       |  $firms firms x 6 sectors (GUS 2024) x ${p.topology.label} x ${rc.runDurationMonths}m
       |
       |  Apache 2.0 | Copyright 2026 BoomBustGroup | www.boombustgroup.com
       |""".stripMargin

  private def parseArgs(args: Chunk[String]): ZIO[Any, IllegalArgumentException, McRunConfig] =
    val usage = "Usage: amor-fati <nSeeds> <prefix> [--duration <months>] [--run-id <id>]"

    def findFlag(name: String): Option[String] =
      val idx = args.indexOf(name)
      if idx >= 0 && idx + 1 < args.length then Some(args(idx + 1)) else None

    def parseInt(value: String, label: String): Either[String, Int] =
      scala.util.Try(value.toInt).toOption.toRight(s"$label must be an integer")

    ZIO
      .fromEither(for
        _           <- Either.cond(args.length >= 2, (), usage)
        nSeedsValue <- args.headOption.toRight(usage)
        nSeeds      <- parseInt(nSeedsValue, "<nSeeds>")
        prefix      <- args.lift(1).toRight(usage)
        runDuration <- findFlag("--duration") match
          case Some(value) => parseInt(value, "--duration")
          case None        => Right(McRunConfig.DefaultRunDuration)
        mcRunConfig <- scala.util
          .Try:
            findFlag("--run-id") match
              case Some(id) => McRunConfig(nSeeds, prefix, runDuration, id)
              case None     => McRunConfig(nSeeds, prefix, runDuration)
          .toEither
          .left
          .map(_.getMessage)
      yield mcRunConfig)
      .mapError(err => new IllegalArgumentException(if err == usage then usage else s"$err\n$usage"))
// $COVERAGE-ON$
