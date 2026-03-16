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
       |  $firms firms x 6 sectors (GUS 2024) x ${p.topology.label} x ${p.timeline.duration}m
       |
       |  Apache 2.0 | Copyright 2026 BoomBustGroup | www.boombustgroup.com
       |""".stripMargin

  private def parseArgs(args: Chunk[String]): ZIO[Any, IllegalArgumentException, McRunConfig] =
    val usage = "Usage: amor-fati <nSeeds> <prefix> [--run-id <id>]"

    def findFlag(name: String): Option[String] =
      val idx = args.indexOf(name)
      if idx >= 0 && idx + 1 < args.length then Some(args(idx + 1)) else None

    ZIO
      .fromOption(for
        _      <- Option.when(args.length >= 2)(())
        nSeeds <- args.headOption.flatMap(s => scala.util.Try(s.toInt).toOption)
        prefix <- args.lift(1)
      yield
        val runId = findFlag("--run-id")
        runId match
          case Some(id) => McRunConfig(nSeeds, prefix, id)
          case None     => McRunConfig(nSeeds, prefix))
      .mapError(_ => new IllegalArgumentException(usage))
// $COVERAGE-ON$
