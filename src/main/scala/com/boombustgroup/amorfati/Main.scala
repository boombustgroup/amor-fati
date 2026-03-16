package com.boombustgroup.amorfati

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.montecarlo.{McRunConfig, McRunner}
import zio.*

// $COVERAGE-OFF$ entry point only
object Main extends ZIOAppDefault:
  override def run: ZIO[ZIOAppArgs, Any, Any] =
    for
      args           <- getArgs
      rc             <- parseArgs(args)
      given SimParams = SimParams.defaults
      _              <- McRunner.runZIO(rc)
    yield ()

  private def parseArgs(args: Chunk[String]): ZIO[Any, IllegalArgumentException, McRunConfig] =
    val usage = "Usage: simulate <nSeeds> <prefix> [--run-id <id>]"

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
