package com.boombustgroup.amorfati

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.montecarlo.{McRunConfig, McRunner}
import zio.*

// $COVERAGE-OFF$ entry point only
object Main extends ZIOAppDefault:
  override def run: ZIO[ZIOAppArgs, Any, Any] =
    for
      args           <- getArgs
      _              <- ZIO.when(args.length < 2)(
        ZIO.fail(new IllegalArgumentException("Usage: simulate <nSeeds> <prefix>")),
      )
      nSeeds          = args(0).toInt
      prefix          = args(1)
      given SimParams = SimParams.defaults
      rc              = McRunConfig(nSeeds, prefix)
      _              <- McRunner.runZIO(rc)
    yield ()
// $COVERAGE-ON$
