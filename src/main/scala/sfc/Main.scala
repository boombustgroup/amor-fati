package sfc

import sfc.config.SimParams

// $COVERAGE-OFF$ entry point only
@main def sfcMonteCarlo(
    nSeedsStr: String,
    outputPrefix: String,
): Unit =
  given SimParams = SimParams.defaults
  val nSeeds      = nSeedsStr.toInt
  val rc          = McRunConfig(nSeeds, outputPrefix)
  McRunner.run(rc)
// $COVERAGE-ON$
