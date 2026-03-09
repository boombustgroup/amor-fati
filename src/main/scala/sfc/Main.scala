package sfc

import sfc.config.*

@main def sfcMonteCarlo(
    nSeedsStr: String,
    outputPrefix: String,
): Unit =
  given SimParams = SimParams.defaults
  val nSeeds      = nSeedsStr.toInt
  val rc          = RunConfig(nSeeds, outputPrefix)
  McRunner.run(rc)
