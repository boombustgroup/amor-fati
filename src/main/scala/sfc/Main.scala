package sfc

import sfc.config.*

@main def sfcMonteCarlo(
  bdpAmountStr: String,
  nSeedsStr: String,
  outputPrefix: String,
): Unit =
  val bdpAmount = bdpAmountStr.toDouble
  val nSeeds = nSeedsStr.toInt
  val rc = RunConfig(bdpAmount, nSeeds, outputPrefix)
  McRunner.run(rc)
