package sfc

import sfc.config.*

@main def sfcMonteCarlo(
  bdpAmountStr: String,
  nSeedsStr: String,
  outputPrefix: String,
  regimeStr: String = "pln",
): Unit =
  val bdpAmount = bdpAmountStr.toDouble
  val nSeeds = nSeedsStr.toInt
  val regime = regimeStr.toLowerCase match
    case "eur" | "euro" | "ecb" => MonetaryRegime.Eur
    case _                      => MonetaryRegime.Pln
  val rc = RunConfig(bdpAmount, nSeeds, outputPrefix, regime)
  McRunner.run(rc)
