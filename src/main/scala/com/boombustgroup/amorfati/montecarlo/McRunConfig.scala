package com.boombustgroup.amorfati.montecarlo

/** Runtime configuration: values that depend on CLI arguments. Passed through
  * McRunner and Simulation.step.
  */
case class McRunConfig(
    nSeeds: Int,
    outputPrefix: String,
)
