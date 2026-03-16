package com.boombustgroup.amorfati.montecarlo

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/** Runtime configuration: values that depend on CLI arguments. Passed through
  * McRunner and Simulation.step.
  */
case class McRunConfig(
    nSeeds: Int,
    outputPrefix: String,
    runId: String = McRunConfig.autoRunId(),
)

object McRunConfig:
  private val fmt: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmm")

  def autoRunId(): String = LocalDateTime.now().format(fmt)
