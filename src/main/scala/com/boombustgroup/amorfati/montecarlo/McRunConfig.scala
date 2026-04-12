package com.boombustgroup.amorfati.montecarlo

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/** Runtime configuration: values that depend on CLI arguments. Passed through
  * McRunner and Simulation.step.
  */
case class McRunConfig(
    nSeeds: Int,
    outputPrefix: String,
    runDurationMonths: Int = McRunConfig.DefaultRunDuration,
    runId: String = McRunConfig.autoRunId(),
):
  McRunConfig.requirePositiveSeeds(nSeeds)
  McRunConfig.requireNonBlankOutputPrefix(outputPrefix)
  McRunConfig.requirePositiveDuration(runDurationMonths)
  McRunConfig.requireNonBlankRunId(runId)

object McRunConfig:
  val DefaultRunDuration: Int        = 120
  private val fmt: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmm")

  def autoRunId(): String = autoRunId(LocalDateTime.now())

  def autoRunId(at: LocalDateTime): String = at.format(fmt)

  def requirePositiveSeeds(nSeeds: Int): Unit =
    require(nSeeds > 0, s"nSeeds must be > 0, got $nSeeds")

  def requireNonBlankOutputPrefix(outputPrefix: String): Unit =
    require(outputPrefix != null && outputPrefix.trim.nonEmpty, "outputPrefix must be non-blank")

  def requirePositiveDuration(runDurationMonths: Int): Unit =
    require(runDurationMonths > 0, s"runDurationMonths must be > 0, got $runDurationMonths")

  def requireNonBlankRunId(runId: String): Unit =
    require(runId != null && runId.trim.nonEmpty, "runId must be non-blank")
