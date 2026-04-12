package com.boombustgroup.amorfati.montecarlo

import zio.ZIO

import java.io.File

private[montecarlo] object McOutputFiles:

  def prepareOutputDir(outputDir: File): ZIO[Any, SimError, Unit] =
    ZIO
      .attemptBlocking:
        if outputDir.exists() then
          if !outputDir.isDirectory then throw java.io.IOException(s"path exists but is not a directory: ${outputDir.getPath}")
        else if !outputDir.mkdirs() && (!outputDir.exists() || !outputDir.isDirectory) then
          throw java.io.IOException(s"failed to create output directory: ${outputDir.getPath}")
      .mapError(outputFailure("prepare output directory", outputDir))

  def seedFile(outputDir: File, seed: Long, rc: McRunConfig): File =
    new File(outputDir, f"${filePrefix(rc)}_seed${seed}%03d.csv")

  def householdFile(outputDir: File, rc: McRunConfig): File =
    new File(outputDir, s"${filePrefix(rc)}_hh.csv")

  def bankFile(outputDir: File, rc: McRunConfig): File =
    new File(outputDir, s"${filePrefix(rc)}_banks.csv")

  def savedFiles(outputDir: File, rc: McRunConfig): Vector[File] =
    (1L to rc.nSeeds.toLong).map(seed => seedFile(outputDir, seed, rc)).toVector ++
      Vector(householdFile(outputDir, rc), bankFile(outputDir, rc))

  private def filePrefix(rc: McRunConfig): String =
    s"${rc.outputPrefix}_${rc.runId}_${rc.runDurationMonths}m"

  private def outputFailure(operation: String, path: File)(err: Throwable): SimError =
    SimError.OutputFailure(operation, path.getPath, Option(err.getMessage).filter(_.nonEmpty).getOrElse(err.getClass.getSimpleName))
