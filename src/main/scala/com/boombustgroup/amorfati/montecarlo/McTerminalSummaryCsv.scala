package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.util.CsvWriter
import zio.ZIO

import java.io.File

private[montecarlo] object McTerminalSummaryCsv:

  def writeAll(rc: McRunConfig, outputDir: File, results: zio.Chunk[McTerminalSummaryRows]): ZIO[Any, SimError, Unit] =
    val sorted = results.sortBy(_.seed)
    ZIO.foreachDiscard(McTerminalSummarySchema.specs)(spec => writeSummaryCsv(spec, rc, outputDir, sorted))

  private def writeSummaryCsv(
      spec: McTerminalSummarySchema.SummarySpec,
      rc: McRunConfig,
      outputDir: File,
      sortedResults: zio.Chunk[McTerminalSummaryRows],
  ) =
    val outputFile = spec.outputFile(outputDir, rc)
    ZIO
      .attemptBlocking:
        val rows = sortedResults.flatMap(_.rowsFor(spec.id))
        CsvWriter.write(outputFile, spec.csvSchema.header, rows)(spec.csvSchema.render)
      .mapError(outputFailure(s"write ${spec.id.toString.toLowerCase} summary CSV", outputFile))

  private def outputFailure(operation: String, path: File)(err: Throwable): SimError =
    SimError.OutputFailure(operation, path.getPath, Option(err.getMessage).filter(_.nonEmpty).getOrElse(err.getClass.getSimpleName))
