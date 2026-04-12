package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import zio.stream.ZStream
import zio.{Scope, ZIO}

import java.io.BufferedWriter
import java.io.File
import java.nio.file.{Files, StandardCopyOption}

private[montecarlo] object McSeedCsv:

  private val operation = "write per-seed CSV"
  private val header    = SimOutput.colNames.mkString(";")

  def writeStreaming[A](
      outputFile: File,
      rows: ZStream[Any, SimError, A],
      render: A => String,
      emptyError: => SimError,
  ): ZIO[Any, SimError, A] =
    val tempFile  = new File(s"${outputFile.getPath}.tmp")
    val writeFile = ZIO.scoped:
      for
        writer    <- openWriter(tempFile)
        _         <- writeLine(writer, header, tempFile)
        maybeLast <- rows.runFoldZIO(Option.empty[A]): (_, row) =>
          writeLine(writer, render(row), tempFile).as(Some(row))
        last      <- maybeLast match
          case Some(value) => ZIO.succeed(value)
          case None        => ZIO.fail(emptyError)
      yield last

    writeFile
      .tap(_ => finalizeFile(tempFile, outputFile))
      .onError(_ => deleteIfExists(tempFile).ignore)

  def renderMonthRow(month: ExecutionMonth, row: Array[Double]): String =
    val sb = new StringBuilder
    sb.append(month.toInt)
    for c <- 1 until SimOutput.nCols do sb.append(f";${row(c)}%.6f")
    sb.toString

  private def openWriter(outputFile: File): ZIO[Scope, SimError, BufferedWriter] =
    ZIO.fromAutoCloseable(
      ZIO
        .attemptBlocking(Files.newBufferedWriter(outputFile.toPath))
        .mapError(outputFailure(s"open $operation writer", outputFile)),
    )

  private def writeLine(writer: BufferedWriter, line: String, outputFile: File): ZIO[Any, SimError, Unit] =
    ZIO
      .attemptBlocking:
        writer.write(line)
        writer.newLine()
      .mapError(outputFailure(operation, outputFile))

  private def finalizeFile(tempFile: File, outputFile: File): ZIO[Any, SimError, Unit] =
    ZIO
      .attemptBlocking:
        Files.move(tempFile.toPath, outputFile.toPath, StandardCopyOption.REPLACE_EXISTING)
      .unit
      .mapError(outputFailure(s"finalize $operation", outputFile))

  private def deleteIfExists(file: File): ZIO[Any, SimError, Unit] =
    ZIO
      .attemptBlocking(Files.deleteIfExists(file.toPath))
      .unit
      .mapError(outputFailure(s"cleanup $operation temp file", file))

  private def outputFailure(operation: String, path: File)(err: Throwable): SimError =
    SimError.OutputFailure(operation, path.getPath, Option(err.getMessage).filter(_.nonEmpty).getOrElse(err.getClass.getSimpleName))
