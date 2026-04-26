package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.config.SimParams
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import zio.{Runtime, Unsafe}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

class McRunnerOutputSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  "runZIO" should "match runSingle CSV outputs on a small deterministic run" in
    withTempDir: outputDir =>
      val rc       = McRunConfig(nSeeds = 2, outputPrefix = "regression", runDurationMonths = 4, runId = "fixed")
      val expected = expectedFiles(rc)

      runToDir(rc, outputDir)

      listFileNames(outputDir) shouldBe expected.keySet
      expected.foreach: (name, lines) =>
        withClue(s"$name: ") {
          Files.readAllLines(outputDir.resolve(name), UTF_8).asScala.toVector shouldBe lines
        }

  private def runToDir(rc: McRunConfig, outputDir: Path): Unit =
    Unsafe.unsafe: unsafe =>
      given Unsafe = unsafe
      Runtime.default.unsafe.run(McRunner.runZIO(rc, outputDir.toFile)).getOrThrowFiberFailure()

  private def expectedFiles(rc: McRunConfig): Map[String, Vector[String]] =
    val results   = (1L to rc.nSeeds.toLong).map: seed =>
      seed -> McRunner.runSingle(seed, rc.runDurationMonths).fold(err => fail(err.toString), identity)
    val summaries = results.toVector.map((seed, result) => McTerminalSummarySchema.fromTerminalState(seed, result.terminalState))

    results.iterator
      .map: (seed, result) =>
        seedFileName(seed, rc) -> expectedSeedLines(result)
      .toMap ++ expectedSummaryFiles(rc, summaries)

  private def expectedSeedLines(result: RunResult): Vector[String] =
    val rows = result.timeSeries.executionMonths.map: month =>
      val row = result.timeSeries.monthRow(month)
      McTimeseriesSchema.csvSchema.render((month, row))
    McTimeseriesSchema.csvSchema.header +: rows

  private def expectedSummaryFiles(rc: McRunConfig, summaries: Vector[McTerminalSummaryRows]): Map[String, Vector[String]] =
    McTerminalSummarySchema.specs
      .map: spec =>
        val fileName = spec.outputFile(new java.io.File("."), rc).getName
        val rows     = summaries.flatMap(_.rowsFor(spec.id)).map(spec.csvSchema.render)
        fileName -> (spec.csvSchema.header +: rows)
      .toMap

  private def filePrefix(rc: McRunConfig): String =
    s"${rc.outputPrefix}_${rc.runId}_${rc.runDurationMonths}m"

  private def seedFileName(seed: Long, rc: McRunConfig): String =
    f"${filePrefix(rc)}_seed${seed}%03d.csv"

  private def listFileNames(outputDir: Path): Set[String] =
    Using.resource(Files.list(outputDir)) { paths =>
      paths.iterator().asScala.map(_.getFileName.toString).toSet
    }

  private def withTempDir[A](f: Path => A): A =
    val outputDir = Files.createTempDirectory("mc-runner-output")
    try f(outputDir)
    finally deleteRecursively(outputDir)

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      Using.resource(Files.walk(path)) { paths =>
        paths.iterator().asScala.toVector.sortBy(_.getNameCount)(using Ordering.Int.reverse).foreach(Files.deleteIfExists)
      }
