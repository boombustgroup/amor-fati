package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class McRunnerConsoleSpec extends AnyFlatSpec with Matchers:

  private val fullBlock  = "\u2588"
  private val emptyBlock = "\u2591"

  "McRunnerConsole.render" should "format month progress consistently" in {
    val monthProgress = McRunnerConsole.Event.MonthProgress(
      seed = 7L,
      totalSeeds = 12,
      month = ExecutionMonth.First.advanceBy(1),
      durationMonths = 4,
    )

    McRunnerConsole.render(monthProgress) shouldBe
      s"\r  Seed   7/12 [${fullBlock * 10}${emptyBlock * 10}]   2/4m ( 50%)"
  }

  it should "format run id consistently" in {
    val runId = McRunnerConsole.Event.RunId("fixed")

    McRunnerConsole.render(runId) shouldBe "  run-id: fixed"
  }

  it should "format seed completion consistently" in {
    val seedDone = McRunnerConsole.Event.SeedDone(
      seed = 7L,
      totalSeeds = 12,
      elapsedMillis = 3456L,
      adoption = MetricValue.fromDecimalDigits(123, 3),
      inflation = MetricValue.fromDecimalDigits(45, 3),
      unemployment = MetricValue.fromDecimalDigits(67, 3),
    )

    McRunnerConsole.render(seedDone) shouldBe
      s"\r  Seed   7/12 [${fullBlock * 20}] done (3456ms) | Adopt=12.3% | pi=4.5% | Unemp=6.7%"
  }

  it should "format saved file reporting consistently" in {
    val savedFile = McRunnerConsole.Event.SavedFile("/tmp/output.csv")

    McRunnerConsole.render(savedFile) shouldBe "Saved: /tmp/output.csv"
  }

  it should "format a blank separator consistently" in {
    McRunnerConsole.render(McRunnerConsole.Event.BlankLine) shouldBe ""
  }

  it should "format total time consistently" in {
    val totalTime = McRunnerConsole.Event.TotalTime(4600L)

    McRunnerConsole.render(totalTime) shouldBe "\nTotal time: 4.6 seconds"
  }
