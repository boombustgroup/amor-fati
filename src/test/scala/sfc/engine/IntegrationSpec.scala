package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.McRunner.runSingle
import sfc.SimOutput
import sfc.SimOutput.Col
import sfc.config.{RunConfig, SimParams}

class IntegrationSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  // Use small firm count via env override if set, otherwise default
  private val rc = RunConfig(1, "test")

  "runSingle" should "complete without exception" in {
    noException should be thrownBy runSingle(42, rc)
  }

  it should "produce 120 rows x 197 columns" in {
    val result = runSingle(42, rc)
    result.timeSeries.length shouldBe p.timeline.duration
    for row <- result.timeSeries do row.length shouldBe SimOutput.nCols
  }

  it should "have Month column = 1..120" in {
    val result = runSingle(42, rc)
    for t <- 0 until p.timeline.duration do result.timeSeries(t)(Col.Month.ordinal) shouldBe (t + 1).toDouble
  }

  it should "keep adoption ratio in [0, 1]" in {
    val result = runSingle(42, rc)
    for t <- 0 until p.timeline.duration do
      result.timeSeries(t)(Col.TotalAdoption.ordinal) should be >= 0.0
      result.timeSeries(t)(Col.TotalAdoption.ordinal) should be <= 1.0
  }

  it should "keep unemployment in [0, 1]" in {
    val result = runSingle(42, rc)
    for t <- 0 until p.timeline.duration do
      result.timeSeries(t)(Col.Unemployment.ordinal) should be >= 0.0
      result.timeSeries(t)(Col.Unemployment.ordinal) should be <= 1.0
  }

  it should "be reproducible with the same seed" in {
    val r1 = runSingle(42, rc)
    val r2 = runSingle(42, rc)
    for t <- 0 until p.timeline.duration; c <- 0 until SimOutput.nCols do r1.timeSeries(t)(c) shouldBe r2.timeSeries(t)(c)
  }

  it should "have positive sigma values in columns 19-24" in {
    val result = runSingle(42, rc)
    for t <- 0 until p.timeline.duration; s <- 0 until 6 do result.timeSeries(t)(Col.sectorSigma(s).ordinal) should be > 0.0
  }

  it should "have positive mean degree in column 25" in {
    val result = runSingle(42, rc)
    for t <- 0 until p.timeline.duration do result.timeSeries(t)(Col.MeanDegree.ordinal) should be > 0.0
  }

  it should "return defined terminalState with hhAgg" in {
    val result = runSingle(42, rc)
    result.terminalState.world.hhAgg.get.employed should be >= 0
  }
