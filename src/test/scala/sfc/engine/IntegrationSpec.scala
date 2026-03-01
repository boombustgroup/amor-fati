package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.{Config, RunConfig}
import sfc.runSingle

class IntegrationSpec extends AnyFlatSpec with Matchers:

  // Use small firm count via env override if set, otherwise default
  private val rc = RunConfig(2000.0, 1, "test")

  "runSingle" should "complete without exception" in {
    noException should be thrownBy runSingle(42, rc)
  }

  it should "produce 120 rows x 126 columns" in {
    val result = runSingle(42, rc)
    result.timeSeries.length shouldBe Config.Duration
    for row <- result.timeSeries do
      row.length shouldBe 126
  }

  it should "have Month column = 1..120" in {
    val result = runSingle(42, rc)
    for t <- 0 until Config.Duration do
      result.timeSeries(t)(0) shouldBe (t + 1).toDouble
  }

  it should "keep adoption ratio in [0, 1]" in {
    val result = runSingle(42, rc)
    for t <- 0 until Config.Duration do
      result.timeSeries(t)(3) should be >= 0.0
      result.timeSeries(t)(3) should be <= 1.0
  }

  it should "keep unemployment in [0, 1]" in {
    val result = runSingle(42, rc)
    for t <- 0 until Config.Duration do
      result.timeSeries(t)(2) should be >= 0.0
      result.timeSeries(t)(2) should be <= 1.0
  }

  it should "be reproducible with the same seed" in {
    val r1 = runSingle(42, rc)
    val r2 = runSingle(42, rc)
    for t <- 0 until Config.Duration; c <- 0 until 126 do
      r1.timeSeries(t)(c) shouldBe r2.timeSeries(t)(c)
  }

  it should "have positive sigma values in columns 19-24" in {
    val result = runSingle(42, rc)
    for t <- 0 until Config.Duration; c <- 19 until 25 do
      result.timeSeries(t)(c) should be > 0.0
  }

  it should "have positive mean degree in column 25" in {
    val result = runSingle(42, rc)
    for t <- 0 until Config.Duration do
      result.timeSeries(t)(25) should be > 0.0
  }

  it should "return None for terminalHhAgg in aggregate mode" in {
    val result = runSingle(42, rc)
    // Default HH_MODE=aggregate → no household aggregates
    result.terminalHhAgg shouldBe None
  }
