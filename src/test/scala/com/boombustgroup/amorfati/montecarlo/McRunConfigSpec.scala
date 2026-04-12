package com.boombustgroup.amorfati.montecarlo

import java.time.LocalDateTime

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class McRunConfigSpec extends AnyFlatSpec with Matchers:

  "McRunConfig" should "default runtime duration to 120 months" in {
    McRunConfig(nSeeds = 2, outputPrefix = "baseline").runDurationMonths shouldBe 120
  }

  it should "preserve an injected runId" in {
    McRunConfig(nSeeds = 2, outputPrefix = "baseline", runId = "manual-run").runId shouldBe "manual-run"
  }

  it should "format generated runIds deterministically for a supplied timestamp" in {
    McRunConfig.autoRunId(LocalDateTime.of(2026, 4, 12, 9, 7)) shouldBe "20260412T0907"
  }

  it should "reject non-positive seed counts" in {
    an[IllegalArgumentException] should be thrownBy McRunConfig(nSeeds = 0, outputPrefix = "baseline")
    an[IllegalArgumentException] should be thrownBy McRunConfig(nSeeds = -1, outputPrefix = "baseline")
  }

  it should "reject non-positive run durations" in {
    an[IllegalArgumentException] should be thrownBy McRunConfig(nSeeds = 2, outputPrefix = "baseline", runDurationMonths = 0)
    an[IllegalArgumentException] should be thrownBy McRunConfig(nSeeds = 2, outputPrefix = "baseline", runDurationMonths = -1)
  }

  it should "reject blank output prefixes and runIds" in {
    an[IllegalArgumentException] should be thrownBy McRunConfig(nSeeds = 2, outputPrefix = "   ")
    an[IllegalArgumentException] should be thrownBy McRunConfig(nSeeds = 2, outputPrefix = "baseline", runId = "   ")
  }
