package com.boombustgroup.amorfati.montecarlo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class McRunConfigSpec extends AnyFlatSpec with Matchers:

  "McRunConfig" should "default runtime duration to 120 months" in {
    McRunConfig(nSeeds = 2, outputPrefix = "baseline").runDurationMonths shouldBe 120
  }
