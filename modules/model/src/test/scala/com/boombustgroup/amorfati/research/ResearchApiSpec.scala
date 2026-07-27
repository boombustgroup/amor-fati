package com.boombustgroup.amorfati.research

import com.boombustgroup.amorfati.config.{BaselineCatalog, BaselineId, BaselineRef, ScenarioId, ScenarioRef}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ResearchApiSpec extends AnyFlatSpec with Matchers:
  "ResearchApi" should "prepare a typed experiment through the catalog" in {
    val spec = ResearchApi.ExperimentSpec(
      baseline = BaselineRef(BaselineCatalog.LegacyDefaultsId),
      scenarios = Vector(ScenarioRef(ScenarioId.from("monetary-tightening").toOption.get)),
      months = 12,
      runId = "pilot-1",
    )

    val prepared = ResearchApi.prepare(spec, BaselineCatalog.legacy).fold(error => fail(error), identity)
    prepared.apiVersion shouldBe ResearchApi.Version
    prepared.baseline.id shouldBe BaselineCatalog.LegacyDefaultsId
    prepared.scenarios.map(_.id) should contain("monetary-tightening")
  }

  it should "reject an unknown scenario before execution" in {
    val spec = ResearchApi.ExperimentSpec(
      baseline = BaselineRef(BaselineCatalog.LegacyDefaultsId),
      scenarios = Vector(ScenarioRef(ScenarioId.from("does-not-exist").toOption.get)),
    )

    ResearchApi.prepare(spec, BaselineCatalog.legacy) match
      case Left(error)  => error should include("Unknown scenario 'does-not-exist'")
      case Right(value) => fail(s"expected unknown scenario failure, got $value")
  }
