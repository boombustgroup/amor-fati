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
    prepared.baseline.contentDigest shouldBe BaselineCatalog.legacy.list.head.contentDigest
    prepared.seedStart shouldBe spec.seedStart
    prepared.seeds shouldBe spec.seeds
    prepared.months shouldBe spec.months
    prepared.runId shouldBe spec.runId
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

  it should "render a deterministic result-bundle manifest" in {
    val digest   = BaselineCatalog.legacy.list.head.contentDigest
    val manifest = ResultBundleManifest(
      runId = "pilot-1",
      apiVersion = ResearchApi.Version,
      baselineId = BaselineCatalog.LegacyDefaultsId.value,
      baselineDigest = digest,
      scenarioIds = Vector("baseline", "monetary-tightening"),
      seedStart = 2L,
      seedCount = 3,
      months = 12,
      validationStatus = "passed",
      reconciliationStatus = "passed",
      evidencePolicy = "controlled-terminal",
    )

    manifest.toTsv.linesIterator.toVector shouldBe Vector(
      ResultBundleManifest.Header.mkString("\t"),
      s"pilot-1\t${ResearchApi.Version}\tPL-2026-04-30-legacy-v1\t$digest\tbaseline,monetary-tightening\t2\t3\t12\tpassed\tpassed\tcontrolled-terminal\tresult-bundle-v0",
    )
  }
