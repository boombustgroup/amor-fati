package com.boombustgroup.amorfati.diagnostics

import com.boombustgroup.amorfati.config.{BaselineCatalog, ScenarioRegistry}
import com.boombustgroup.amorfati.research.{ResearchApi, ResultBundleManifest}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/** Execution adapter that keeps the Research API boundary above the legacy
  * scenario runner while emitting the versioned result-bundle manifest.
  */
object ResearchApiScenarioRunner:
  final case class Result(paths: Vector[Path], manifest: ResultBundleManifest)

  def run(spec: ResearchApi.ExperimentSpec, out: Path, catalog: BaselineCatalog = BaselineCatalog.legacy): Either[String, Result] =
    for
      prepared    <- ResearchApi.prepare(spec, catalog)
      scenarios   <- prepared.scenarios
        .flatMap(_.scenarioId)
        .foldLeft[Either[String, Vector[ScenarioRegistry.ScenarioSpec]]](Right(Vector.empty)): (acc, id) =>
          acc.flatMap(selected => ScenarioRegistry.get(id).map(selected :+ _))
      exported    <- ScenarioRunExport.run(
        ScenarioRunExport.Config(
          baseline = spec.baseline,
          scenarios = scenarios,
          seedStart = spec.seedStart,
          seeds = spec.seeds,
          months = spec.months,
          runId = spec.runId,
          out = out,
        ),
      )
      manifest     = ResultBundleManifest(
        runId = spec.runId,
        apiVersion = ResearchApi.Version,
        baselineId = prepared.baseline.id.toString,
        baselineDigest = prepared.baseline.contentDigest,
        scenarioIds = prepared.scenarios.map(_.id),
        seedStart = spec.seedStart,
        seedCount = spec.seeds,
        months = spec.months,
        validationStatus = "completed",
        reconciliationStatus = "reported-by-engine",
        evidencePolicy = "controlled-terminal",
      )
      manifestPath = out.resolve(spec.runId).resolve("result-bundle-manifest.tsv")
      _            = Files.createDirectories(manifestPath.getParent)
      _            = Files.writeString(manifestPath, manifest.toTsv, UTF_8)
    yield Result(exported.paths :+ manifestPath, manifest)
