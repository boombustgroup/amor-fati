package com.boombustgroup.amorfati.research

import com.boombustgroup.amorfati.config.{BaselineCatalog, BaselineRef, PreparedScenario, ScenarioRef, ScenarioRegistry}

/** Pre-release, typed boundary for researcher experiment construction.
  *
  * This facade deliberately prepares an experiment without exposing SimParams,
  * agent collections, or the future data-oriented storage layout. Execution
  * adapters may consume the prepared value while the public vocabulary remains
  * stable during the controlled core replacement.
  */
object ResearchApi:
  val Version = "research-api-v0"

  final case class ExperimentSpec(
      baseline: BaselineRef,
      scenarios: Vector[ScenarioRef],
      seedStart: Long = 1L,
      seeds: Int = 1,
      months: Int = 1,
      runId: String = "research-run",
  ):
    require(seedStart >= 0L, "seedStart must be non-negative")
    require(seeds > 0, "seeds must be positive")
    require(months > 0, "months must be positive")
    require(runId.matches("[A-Za-z0-9][A-Za-z0-9._-]*"), s"invalid runId: $runId")

  final case class PreparedExperiment(
      apiVersion: String,
      baseline: com.boombustgroup.amorfati.config.BaselineManifest,
      scenarios: Vector[PreparedScenario],
      seedStart: Long,
      seeds: Int,
      months: Int,
      runId: String,
  )

  def prepare(spec: ExperimentSpec, catalog: BaselineCatalog): Either[String, PreparedExperiment] =
    for
      baselineBundle <- catalog.resolve(spec.baseline).left.map(_.toString)
      scenarioSpecs  <- spec.scenarios.foldLeft[Either[String, Vector[com.boombustgroup.amorfati.config.ScenarioRegistry.ScenarioSpec]]](Right(Vector.empty)):
        (acc, ref) => acc.flatMap(selected => ScenarioRegistry.get(ref.id.value).map(selected :+ _))
      scenarios      <- ScenarioRegistry.prepare(baselineBundle, scenarioSpecs).left.map(_.toString)
    yield PreparedExperiment(Version, baselineBundle.manifest, scenarios, spec.seedStart, spec.seeds, spec.months, spec.runId)
