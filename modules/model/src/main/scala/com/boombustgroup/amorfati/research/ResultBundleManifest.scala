package com.boombustgroup.amorfati.research

import com.boombustgroup.amorfati.config.BaselineDigest

/** Immutable provenance contract for one controlled Research API result bundle.
  */
final case class ResultBundleManifest(
    runId: String,
    apiVersion: String,
    baselineId: String,
    baselineDigest: BaselineDigest,
    scenarioIds: Vector[String],
    seedStart: Long,
    seedCount: Int,
    months: Int,
    validationStatus: String,
    reconciliationStatus: String,
    evidencePolicy: String,
    resultSchemaVersion: String = "result-bundle-v0",
):
  require(runId.matches("[A-Za-z0-9][A-Za-z0-9._-]*"), s"invalid runId: $runId")
  require(seedStart >= 0L, "seedStart must be non-negative")
  require(seedCount > 0, "seedCount must be positive")
  require(months > 0, "months must be positive")
  require(validationStatus.nonEmpty, "validationStatus must be non-blank")
  require(reconciliationStatus.nonEmpty, "reconciliationStatus must be non-blank")
  require(evidencePolicy.nonEmpty, "evidencePolicy must be non-blank")

  /** Stable one-row TSV representation for a result-bundle manifest. */
  def toTsv: String =
    val fields = Vector(
      runId,
      apiVersion,
      baselineId,
      baselineDigest.toString,
      scenarioIds.mkString(","),
      seedStart.toString,
      seedCount.toString,
      months.toString,
      validationStatus,
      reconciliationStatus,
      evidencePolicy,
      resultSchemaVersion,
    )
    ResultBundleManifest.Header.mkString("\t") + "\n" + fields.mkString("\t") + "\n"

object ResultBundleManifest:
  val Header: Vector[String] = Vector(
    "run_id",
    "api_version",
    "baseline_id",
    "baseline_digest",
    "scenario_ids",
    "seed_start",
    "seed_count",
    "months",
    "validation_status",
    "reconciliation_status",
    "evidence_policy",
    "result_schema_version",
  )
