package com.boombustgroup.amorfati.config

object CalibrationRegisterRenderer:

  import CalibrationProvenance.*

  private val ParameterHeader: Vector[String] = Vector(
    "| Parameter | Value | Unit | Source / provenance | Empirical target | Transformation | Owner module | Status |",
    "| --- | --- | --- | --- | --- | --- | --- | --- |",
  )

  private val Sections: Vector[(String, CalibrationParameter => Boolean)] = Vector(
    "Core Scale And Sectors"                          -> { parameter =>
      hasId(parameter, "household.count") ||
      hasId(parameter, "gdpRatio") ||
      hasId(parameter, "topology") ||
      hasPrefix(parameter, "pop.", "sectorDefs")
    },
    "Household And Labor"                             -> { parameter =>
      hasPrefix(parameter, "household.", "labor.", "social.") &&
      !hasId(parameter, "household.count")
    },
    "Firm Production, Entry, Capital, And Climate"    -> { parameter =>
      hasPrefix(parameter, "firm.", "capital.", "climate.")
    },
    "Fiscal, Monetary, And Banking"                   -> { parameter =>
      hasPrefix(parameter, "fiscal.", "monetary.", "banking.") ||
      hasId(parameter, "io.crossSectorSpillover")
    },
    "External Sector And Financial Markets"           -> { parameter =>
      hasPrefix(parameter, "forex.", "openEcon.", "fdi.", "gvc.", "immigration.", "remittance.", "tourism.", "equity.", "corpBond.") ||
      hasId(parameter, "priceLevel.importPush")
    },
    "Housing, Prices, Regions, IO, Informal, And SOE" -> { parameter =>
      hasPrefix(parameter, "housing.", "regional.", "pricing.", "informal.", "soe.") ||
      (hasPrefix(parameter, "io.") && !hasId(parameter, "io.crossSectorSpillover"))
    },
    "Non-Bank Financials And Public Funds"            -> { parameter =>
      hasPrefix(parameter, "ins.", "nbfi.", "quasiFiscal.", "earmarked.")
    },
  )

  def render(parameters: Vector[CalibrationParameter] = CalibrationProvenance.Baseline.parameters): String =
    validateSectionCoverage(parameters)
    val lines = Vector.newBuilder[String]
    lines ++= header
    lines ++= statusTaxonomy
    lines ++= statusCounts(parameters)
    lines ++= placeholderDecisions(parameters)
    lines ++= structuredSourceMetadata(parameters)
    lines ++= tunedValidationEvidence(parameters)
    lines ++= transformationRules
    Sections.foreach: (title, predicate) =>
      val sectionParameters = parameters.filter(predicate)
      if sectionParameters.nonEmpty then
        lines += s"## $title"
        lines += ""
        lines ++= ParameterHeader
        sectionParameters.foreach(parameter => lines += renderParameter(parameter))
        lines += ""
    lines ++= searchableGaps
    lines.result().mkString("\n") + "\n"

  private def header: Vector[String] =
    Vector(
      "# Calibration Register",
      "",
      "This register records the current calibration surface of Amor Fati. It is a",
      "paper-facing inventory of key parameters, their implementation owner, value,",
      "unit, provenance, empirical target, transformation, and calibration status.",
      "",
      "Production defaults are calibrated directly to the Poland snapshot as of",
      "2026-04-30. There is no alternate baseline profile in the production parameter",
      "surface.",
      "",
      "The register is generated from `CalibrationProvenance.Baseline`; edit the",
      "typed provenance registry first, then regenerate this Markdown artifact with",
      "`sbt \"calibrationRegister --out docs/calibration-register.md\"`.",
      "",
      "The register is intentionally useful before every value has a final data",
      "source. Missing or weak provenance is marked explicitly with searchable tokens:",
      "`UNKNOWN_SOURCE`, `TUNED_NEEDS_VALIDATION`, `ASSUMED`, `PLACEHOLDER`,",
      "`EMPIRICAL_TRANSFORMED`, or `POLICY_SCENARIO`.",
      "",
    )

  private def statusTaxonomy: Vector[String] =
    Vector(
      "## Status Taxonomy",
      "",
      "| Status | Meaning |",
      "| --- | --- |",
    ) ++ CalibrationStatus.values.toVector.map(status => s"| `${status.token}` | ${statusMeaning(status)} |") :+
      ""

  private def statusCounts(parameters: Vector[CalibrationParameter]): Vector[String] =
    val counts = parameters.groupMapReduce(_.status)(_ => 1)(_ + _)
    Vector(
      "## Status Counts",
      "",
      "These counts are rendered from `CalibrationProvenance.Baseline` at generation time.",
      "",
      "| Status | Count |",
      "| --- | --- |",
    ) ++ CalibrationStatus.values.toVector.map(status => s"| `${status.token}` | ${counts.getOrElse(status, 0)} |") :+
      ""

  private def transformationRules: Vector[String] =
    Vector(
      "## Transformation Rules",
      "",
      "- Raw PLN stock values in config classes are scaled by `SimParams.gdpRatio` in",
      "  `SimParams.defaults` when they represent macro stocks or monthly macro flows.",
      "- Agent-level monetary values such as wages, rents, firm costs, and per-worker",
      "  values are not scaled by `gdpRatio`.",
      "- Rates are annual unless the consuming rule applies `.monthly`.",
      "- Vector parameters generally follow the six-sector order:",
      "  `BPO/SSC`, `Manufacturing`, `Retail/Services`, `Healthcare`, `Public`,",
      "  `Agriculture`.",
      "- Runtime numeric evidence comes from Monte Carlo output columns in",
      "  `McTimeseriesSchema`; this register documents parameter provenance, not run",
      "  results. Empirical validation targets and output mappings are documented in",
      "  `docs/empirical-validation-report.md`.",
      "- External source selection, unit/frequency conversion, sector/instrument",
      "  crosswalks, source vintages, license/reuse notes, and prioritized empirical",
      "  gaps are documented in",
      "  `docs/data-bridge-national-financial-accounts.md`.",
      "",
    )

  private def searchableGaps: Vector[String] =
    Vector(
      "## Searchable Gaps",
      "",
      "Use the following commands to audit open provenance gaps:",
      "",
      "```bash",
      "rg \"UNKNOWN_SOURCE|TUNED_NEEDS_VALIDATION|ASSUMED|PLACEHOLDER|EMPIRICAL_TRANSFORMED|POLICY_SCENARIO\" modules/model/src/main/scala/com/boombustgroup/amorfati/config/CalibrationProvenance.scala docs/calibration-register.md",
      "```",
      "",
      "Priority gaps before publication:",
      "",
      "- Replace `UNKNOWN_SOURCE` rows with source table, year, and transformation",
      "  notes.",
      "- Split `CODE_NOTE_EMPIRICAL` rows into source-specific evidence links once",
      "  the data bridge identifies the target source family and extraction rule.",
      "- Validate `TUNED_NEEDS_VALIDATION` coefficients with point-in-time validation,",
      "  stylized-fact matching, or sensitivity ranges.",
      "- Decide whether currently unscaled institutional monthly flows, such as PFRON",
      "  monthly revenue/spending, should remain agent-scale values or be moved into",
      "  the `gdpRatio`-scaled macro-stock convention.",
      "- Keep scenario deltas in `docs/scenario-registry.md`; this register remains",
      "  the baseline parameter-provenance surface.",
    )

  private def renderParameter(parameter: CalibrationParameter): String =
    Vector(
      renderParameterIds(parameter.parameterIds),
      valueCell(parameter.renderedValue),
      plainCell(parameter.unit),
      plainCell(parameter.provenance),
      plainCell(parameter.empiricalTarget),
      plainCell(parameter.transformation),
      parameter.ownerModules.map(codeCell).mkString(", "),
      codeCell(parameter.status.token),
    ).mkString("| ", " | ", " |")

  private def renderParameterIds(ids: Vector[String]): String =
    val prefix = ids.headOption.flatMap: id =>
      val dotIndex = id.lastIndexOf('.')
      if dotIndex < 0 then None else Some(id.take(dotIndex + 1))

    ids.zipWithIndex
      .map: (id, index) =>
        val rendered =
          if index == 0 then id
          else prefix.filter(id.startsWith).fold(id)(prefix => id.drop(prefix.length))
        codeCell(rendered)
      .mkString(", ")

  private def statusMeaning(status: CalibrationStatus): String =
    status match
      case CalibrationStatus.Empirical            =>
        "Direct statutory, institutional, or data-note target is documented in code comments."
      case CalibrationStatus.EmpiricalTransformed =>
        "Institutional/data target transformed to match the model perimeter or units."
      case CalibrationStatus.CodeNoteEmpirical    =>
        "Code comments name source institution/year, but no source table or URL is attached yet."
      case CalibrationStatus.Assumed              =>
        "Structural modeling assumption or stylized calibration."
      case CalibrationStatus.TunedNeedsValidation =>
        "Behavioral/dynamic coefficient chosen for model behavior; needs typed validation evidence."
      case CalibrationStatus.PolicyScenario       =>
        "Scenario/shock switch or policy parameter, not a baseline empirical estimate."
      case CalibrationStatus.Placeholder          =>
        "Explicit placeholder or simplified value with a typed decision and follow-up path."
      case CalibrationStatus.UnknownSource        =>
        "Parameter is active in the model but final provenance is not yet documented."

  private def placeholderDecisions(parameters: Vector[CalibrationParameter]): Vector[String] =
    val placeholders = parameters.filter(_.status == CalibrationStatus.Placeholder)
    val missing      = placeholders.filter(_.placeholderDecision.isEmpty)
    require(
      missing.isEmpty,
      s"Missing placeholder decision for calibration parameter(s): ${missing.map(_.id).mkString(", ")}",
    )
    val decisions    = placeholders.flatMap(_.placeholderDecision)
    if decisions.isEmpty then Vector.empty
    else
      Vector(
        "## Placeholder Decisions",
        "",
        "Remaining placeholders are explicit startup decisions carried in typed provenance metadata.",
        "",
        "| Parameter | Decision | Reason | Validation impact | Follow-up path |",
        "| --- | --- | --- | --- | --- |",
      ) ++ decisions.map(renderPlaceholderDecision) :+
        ""

  private def renderPlaceholderDecision(decision: PlaceholderDecision): String =
    Vector(
      codeCell(decision.parameterId),
      codeCell(decision.decision.toString),
      plainCell(decision.reason),
      plainCell(decision.validationImpact),
      plainCell(decision.followUpPath),
    ).mkString("| ", " | ", " |")

  private def structuredSourceMetadata(parameters: Vector[CalibrationParameter]): Vector[String] =
    val sourced = parameters.filter(_.sourceMetadata.nonEmpty)
    if sourced.isEmpty then Vector.empty
    else
      Vector(
        "## Structured Source Metadata",
        "",
        "Rows below have been migrated from informal code-note provenance to typed source metadata.",
        "",
        "| Parameter | Source family | Source table/code | Vintage/date | URL or manifest | Transformation notes |",
        "| --- | --- | --- | --- | --- | --- |",
      ) ++ sourced.map(renderSourceMetadata) :+
        ""

  private def renderSourceMetadata(parameter: CalibrationParameter): String =
    val source = parameter.sourceMetadata.getOrElse:
      throw new IllegalArgumentException(s"Missing source metadata for calibration parameter: ${parameter.id}")
    Vector(
      renderParameterIds(parameter.parameterIds),
      plainCell(source.sourceFamily),
      codeCell(source.sourceTableOrCode),
      plainCell(source.vintage),
      plainCell(source.sourceReference),
      plainCell(source.transformationNotes),
    ).mkString("| ", " | ", " |")

  private def tunedValidationEvidence(parameters: Vector[CalibrationParameter]): Vector[String] =
    val tuned = parameters.filter(_.status == CalibrationStatus.TunedNeedsValidation)
    if tuned.isEmpty then Vector.empty
    else
      val missing = tuned.filter(_.validationEvidence.isEmpty)
      require(
        missing.isEmpty,
        s"Missing tuned validation evidence mode for calibration parameter(s): ${missing.map(_.id).mkString(", ")}",
      )

      val evidenceByMode = CalibrationValidationMode.values.toVector.map: mode =>
        val rows    = tuned.filter(_.validationEvidence.exists(_.mode == mode))
        val linked  = rows.count(_.hasValidationEvidencePath)
        val missing = rows.length - linked
        s"| `${mode.token}` | ${rows.length} | $linked | $missing |"

      val evidenceRows = tuned.map(renderTunedValidationEvidence)

      Vector(
        "## Tuned Validation Evidence",
        "",
        "`TUNED_NEEDS_VALIDATION` rows carry a typed expected validation mode.",
        "`MISSING_VALIDATION_EVIDENCE` means the row is classified but still lacks",
        "a concrete diagnostic artifact path.",
        "",
        "### Mode Counts",
        "",
        "| Validation mode | Count | Linked evidence paths | Missing evidence paths |",
        "| --- | ---: | ---: | ---: |",
      ) ++ evidenceByMode ++ Vector(
        "",
        "### Evidence Paths",
        "",
        "| Parameter | Validation mode | Evidence path | Artifact label | Scenario ids | Evidence target | Notes |",
        "| --- | --- | --- | --- | --- | --- | --- |",
      ) ++ evidenceRows :+
        ""

  private def renderTunedValidationEvidence(parameter: CalibrationParameter): String =
    val evidence = parameter.validationEvidence.getOrElse:
      throw new IllegalArgumentException(s"Missing tuned validation evidence mode for calibration parameter: ${parameter.id}")
    Vector(
      renderParameterIds(parameter.parameterIds),
      codeCell(evidence.mode.token),
      evidence.evidencePath.map(plainCell).getOrElse(codeCell("MISSING_VALIDATION_EVIDENCE")),
      evidence.artifactLabel.map(plainCell).getOrElse(""),
      if evidence.scenarioIds.isEmpty then "" else plainCell(evidence.scenarioIds.mkString(", ")),
      plainCell(evidence.evidenceTarget),
      plainCell(evidence.notes),
    ).mkString("| ", " | ", " |")

  private def hasId(parameter: CalibrationParameter, id: String): Boolean =
    parameter.parameterIds.contains(id)

  private def hasPrefix(parameter: CalibrationParameter, prefixes: String*): Boolean =
    parameter.parameterIds.exists(parameterId => prefixes.exists(parameterId.startsWith))

  private def validateSectionCoverage(parameters: Vector[CalibrationParameter]): Unit =
    val unmatched = parameters.filter: parameter =>
      !Sections.exists { case (_, predicate) => predicate(parameter) }
    val repeated  = parameters.filter: parameter =>
      Sections.count { case (_, predicate) => predicate(parameter) } > 1
    require(unmatched.isEmpty, s"Unsectioned calibration parameters: ${unmatched.map(_.id).mkString(", ")}")
    require(repeated.isEmpty, s"Calibration parameters matched multiple sections: ${repeated.map(_.id).mkString(", ")}")

  private def plainCell(value: String): String =
    escapeCell(value)

  private def valueCell(value: String): String =
    if value.startsWith("$") && value.endsWith("$") then plainCell(value)
    else codeCell(value)

  private def codeCell(value: String): String =
    s"`${escapeCell(value)}`"

  private def escapeCell(value: String): String =
    value.replace("\n", " ").replace("|", "\\|")

end CalibrationRegisterRenderer
