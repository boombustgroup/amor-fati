package com.boombustgroup.amorfati.accounting.matrix

import com.boombustgroup.amorfati.accounting.matrix.SfcMatrixEvidence.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.{AssetType, MechanismId}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

object SfcMatrixRenderers:

  enum OutputFormat:
    case Latex
    case Markdown

    def extension: String =
      this match
        case Latex    => "tex"
        case Markdown => "md"

  object OutputFormat:
    val Default: Vector[OutputFormat] = Vector(OutputFormat.Latex, OutputFormat.Markdown)

    def parseList(value: String): Either[String, Vector[OutputFormat]] =
      val parsed = value
        .split(",")
        .toVector
        .map(_.trim.toLowerCase)
        .filter(_.nonEmpty)
        .map:
          case "latex" | "tex"   => Right(OutputFormat.Latex)
          case "markdown" | "md" => Right(OutputFormat.Markdown)
          case other             => Left(s"Unknown matrix output format: $other")

      parsed.collectFirst { case Left(err) => err } match
        case Some(err) => Left(err)
        case None      =>
          val formats = parsed.collect { case Right(format) => format }.distinct
          if formats.nonEmpty then Right(formats) else Left("At least one matrix output format is required")

  final case class RenderedArtifact(relativePath: String, contents: String)

  def renderSymbolicBundle(
      bundle: MatrixEvidenceBundle,
      formats: Vector[OutputFormat],
  )(using SimParams): Vector[RenderedArtifact] =
    val matrixArtifacts = SfcSymbolicMatrices.matrices.flatMap: matrix =>
      formats.map: format =>
        RenderedArtifact(s"${matrix.name}.${format.extension}", renderSymbolicMatrix(bundle.metadata, matrix, format))

    val mappingArtifacts = formats.map: format =>
      RenderedArtifact(s"matrix-mapping.${format.extension}", renderMapping(bundle.metadata, format))

    val mechanismArtifacts = formats.map: format =>
      RenderedArtifact(s"flow-mechanism-semantics.${format.extension}", renderFlowMechanismSemantics(bundle.metadata, format))

    val reconciliationArtifacts = formats.map: format =>
      RenderedArtifact(s"stock-flow-reconciliation.${format.extension}", renderReconciliation(bundle, format))

    matrixArtifacts ++ mappingArtifacts ++ mechanismArtifacts ++ reconciliationArtifacts

  def writeSymbolicBundle(
      bundle: MatrixEvidenceBundle,
      outDir: Path,
      formats: Vector[OutputFormat],
  )(using SimParams): Vector[Path] =
    Files.createDirectories(outDir)
    renderSymbolicBundle(bundle, formats).map: artifact =>
      val path = outDir.resolve(artifact.relativePath)
      Files.createDirectories(path.getParent)
      Files.writeString(path, artifact.contents, StandardCharsets.UTF_8)
      path

  private def renderSymbolicMatrix(
      metadata: MatrixMetadata,
      matrix: SfcSymbolicMatrices.SymbolicMatrix,
      format: OutputFormat,
  ): String =
    format match
      case OutputFormat.Latex    => renderSymbolicLatex(metadata, matrix)
      case OutputFormat.Markdown => renderSymbolicMarkdown(metadata, matrix)

  private def renderSymbolicLatex(metadata: MatrixMetadata, matrix: SfcSymbolicMatrices.SymbolicMatrix): String =
    val columns = "p{0.24\\linewidth}" + ("c" * SfcSymbolicMatrices.sectors.length) + "c"
    val header  =
      (matrix.rowHeader +: SfcSymbolicMatrices.sectors.map(sector => SfcMatrixRegistry.sector(sector).shortLabel) :+ "Sum")
        .map(escapeLatex)
        .mkString(" & ")
    val body    = matrix.rows.map: row =>
      val cells = SfcSymbolicMatrices.sectors.map(sector => latexSymbol(row.cells.getOrElse(sector, "")))
      (escapeLatex(row.label) +: cells :+ latexSymbol(row.zeroSymbol)).mkString(" & ") + " \\\\"

    s"""% schema=${metadata.schemaVersion} seed=${metadata.seed} month=${metadata.executionMonth} commit=${escapeLatex(
        metadata.commit,
      )} sfc=${metadata.sfcStatus} matrix=${metadata.matrixStatus} output=symbolic
       |\\begingroup
       |\\scriptsize
       |\\setlength{\\tabcolsep}{2pt}
       |\\renewcommand{\\arraystretch}{1.15}
       |\\begin{tsv}{$columns}
       |$header \\\\
       |\\hline
       |${body.mkString("\n")}
       |\\end{tsv}
       |\\endgroup
       |""".stripMargin

  private def renderSymbolicMarkdown(metadata: MatrixMetadata, matrix: SfcSymbolicMatrices.SymbolicMatrix): String =
    val header = matrix.rowHeader +: SfcSymbolicMatrices.sectors.map(sector => SfcMatrixRegistry.sector(sector).label) :+ "Sum"
    val rows   = matrix.rows.map: row =>
      row.label +: SfcSymbolicMatrices.sectors.map(sector => markdownSymbol(row.cells.getOrElse(sector, ""))) :+ markdownSymbol(row.zeroSymbol)

    renderMarkdownTable(
      s"""<!-- schema=${metadata.schemaVersion} seed=${metadata.seed} month=${metadata.executionMonth} commit=${metadata.commit} sfc=${metadata.sfcStatus} matrix=${metadata.matrixStatus} output=symbolic -->
         |
         |${markdownSourceContract}
         |
         |# ${matrix.title}
         |""".stripMargin,
      header,
      rows,
    )

  private def renderMapping(metadata: MatrixMetadata, format: OutputFormat): String =
    format match
      case OutputFormat.Latex    => renderMappingLatex(metadata)
      case OutputFormat.Markdown => renderMappingMarkdown(metadata)

  private def renderMappingLatex(metadata: MatrixMetadata): String =
    val header = Vector("Matrix", "Row", "Symbols", "Runtime assets", "Runtime mechanisms", "Note").map(escapeLatex).mkString(" & ")
    val body   = SfcSymbolicMatrices.mappingRows.map: row =>
      Vector(
        escapeLatex(row.matrix),
        escapeLatex(row.rowLabel),
        row.symbols.map(latexSymbol).mkString(", "),
        latexTextList(row.assets.map(assetLabel)),
        latexTextList(row.mechanisms.map(mechanismLabel)),
        escapeLatex(row.note),
      ).mkString(" & ") + " \\\\"

    s"""% schema=${metadata.schemaVersion} seed=${metadata.seed} month=${metadata.executionMonth} commit=${escapeLatex(
        metadata.commit,
      )} sfc=${metadata.sfcStatus} matrix=${metadata.matrixStatus} output=symbolic-mapping
       |% requires \\usepackage{longtable}
       |\\begingroup
       |\\scriptsize
       |\\setlength{\\tabcolsep}{2pt}
       |\\renewcommand{\\arraystretch}{1.15}
       |\\begin{longtable}{p{0.08\\linewidth}p{0.13\\linewidth}p{0.14\\linewidth}p{0.17\\linewidth}p{0.24\\linewidth}p{0.14\\linewidth}}
       |$header \\\\
       |\\hline
       |\\endfirsthead
       |$header \\\\
       |\\hline
       |\\endhead
       |${body.mkString("\n")}
       |\\end{longtable}
       |\\endgroup
       |""".stripMargin

  private def renderMappingMarkdown(metadata: MatrixMetadata): String =
    val rows = SfcSymbolicMatrices.mappingRows.map: row =>
      Vector(
        row.matrix,
        row.rowLabel,
        row.symbols.map(markdownSymbol).mkString(", "),
        row.assets.map(assetLabel).mkString("<br>"),
        row.mechanisms.map(mechanismLabel).mkString("<br>"),
        row.note,
      )

    renderMarkdownTable(
      s"""<!-- schema=${metadata.schemaVersion} seed=${metadata.seed} month=${metadata.executionMonth} commit=${metadata.commit} sfc=${metadata.sfcStatus} matrix=${metadata.matrixStatus} output=symbolic-mapping -->
         |
         |${markdownSourceContract}
         |
         |# Symbolic Matrix Mapping
         |""".stripMargin,
      Vector("Matrix", "Row", "Symbols", "Runtime assets", "Runtime mechanisms", "Note"),
      rows,
    )

  private def renderFlowMechanismSemantics(metadata: MatrixMetadata, format: OutputFormat): String =
    format match
      case OutputFormat.Latex    => renderFlowMechanismSemanticsLatex(metadata)
      case OutputFormat.Markdown => renderFlowMechanismSemanticsMarkdown(metadata)

  private def renderFlowMechanismSemanticsLatex(metadata: MatrixMetadata): String =
    val header = Vector(
      "ID",
      "Mechanism",
      "Family",
      "Topology",
      "Asset class",
      "Matrix rows",
      "Survivability",
      "SFC / reconciliation impact",
      "Coverage",
    ).map(escapeLatex).mkString(" & ")
    val body   = FlowMechanismSemantics.rows.map: row =>
      Vector(
        escapeLatex(row.mechanism.toInt.toString),
        escapeLatex(row.label),
        escapeLatex(row.flowFamily),
        escapeLatex(row.expectedTopology),
        escapeLatex(row.assetClass),
        latexTextList(symbolicRows(row)),
        escapeLatex(row.survivability.toString),
        escapeLatex(row.sfcImpact),
        escapeLatex(row.coverage),
      ).mkString(" & ") + " \\\\"

    s"""% schema=${metadata.schemaVersion} seed=${metadata.seed} month=${metadata.executionMonth} commit=${escapeLatex(
        metadata.commit,
      )} sfc=${metadata.sfcStatus} matrix=${metadata.matrixStatus} output=flow-mechanism-semantics
       |% requires \\usepackage{longtable}
       |\\begingroup
       |\\scriptsize
       |\\setlength{\\tabcolsep}{2pt}
       |\\renewcommand{\\arraystretch}{1.12}
       |\\begin{longtable}{p{0.03\\linewidth}p{0.12\\linewidth}p{0.10\\linewidth}p{0.15\\linewidth}p{0.12\\linewidth}p{0.11\\linewidth}p{0.08\\linewidth}p{0.17\\linewidth}p{0.13\\linewidth}}
       |$header \\\\
       |\\hline
       |\\endfirsthead
       |$header \\\\
       |\\hline
       |\\endhead
       |${body.mkString("\n")}
       |\\end{longtable}
       |\\endgroup
       |""".stripMargin

  private def renderFlowMechanismSemanticsMarkdown(metadata: MatrixMetadata): String =
    val rows = FlowMechanismSemantics.rows.map: row =>
      Vector(
        row.mechanism.toInt.toString,
        row.label,
        row.flowFamily,
        row.expectedTopology,
        row.assetClass,
        symbolicRows(row).mkString("<br>"),
        row.survivability.toString,
        row.sfcImpact,
        row.coverage,
      )

    renderMarkdownTable(
      s"""<!-- schema=${metadata.schemaVersion} seed=${metadata.seed} month=${metadata.executionMonth} commit=${metadata.commit} sfc=${metadata.sfcStatus} matrix=${metadata.matrixStatus} output=flow-mechanism-semantics -->
         |
         |${markdownSourceContract}
         |
         |# Flow Mechanism Semantics
         |
         |Every one of the ${FlowMechanismSemantics.rows.size} runtime-emitted `FlowMechanism` entries appears exactly once in this table. The map composes `FlowMechanism`, `SfcMatrixRegistry`, `SfcSymbolicMatrices`, `RuntimeMechanismSurvivability`, and existing test/diagnostic ownership into one reviewer-facing audit surface.
         |""".stripMargin,
      Vector(
        "ID",
        "Mechanism",
        "Family",
        "Topology",
        "Asset class",
        "Matrix rows",
        "Survivability",
        "SFC / reconciliation impact",
        "Coverage",
      ),
      rows,
    )

  private def renderReconciliation(bundle: MatrixEvidenceBundle, format: OutputFormat)(using SimParams): String =
    format match
      case OutputFormat.Latex    => renderReconciliationLatex(bundle)
      case OutputFormat.Markdown => renderReconciliationMarkdown(bundle)

  private def renderReconciliationLatex(bundle: MatrixEvidenceBundle)(using SimParams): String =
    val metadata = bundle.metadata
    val header   =
      Vector("Identity", "Expected (macro PLN)", "Actual (macro PLN)", "Residual (macro PLN)", "Status", "Runtime channels", "Source")
        .map(escapeLatex)
        .mkString(" & ")
    val body     = bundle.reconciliation.rows.map: row =>
      Vector(
        escapeLatex(row.label),
        escapeLatex(formatAmountMacroPln(row.expectedRaw)),
        escapeLatex(formatAmountMacroPln(row.actualRaw)),
        escapeLatex(formatAmountMacroPln(row.residualRaw)),
        escapeLatex(row.status),
        latexTextList(reconciliationChannels(row)),
        escapeLatex(s"${row.source} ${row.note}"),
      ).mkString(" & ") + " \\\\"

    s"""% schema=${metadata.schemaVersion} seed=${metadata.seed} month=${metadata.executionMonth} commit=${escapeLatex(
        metadata.commit,
      )} sfc=${metadata.sfcStatus} matrix=${metadata.matrixStatus} output=stock-flow-reconciliation ${reconciliationScaleMetadata}
       |% requires \\usepackage{longtable}
       |\\begingroup
       |\\scriptsize
       |\\setlength{\\tabcolsep}{2pt}
       |\\renewcommand{\\arraystretch}{1.15}
       |\\noindent\\textit{Displayed monetary columns are macro-scaled PLN (raw model-scale PLN divided by SimParams.gdpRatio); identity validation remains on raw model-scale fixed-point PLN.}\\\\[0.5em]
       |\\begin{longtable}{p{0.13\\linewidth}p{0.10\\linewidth}p{0.10\\linewidth}p{0.10\\linewidth}p{0.06\\linewidth}p{0.24\\linewidth}p{0.20\\linewidth}}
       |$header \\\\
       |\\hline
       |\\endfirsthead
       |$header \\\\
       |\\hline
       |\\endhead
       |${body.mkString("\n")}
       |\\end{longtable}
       |\\endgroup
       |""".stripMargin

  private def renderReconciliationMarkdown(bundle: MatrixEvidenceBundle)(using SimParams): String =
    val metadata     = bundle.metadata
    val scaleFormula = "$\\mathrm{rawModelScalePLN} / \\mathrm{SimParams.gdpRatio}$"
    val rows         = bundle.reconciliation.rows.map: row =>
      Vector(
        row.label,
        formatAmountMacroPln(row.expectedRaw),
        formatAmountMacroPln(row.actualRaw),
        formatAmountMacroPln(row.residualRaw),
        row.status,
        reconciliationChannels(row).mkString("<br>"),
        s"${row.source} ${row.note}",
      )

    renderMarkdownTable(
      s"""<!-- schema=${metadata.schemaVersion} seed=${metadata.seed} month=${metadata.executionMonth} commit=${metadata.commit} sfc=${metadata.sfcStatus} matrix=${metadata.matrixStatus} output=stock-flow-reconciliation ${reconciliationScaleMetadata} -->
         |
         |${markdownSourceContract}
         |
         |# Stock-Flow Reconciliation and Revaluation Evidence
         |
         |Rows compare independently sourced transaction, revaluation, default, write-off, and other-change channels with observed stock deltas or level identities. Residual is actual minus expected. Displayed monetary columns are macro-scaled PLN ($scaleFormula) to match Monte Carlo `macroPln`; identity validation remains on raw model-scale fixed-point PLN.
         |""".stripMargin,
      Vector("Identity", "Expected (macro PLN)", "Actual (macro PLN)", "Residual (macro PLN)", "Status", "Runtime channels", "Source"),
      rows,
    )

  private def renderMarkdownTable(prefix: String, header: Vector[String], rows: Vector[Vector[String]]): String =
    val tableHeader = markdownRow(header)
    val separator   = markdownRow(header.map(_ => "---"))
    val body        = rows.map(markdownRow)
    (Vector(prefix.trim, "", tableHeader, separator) ++ body).mkString("\n") + "\n"

  private def markdownSourceContract: String =
    "Generated artifact. Do not edit by hand; regenerate with `sbt \"sfcMatrices --seed 1 --months 12 --out docs/sfc-matrix-artifacts --format md --commit committed-snapshot\"`."

  private def markdownRow(values: Vector[String]): String =
    values.map(escapeMarkdown).mkString("| ", " | ", " |")

  private def latexSymbol(value: String): String =
    mathSymbol(value)

  private def markdownSymbol(value: String): String =
    mathSymbol(value)

  private def mathSymbol(value: String): String =
    if value.isBlank then ""
    else "$" + value + "$"

  private def latexTextList(values: Vector[String]): String =
    values.map(escapeLatex).mkString("\\newline ")

  private def assetLabel(asset: AssetType): String =
    val metadata = SfcMatrixRegistry.instrument(asset)
    s"${metadata.label} (${asset.toString})"

  private def mechanismLabel(mechanism: MechanismId): String =
    val metadata = SfcMatrixRegistry.mechanism(mechanism)
    s"${metadata.label} [id: ${mechanism.toInt}]"

  private def reconciliationChannels(row: StockFlowReconciliationCell): Vector[String] =
    val channels = (row.assets.map(assetLabel) ++ row.mechanisms.map(mechanismLabel)).distinct
    if channels.nonEmpty then channels else Vector("No first-class runtime asset or mechanism")

  private def symbolicRows(row: FlowMechanismSemantics.Row): Vector[String] =
    if row.symbolicRows.nonEmpty then row.symbolicRows else Vector("No symbolic row; see SFC / reconciliation impact")

  private def formatAmountMacroPln(value: Long)(using p: SimParams): String =
    p.macroPln(PLN.fromRaw(value)).format(4)

  private def reconciliationScaleMetadata(using p: SimParams): String =
    s"money_scale=macro_pln gdp_ratio=${p.gdpRatio.format(8)} raw_validation=model_scale_pln"

  private[matrix] def escapeLatex(value: String): String =
    value.flatMap:
      case '\\' => "\\textbackslash{}"
      case '&'  => "\\&"
      case '%'  => "\\%"
      case '$'  => "\\$"
      case '#'  => "\\#"
      case '_'  => "\\_"
      case '{'  => "\\{"
      case '}'  => "\\}"
      case '~'  => "\\textasciitilde{}"
      case '^'  => "\\textasciicircum{}"
      case ch   => ch.toString

  private[matrix] def escapeMarkdown(value: String): String =
    value
      .replace("\\", "\\\\")
      .replace("|", "\\|")
      .replace("\n", "<br>")
      .replace("\r", "")

end SfcMatrixRenderers
