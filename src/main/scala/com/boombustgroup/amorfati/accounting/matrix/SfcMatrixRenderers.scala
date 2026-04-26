package com.boombustgroup.amorfati.accounting.matrix

import com.boombustgroup.amorfati.accounting.matrix.SfcMatrixEvidence.*
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
          case "csv" | "json"    =>
            Left("SFC matrix export writes only tex and md; numeric evidence is already available from the engine CSV output.")
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
  ): Vector[RenderedArtifact] =
    val matrixArtifacts = SfcSymbolicMatrices.matrices.flatMap: matrix =>
      formats.map: format =>
        RenderedArtifact(s"${matrix.name}.${format.extension}", renderSymbolicMatrix(bundle.metadata, matrix, format))

    val mappingArtifacts = formats.map: format =>
      RenderedArtifact(s"matrix-mapping.${format.extension}", renderMapping(bundle.metadata, format))

    matrixArtifacts ++ mappingArtifacts

  def writeSymbolicBundle(
      bundle: MatrixEvidenceBundle,
      outDir: Path,
      formats: Vector[OutputFormat],
  ): Vector[Path] =
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
       |\\begin{tabular}{$columns}
       |$header \\\\
       |\\hline
       |${body.mkString("\n")}
       |\\end{tabular}
       |\\endgroup
       |""".stripMargin

  private def renderSymbolicMarkdown(metadata: MatrixMetadata, matrix: SfcSymbolicMatrices.SymbolicMatrix): String =
    val header = matrix.rowHeader +: SfcSymbolicMatrices.sectors.map(sector => SfcMatrixRegistry.sector(sector).label) :+ "Sum"
    val rows   = matrix.rows.map: row =>
      row.label +: SfcSymbolicMatrices.sectors.map(sector => row.cells.getOrElse(sector, "")) :+ row.zeroSymbol

    renderMarkdownTable(
      s"""<!-- schema=${metadata.schemaVersion} seed=${metadata.seed} month=${metadata.executionMonth} commit=${metadata.commit} sfc=${metadata.sfcStatus} matrix=${metadata.matrixStatus} output=symbolic -->
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
        row.symbols.mkString(", "),
        row.assets.map(assetLabel).mkString("<br>"),
        row.mechanisms.map(mechanismLabel).mkString("<br>"),
        row.note,
      )

    renderMarkdownTable(
      s"""<!-- schema=${metadata.schemaVersion} seed=${metadata.seed} month=${metadata.executionMonth} commit=${metadata.commit} sfc=${metadata.sfcStatus} matrix=${metadata.matrixStatus} output=symbolic-mapping -->
         |# Symbolic Matrix Mapping
         |""".stripMargin,
      Vector("Matrix", "Row", "Symbols", "Runtime assets", "Runtime mechanisms", "Note"),
      rows,
    )

  private def renderMarkdownTable(prefix: String, header: Vector[String], rows: Vector[Vector[String]]): String =
    val tableHeader = markdownRow(header)
    val separator   = markdownRow(header.map(_ => "---"))
    val body        = rows.map(markdownRow)
    (Vector(prefix.trim, tableHeader, separator) ++ body).mkString("\n") + "\n"

  private def markdownRow(values: Vector[String]): String =
    values.map(escapeMarkdown).mkString("| ", " | ", " |")

  private def latexSymbol(value: String): String =
    if value.isBlank then ""
    else "$" + value + "$"

  private def latexTextList(values: Vector[String]): String =
    values.map(escapeLatex).mkString("\\newline ")

  private def assetLabel(asset: AssetType): String =
    val metadata = SfcMatrixRegistry.instrument(asset)
    s"${metadata.label} (${asset.toString})"

  private def mechanismLabel(mechanism: MechanismId): String =
    val metadata = SfcMatrixRegistry.mechanism(mechanism)
    s"${metadata.label} (#${mechanism.toInt})"

  private[matrix] def formatPLN(raw: Long): String =
    s"PLN ${formatDecimal(raw)}"

  private[matrix] def formatDecimal(raw: Long): String =
    (BigDecimal(raw) / BigDecimal(10000)).setScale(4).bigDecimal.toPlainString

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
