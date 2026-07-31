package com.boombustgroup.amorfati.accounting.matrix

import com.boombustgroup.amorfati.accounting.matrix.SfcMatrixRenderers.OutputFormat
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.fp.FixedPointBase
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SfcMatrixRenderersSpec extends AnyFlatSpec with Matchers:

  private given SimParams = SimParams.defaults

  private lazy val bundle =
    SfcMatrixEvidenceTestSupport.deterministicBundle(commit = "renderer-test")

  "SfcMatrixRenderers" should "escape LaTeX labels deterministically" in {
    SfcMatrixRenderers.escapeLatex("A&B_%$#{}~^") should include("\\&")
    SfcMatrixRenderers.escapeLatex("A&B_%$#{}~^") should include("\\_")
  }

  it should "render symbolic LaTeX and Markdown artifacts with runtime mapping" in {
    val artifacts = SfcMatrixRenderers.renderSymbolicBundle(bundle, OutputFormat.Default)
    val byName    = artifacts.map(artifact => artifact.relativePath -> artifact.contents).toMap

    byName.keySet shouldBe Set(
      "symbolic-bsm.tex",
      "symbolic-bsm.md",
      "symbolic-tfm.tex",
      "symbolic-tfm.md",
      "matrix-mapping.tex",
      "matrix-mapping.md",
      "flow-mechanism-semantics.tex",
      "flow-mechanism-semantics.md",
      "stock-flow-reconciliation.tex",
      "stock-flow-reconciliation.md",
    )

    byName("symbolic-bsm.tex") should include("\\begin{tsv}")
    byName("symbolic-bsm.tex") should include("$+D_h$")
    byName("symbolic-bsm.md") should include("$+D_h$")
    byName("symbolic-tfm.md") should include("| Flow \\\\ Sector |")
    byName("symbolic-tfm.md") should include("Consumption")
    byName("symbolic-tfm.md") should include("Equity revaluation")
    byName("matrix-mapping.md") should include("Demand deposits")
    byName("matrix-mapping.md") should include("$+D_h$")
    byName("matrix-mapping.md") should include("Household consumption")
    byName("matrix-mapping.md") should include("Equity revaluation [id: 57]")
    byName("flow-mechanism-semantics.md") should include(s"${FlowMechanismSemantics.rows.size} runtime-emitted `FlowMechanism` entries")
    byName("flow-mechanism-semantics.md") should include("Bank firm-loan interest")
    byName("flow-mechanism-semantics.md") should include("BankCapital SFC identity")
    byName("flow-mechanism-semantics.tex") should include("\\begin{longtable}")
    byName("stock-flow-reconciliation.md") should include("Net foreign assets")
    byName("stock-flow-reconciliation.md") should include("FX valuation")
    byName("stock-flow-reconciliation.tex") should include("\\begin{longtable}")
  }

  it should "render reconciliation monetary columns as explicit macro PLN values" in {
    val artifacts           = SfcMatrixRenderers.renderSymbolicBundle(bundle, Vector(OutputFormat.Markdown, OutputFormat.Latex))
    val byName              = artifacts.map(artifact => artifact.relativePath -> artifact.contents).toMap
    val row                 = bundle.reconciliation.rows
      .find(row => row.expectedRaw != 0L && formatRaw(row.expectedRaw) != formatMacroPln(row.expectedRaw))
      .getOrElse(fail("Expected at least one non-zero reconciliation row whose raw and macro-scaled amounts differ"))
    val expectedMacroAmount = formatMacroPln(row.expectedRaw)

    val markdown = byName("stock-flow-reconciliation.md")
    val mdRow    = markdown.linesIterator
      .find(_.startsWith(s"| ${row.label} |"))
      .getOrElse(fail(s"Missing rendered reconciliation row for ${row.label}"))
    mdRow should include(s"| $expectedMacroAmount |")
    mdRow should not include s"| ${formatRaw(row.expectedRaw)} |"
    markdown should include("money_scale=macro_pln")
    markdown should include("raw_validation=model_scale_pln")
    markdown should include("Expected (macro PLN)")
    markdown should include("""macro-scaled PLN ($\mathrm{rawModelScalePLN} / \mathrm{SimParams.gdpRatio}$) to match Monte Carlo""")
    markdown should not include "`raw model-scale PLN / SimParams.gdpRatio`"
    markdown should include("identity validation remains on raw model-scale fixed-point PLN")

    val latex = byName("stock-flow-reconciliation.tex")
    latex should include("money_scale=macro_pln")
    latex should include("raw_validation=model_scale_pln")
    latex should include("Expected (macro PLN)")
    latex should include(expectedMacroAmount)
  }

  it should "parse output format lists and reject unknown formats" in {
    OutputFormat.parseList("latex,md") shouldBe Right(OutputFormat.Default)
    OutputFormat.parseList("tex") shouldBe Right(Vector(OutputFormat.Latex))
    OutputFormat.parseList("markdown") shouldBe Right(Vector(OutputFormat.Markdown))
    OutputFormat.parseList("yaml").isLeft shouldBe true
  }

  private def formatRaw(value: Long): String =
    FixedPointBase.format(value, 4)

  private def formatMacroPln(value: Long): String =
    val gdpRatioRaw = BigDecimal(SimParams.defaults.gdpRatio.toLong)
    (BigDecimal(value) / gdpRatioRaw).setScale(4, BigDecimal.RoundingMode.HALF_EVEN).toString

end SfcMatrixRenderersSpec
