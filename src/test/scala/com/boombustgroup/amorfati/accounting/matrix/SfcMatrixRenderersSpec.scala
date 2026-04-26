package com.boombustgroup.amorfati.accounting.matrix

import com.boombustgroup.amorfati.accounting.matrix.SfcMatrixEvidence.MatrixEvidenceBundle
import com.boombustgroup.amorfati.accounting.matrix.SfcMatrixRenderers.OutputFormat
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.MonthRandomness
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SfcMatrixRenderersSpec extends AnyFlatSpec with Matchers:

  private given SimParams = SimParams.defaults

  private lazy val bundle =
    val init = WorldInit.initialize(InitRandomness.Contract.fromSeed(1L))
    val step = FlowSimulation.step(FlowSimulation.SimState.fromInit(init), MonthRandomness.Contract.fromSeed(1001L))
    MatrixEvidenceBundle.fromStep(seed = 1L, step = step, commit = "renderer-test")

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
    )

    byName("symbolic-bsm.tex") should include("\\begin{tabular}")
    byName("symbolic-bsm.tex") should include("$+D_h$")
    byName("symbolic-tfm.md") should include("| Flow \\\\ Sector |")
    byName("symbolic-tfm.md") should include("Consumption")
    byName("matrix-mapping.md") should include("Demand deposits")
    byName("matrix-mapping.md") should include("Household consumption")
  }

  it should "parse output format lists and reject unknown formats" in {
    OutputFormat.parseList("latex,md") shouldBe Right(OutputFormat.Default)
    OutputFormat.parseList("tex") shouldBe Right(Vector(OutputFormat.Latex))
    OutputFormat.parseList("markdown") shouldBe Right(Vector(OutputFormat.Markdown))
    OutputFormat.parseList("yaml").isLeft shouldBe true
  }

end SfcMatrixRenderersSpec
