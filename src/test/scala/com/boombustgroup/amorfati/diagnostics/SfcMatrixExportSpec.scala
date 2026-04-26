package com.boombustgroup.amorfati.diagnostics

import com.boombustgroup.amorfati.accounting.matrix.SfcMatrixRenderers.OutputFormat
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

class SfcMatrixExportSpec extends AnyFlatSpec with Matchers:

  "SfcMatrixExport" should "parse CLI arguments" in {
    val parsed = SfcMatrixExport.parseArgs(
      Vector("--seed", "7", "--months", "3", "--out", "target/sfc-export-parse", "--format", "md,tex"),
    )

    parsed.map(config => (config.seed, config.months, config.out, config.formats)) shouldBe
      Right((7L, 3, Path.of("target/sfc-export-parse"), Vector(OutputFormat.Markdown, OutputFormat.Latex)))
  }

  it should "reject CSV and JSON matrix formats" in {
    SfcMatrixExport.parseArgs(Vector("--format", "csv")).isLeft shouldBe true
    SfcMatrixExport.parseArgs(Vector("--format", "json")).isLeft shouldBe true
  }

  it should "write generated artifacts under the requested output directory" in {
    Files.createDirectories(Path.of("target"))
    val out = Files.createTempDirectory(Path.of("target"), "sfc-export-spec-")

    val result = SfcMatrixExport.run(
      SfcMatrixExport.Config(seed = 1L, months = 1, out = out, formats = Vector(OutputFormat.Markdown)),
    )

    result.isRight shouldBe true
    Files.exists(out.resolve("symbolic-bsm.md")) shouldBe true
    Files.exists(out.resolve("symbolic-tfm.md")) shouldBe true
    Files.exists(out.resolve("matrix-mapping.md")) shouldBe true
    Files.exists(out.resolve("symbolic-bsm.csv")) shouldBe false
    Files.exists(out.resolve("metadata.json")) shouldBe false
  }

end SfcMatrixExportSpec
