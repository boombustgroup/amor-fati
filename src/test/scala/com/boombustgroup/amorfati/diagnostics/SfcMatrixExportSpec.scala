package com.boombustgroup.amorfati.diagnostics

import com.boombustgroup.amorfati.accounting.matrix.SfcMatrixRenderers.OutputFormat
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class SfcMatrixExportSpec extends AnyFlatSpec with Matchers:

  "SfcMatrixExport" should "parse CLI arguments" in {
    val parsed = SfcMatrixExport.parseArgs(
      Vector("--seed", "7", "--months", "3", "--out", "target/sfc-export-parse", "--format", "md,tex"),
    )

    parsed.map(config => (config.seed, config.months, config.out, config.formats)) shouldBe
      Right((7L, 3, Path.of("target/sfc-export-parse"), Vector(OutputFormat.Markdown, OutputFormat.Latex)))

    SfcMatrixExport.parseArgs(Vector("--formats", "md")).map(_.formats) shouldBe
      Right(Vector(OutputFormat.Markdown))
  }

  it should "reject unsupported matrix formats" in {
    SfcMatrixExport.parseArgs(Vector("--format", "pdf")).isLeft shouldBe true
  }

  it should "report missing values for recognized flags" in {
    SfcMatrixExport.parseArgs(Vector("--seed")) shouldBe Left("Missing value for --seed")
    SfcMatrixExport.parseArgs(Vector("--months")) shouldBe Left("Missing value for --months")
    SfcMatrixExport.parseArgs(Vector("--out")) shouldBe Left("Missing value for --out")
    SfcMatrixExport.parseArgs(Vector("--format")) shouldBe Left("Missing value for --format")
    SfcMatrixExport.parseArgs(Vector("--formats")) shouldBe Left("Missing value for --format")
    SfcMatrixExport.parseArgs(Vector("--seed", "--months", "3")) shouldBe Left("Missing value for --seed")
  }

  it should "document format aliases in help output" in {
    SfcMatrixExport.parseArgs(Vector("--help")) match
      case Left(help) => help should include("--format|--formats")
      case Right(_)   => fail("Expected --help to return usage text")
  }

  it should "write generated artifacts under the requested output directory" in {
    Files.createDirectories(Path.of("target"))
    val out = Files.createTempDirectory(Path.of("target"), "sfc-export-spec-")

    try {
      val result       = SfcMatrixExport.run(
        SfcMatrixExport.Config(seed = 1L, months = 1, out = out, formats = Vector(OutputFormat.Markdown)),
      )
      val exportResult = result.fold(err => fail(err), identity)

      exportResult.paths.map(_.getFileName.toString).toSet shouldBe Set(
        "symbolic-bsm.md",
        "symbolic-tfm.md",
        "matrix-mapping.md",
        "stock-flow-reconciliation.md",
      )
      Files.exists(out.resolve("symbolic-bsm.md")) shouldBe true
      Files.exists(out.resolve("symbolic-tfm.md")) shouldBe true
      Files.exists(out.resolve("matrix-mapping.md")) shouldBe true
      Files.exists(out.resolve("stock-flow-reconciliation.md")) shouldBe true
      Files.exists(out.resolve("symbolic-bsm.tex")) shouldBe false
      Files.exists(out.resolve("symbolic-tfm.tex")) shouldBe false
      Files.exists(out.resolve("matrix-mapping.tex")) shouldBe false
      Files.exists(out.resolve("stock-flow-reconciliation.tex")) shouldBe false
    } finally deleteRecursively(out)
  }

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      val stream = Files.walk(path)
      try
        val paths = stream.iterator().asScala.toVector.sortBy(_.getNameCount).reverse
        paths.foreach(Files.deleteIfExists)
      finally stream.close()

end SfcMatrixExportSpec
