package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.config.PopulationControlBundleLoader.*
import com.boombustgroup.amorfati.config.PopulationControlSchema.PopulationScope
import com.boombustgroup.amorfati.config.PopulationControlSchema.ValidationError
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.jdk.CollectionConverters.*
import scala.util.Using

class PopulationControlBundleLoaderSpec extends AnyFlatSpec with Matchers:

  private val FixtureDigest = "dd889e926eec9a6cabd13394e2a029f4f3efea547bf5e50dc67c635983be210b"

  private def fixtureRoot: Path =
    Path.of(Option(getClass.getResource("/population-control-bundles/synthetic-v1")).getOrElse(fail("synthetic population-control fixture is missing")).toURI)

  private def withCopiedFixture[A](f: Path => A): A =
    val destination = Files.createTempDirectory("population-control-bundle-")
    try
      pathsUnder(fixtureRoot).foreach: source =>
        val target = destination.resolve(fixtureRoot.relativize(source))
        if Files.isDirectory(source) then Files.createDirectories(target)
        else Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING)
      f(destination)
    finally deleteTree(destination)

  private def refreshDigest(root: Path): Unit =
    val originalManifest = PopulationControlBundleLoader.load(fixtureRoot).fold(error => fail(s"fixture must load before mutation: $error"), _.manifest)
    val actualDigest     =
      PopulationControlBundleLoader.computeDigest(root, originalManifest).fold(error => fail(s"cannot recompute fixture digest: $error"), identity)
    val manifestPath     = root.resolve("manifest.tsv")
    val updated          = Files.readString(manifestPath, UTF_8).replace(FixtureDigest, actualDigest.toString)
    Files.writeString(manifestPath, updated, UTF_8)

  private def deleteTree(root: Path): Unit =
    if Files.exists(root) then pathsUnder(root).sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists)

  private def pathsUnder(root: Path): Vector[Path] =
    Using.resource(Files.walk(root))(_.iterator().asScala.toVector)

  "PopulationControlBundleLoader" should "load and reconcile a synthetic data-only bundle" in {
    val loaded = PopulationControlBundleLoader.load(fixtureRoot).fold(error => fail(s"unexpected loader failure: $error"), identity)

    loaded.manifest.baseline.id.toString shouldBe "synthetic-population-controls-v1"
    loaded.manifest.populationScope shouldBe PopulationScope.AllUsualResidents
    loaded.controls.populationScope shouldBe PopulationScope.AllUsualResidents
    loaded.manifest.populationControlsDigest.toString shouldBe FixtureDigest
    loaded.controls.persons.rows.size shouldBe 9
    loaded.controls.employment.rows.size shouldBe 4
    loaded.validation.isValid shouldBe true
  }

  it should "reject a changed control table when its manifest digest was not updated" in
    withCopiedFixture: root =>
      val personsPath = root.resolve("persons.tsv")
      Files.writeString(
        personsPath,
        Files.readString(personsPath, UTF_8).replace("north\tfemale\t0-14\tprivate_household\t10", "north\tfemale\t0-14\tprivate_household\t11"),
        UTF_8,
      )

      PopulationControlBundleLoader.load(root) match
        case Left(LoadError.PopulationControlsDigestMismatch(expected, actual)) =>
          expected.toString shouldBe FixtureDigest
          actual should not be expected
        case other                                                              => fail(s"expected population-controls digest mismatch, got: $other")

  it should "reject an unknown population scope in the manifest" in
    withCopiedFixture: root =>
      val manifestPath = root.resolve("manifest.tsv")
      Files.writeString(
        manifestPath,
        Files.readString(manifestPath, UTF_8).replace("all_usual_residents", "not-a-scope"),
        UTF_8,
      )

      PopulationControlBundleLoader.load(root) match
        case Left(LoadError.InvalidManifest(path, detail)) =>
          path shouldBe manifestPath
          detail should include("unknown population scope")
        case other                                         => fail(s"expected invalid population scope, got: $other")

  it should "reject malformed UTF-8 after its digest has been refreshed" in
    withCopiedFixture: root =>
      val personsPath = root.resolve("persons.tsv")
      Files.write(personsPath, Array(0x80.toByte))
      refreshDigest(root)

      PopulationControlBundleLoader.load(root) match
        case Left(LoadError.InvalidTsv(path, detail)) =>
          path shouldBe personsPath
          detail should include("invalid UTF-8")
        case other                                    => fail(s"expected invalid UTF-8 failure, got: $other")

  it should "reject invalid source metadata after its digest has been refreshed" in
    withCopiedFixture: root =>
      val metadataPath = root.resolve("tables.tsv")
      Files.writeString(
        metadataPath,
        Files
          .readString(metadataPath, UTF_8)
          .replace("\npersons\t", "\n\n# retained comment\npersons\t")
          .replace("2026-01-01", "not-a-date"),
        UTF_8,
      )
      refreshDigest(root)

      PopulationControlBundleLoader.load(root) match
        case Left(LoadError.InvalidControlRow(path, lineNumber, detail)) =>
          path shouldBe metadataPath
          lineNumber shouldBe 4
          detail should include("accessed_at")
        case other                                                       => fail(s"expected invalid source metadata, got: $other")

  it should "reject an unreconciled bundle after its digest has been refreshed" in
    withCopiedFixture: root =>
      val employmentPath = root.resolve("employment.tsv")
      Files.writeString(
        employmentPath,
        Files.readString(employmentPath, UTF_8).replace("south\tsouth\tservices\t20", "south\tsouth\tservices\t19"),
        UTF_8,
      )
      refreshDigest(root)

      PopulationControlBundleLoader.load(root) match
        case Left(LoadError.ControlValidationFailed(errors)) =>
          val hasEmploymentReconciliationFailure = errors.exists:
            case ValidationError.FailedReconciliation(reconciliation) => reconciliation.id == "employed-residents-to-primary-job-assignments:RegionCode(south)"
            case _                                                    => false
          hasEmploymentReconciliationFailure shouldBe true
        case other                                           => fail(s"expected reconciliation failure, got: $other")

end PopulationControlBundleLoaderSpec
