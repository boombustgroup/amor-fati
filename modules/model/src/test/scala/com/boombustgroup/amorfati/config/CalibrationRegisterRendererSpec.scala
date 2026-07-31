package com.boombustgroup.amorfati.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class CalibrationRegisterRendererSpec extends AnyFlatSpec with Matchers:

  import CalibrationProvenance.CalibrationStatus

  "CalibrationRegisterRenderer" should "match the checked-in calibration register" in {
    val rendered  = CalibrationRegisterRenderer.render()
    val checkedIn = Files.readString(Path.of("docs/calibration-register.md"), StandardCharsets.UTF_8)

    checkedIn shouldBe rendered
  }

  it should "render status counts from the typed registry" in {
    val rendered = CalibrationRegisterRenderer.render()
    val counts   = CalibrationProvenance.Baseline.statusCounts

    CalibrationStatus.values.foreach: status =>
      rendered should include(s"| `${status.token}` | ${counts.getOrElse(status, 0)} |")
  }

  it should "render typed placeholder decisions" in {
    val rendered = CalibrationRegisterRenderer.render()

    rendered should include("## Placeholder Decisions")
    rendered should include("| `immigration.initStock` | `StartupPlaceholder` |")
    rendered should include("Opening migration-stock comparisons")
  }

  it should "render formula values as GitHub MathJax instead of code spans" in {
    val rendered = CalibrationRegisterRenderer.render()

    rendered should include("""| `household.count` | $\mathrm{firmsCount} \cdot \mathrm{workersPerFirm} = 100000$ |""")
    rendered should include("""| `social.demInitialRetirees` | $\mathrm{pop.firmsCount} \cdot \mathrm{pop.workersPerFirm} / 3$ |""")
    rendered should not include "`firmsCount * workersPerFirm = 100000`"
    rendered should not include "`pop.firmsCount * pop.workersPerFirm / 3`"
  }

  it should "render structured source metadata" in {
    val rendered = CalibrationRegisterRenderer.render()

    rendered should include("## Structured Source Metadata")
    rendered should include("| `fiscal.govBaseSpending` | Fiscal stance | `MF state-budget 2026 spending plan` |")
    rendered should include("| `banking.initGovBonds`, `initNbpGovBonds` | Banking and central-bank balance sheets |")
  }

  it should "render tuned validation evidence modes and missing evidence paths" in {
    val rendered = CalibrationRegisterRenderer.render()

    rendered should include("## Tuned Validation Evidence")
    rendered should include("| Validation mode | Count | Linked evidence paths | Missing evidence paths |")
    rendered should include(
      "| `household.mpc` | `SENSITIVITY_RANGE` | docs/sensitivity-robustness-workflow.md | sensitivity-summary.tsv | mpc-low, mpc-high |",
    )
    rendered should include("| `banking.depositPanicRate` | `SENSITIVITY_RANGE` | `MISSING_VALIDATION_EVIDENCE` |")
  }

  it should "fail fast when a placeholder row lacks typed decision metadata" in {
    val brokenPlaceholder = CalibrationProvenance.CalibrationParameter(
      id = "immigration.initStock",
      parameterIds = Vector("immigration.initStock"),
      renderedValue = "0",
      unit = "agents",
      provenance = "test",
      empiricalTarget = "test",
      transformation = "test",
      ownerModules = Vector("ImmigrationConfig"),
      status = CalibrationProvenance.CalibrationStatus.Placeholder,
    )

    val thrown = intercept[IllegalArgumentException]:
      CalibrationRegisterRenderer.render(Vector(brokenPlaceholder))

    thrown.getMessage should include("Missing placeholder decision")
    thrown.getMessage should include("immigration.initStock")
  }

  it should "fail fast when a tuned row lacks validation evidence metadata" in {
    val brokenTuned = CalibrationProvenance.CalibrationParameter(
      id = "household.mpc",
      parameterIds = Vector("household.mpc"),
      renderedValue = "0.92",
      unit = "share",
      provenance = "test",
      empiricalTarget = "test",
      transformation = "test",
      ownerModules = Vector("HouseholdConfig"),
      status = CalibrationProvenance.CalibrationStatus.TunedNeedsValidation,
    )

    val thrown = intercept[IllegalArgumentException]:
      CalibrationRegisterRenderer.render(Vector(brokenTuned))

    thrown.getMessage should include("Missing tuned validation evidence mode")
    thrown.getMessage should include("household.mpc")
  }

end CalibrationRegisterRendererSpec
