import complete.DefaultParsers.spaceDelimited

lazy val ledger = ProjectRef(file("modules/ledger"), "root")

lazy val sfcMatrices = inputKey[Unit]("Generate symbolic SFC BSM/TFM matrix artifacts")
lazy val robustnessReport = inputKey[Unit]("Generate lightweight sensitivity and robustness artifacts")
lazy val scenarioRun = inputKey[Unit]("Run named scenario-registry experiments")
lazy val empiricalValidation = inputKey[Unit]("Generate empirical validation baseline snapshot artifacts")
lazy val calibrationRegister = inputKey[Unit]("Generate calibration register docs from typed provenance registry")
lazy val householdCreditStressCalibration = inputKey[Unit]("Generate household liquidity and credit-stress calibration artifacts")
lazy val bankBalanceSheetBenchmark = inputKey[Unit]("Generate initial bank balance-sheet benchmark artifacts")
lazy val bankFailureAblations = inputKey[Unit]("Run controlled bank-failure ablation diagnostics")
lazy val hhBankLeadLagDiagnostics = inputKey[Unit]("Run HH-to-bank lead-lag and causal attribution diagnostics")
lazy val loanOriginationQuality = inputKey[Unit]("Run HH/Firm loan-origination quality diagnostics")
lazy val productionSectorGvaShareBridge = inputKey[Unit]("Generate production-sector GVA share source bridge artifact")
lazy val productionSectorLaborSourceBridge = inputKey[Unit]("Generate production-sector firm, employment, and wage source bridge artifact")
lazy val ioTechnicalCoefficientBridge =
  inputKey[Unit]("Generate I-O technical-coefficient source comparison artifact")
lazy val openingBankBalanceProfileBridge =
  inputKey[Unit]("Generate opening bank balance profile source bridge artifact")

lazy val baseScalacOptions = Seq(
  "-Werror",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-explain",
  "-Wunused:all",
)

lazy val testDeps = Seq(
  "org.scalatest"     %% "scalatest"       % "3.2.20"   % Test,
  "org.scalacheck"    %% "scalacheck"      % "1.19.0"   % Test,
  "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
  "dev.zio"           %% "zio-test"        % "2.1.26"   % Test,
  "dev.zio"           %% "zio-test-sbt"    % "2.1.26"   % Test,
)

lazy val zioDeps = Seq(
  "dev.zio" %% "zio"         % "2.1.26",
  "dev.zio" %% "zio-streams" % "2.1.26",
)

lazy val cliDeps = Seq(
  "com.github.lalyos" % "jfiglet" % "0.0.9",
)

lazy val commonProjectSettings = Seq(
  organization := "com.boombustgroup",
  scalaVersion := "3.8.4",
  scalacOptions ++= baseScalacOptions,
)

lazy val excludeHeavyTestsByDefault = Test / testOptions ++= {
  val heavyTag = "com.boombustgroup.amorfati.tags.Heavy"
  // Standard `sbt test` skips @Heavy suites by default, including coverage
  // runs where scoverage instrumentation makes long simulations expensive.
  // Run the full suite with: `sbt -DamorFati.includeHeavyTests=true test`
  val includeHeavy = sys.props.get("amorFati.includeHeavyTests").exists { value =>
    val normalized = value.trim.toLowerCase
    normalized == "true" || normalized == "1" || normalized == "yes"
  }
  val excludeHeavyByDefault = !includeHeavy
  if (excludeHeavyByDefault) Seq(Tests.Argument(TestFrameworks.ScalaTest, "-l", heavyTag))
  else Nil
}

lazy val versionResourceGenerator = Def.task {
  import scala.sys.process.*

  val hash = try "git rev-parse --short HEAD".!!.trim
  catch { case _: Exception => "unknown" }
  val dirty = try {
    val s = "git status --porcelain".!!.trim; if (s.nonEmpty) "-dirty" else ""
  } catch { case _: Exception => "" }
  val file = (Compile / resourceManaged).value / "version.properties"
  IO.write(file, s"git.commit=$hash$dirty\n")
  Seq(file)
}

lazy val tsv = project
  .in(file("modules/tsv"))
  .settings(commonProjectSettings)
  .settings(
    name := "amor-fati-tsv",
    libraryDependencies ++= zioDeps ++ testDeps,
    excludeHeavyTestsByDefault,
  )

lazy val model = project
  .in(file("modules/model"))
  .dependsOn(ledger, tsv)
  .settings(commonProjectSettings)
  .settings(
    name := "amor-fati-model",
    Compile / resourceGenerators += versionResourceGenerator.taskValue,
    libraryDependencies ++= zioDeps ++ testDeps,
    excludeHeavyTestsByDefault,
  )

lazy val monteCarlo = project
  .in(file("modules/montecarlo"))
  .dependsOn(
    tsv,
    model % "compile->compile;test->test",
  )
  .settings(commonProjectSettings)
  .settings(
    name := "amor-fati-montecarlo",
    libraryDependencies ++= zioDeps ++ testDeps,
    excludeHeavyTestsByDefault,
  )

lazy val cli = project
  .in(file("modules/cli"))
  .dependsOn(
    tsv,
    model      % "compile->compile;test->test",
    monteCarlo % "compile->compile;test->test",
  )
  .settings(commonProjectSettings)
  .settings(
    name                       := "amor-fati-cli",
    Compile / mainClass        := Some("com.boombustgroup.amorfati.Main"),
    assembly / mainClass       := Some("com.boombustgroup.amorfati.Main"),
    assembly / assemblyJarName := "amor-fati.jar",
    libraryDependencies ++= zioDeps ++ cliDeps ++ testDeps,
    excludeHeavyTestsByDefault,
  )

lazy val integrationTests = project
  .in(file("integration-tests"))
  .dependsOn(
    tsv,
    model      % "compile->compile;test->test",
    monteCarlo % "compile->compile;test->test",
  )
  .settings(commonProjectSettings)
  .settings(
    name := "amor-fati-integration-tests",
    libraryDependencies ++= zioDeps ++ testDeps,
    excludeHeavyTestsByDefault,
  )

def runCliMain(mainClassName: String, args: Seq[String]) =
  (cli / Compile / runMain).toTask(s" $mainClassName ${args.mkString(" ")}")

lazy val root = project
  .in(file("."))
  .dependsOn(cli)
  .aggregate(tsv, model, monteCarlo, cli, integrationTests)
  .settings(commonProjectSettings)
  .settings(
    name                       := "amor-fati",
    Compile / mainClass        := Some("com.boombustgroup.amorfati.Main"),
    assembly / mainClass       := Some("com.boombustgroup.amorfati.Main"),
    assembly / assemblyJarName := "amor-fati.jar",
    assembly / aggregate       := false,
    publish / skip            := true,
    sfcMatrices := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<sfc matrix args>").parsed
        runCliMain("com.boombustgroup.amorfati.diagnostics.SfcMatrixExport", parsedArgs)
      }
      .evaluated,
    robustnessReport := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<robustness args>").parsed
        runCliMain("com.boombustgroup.amorfati.diagnostics.SensitivityRobustnessExport", parsedArgs)
      }
      .evaluated,
    scenarioRun := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<scenario args>").parsed
        runCliMain("com.boombustgroup.amorfati.diagnostics.ScenarioRunExport", parsedArgs)
      }
      .evaluated,
    empiricalValidation := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<empirical validation args>").parsed
        runCliMain("com.boombustgroup.amorfati.diagnostics.EmpiricalValidationExport", parsedArgs)
      }
      .evaluated,
    calibrationRegister := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<calibration register args>").parsed
        runCliMain("com.boombustgroup.amorfati.diagnostics.CalibrationRegisterExport", parsedArgs)
      }
      .evaluated,
    householdCreditStressCalibration := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<household credit stress args>").parsed
        runCliMain("com.boombustgroup.amorfati.diagnostics.HouseholdCreditStressCalibrationExport", parsedArgs)
      }
      .evaluated,
    bankBalanceSheetBenchmark := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<bank balance-sheet benchmark args>").parsed
        runCliMain("com.boombustgroup.amorfati.diagnostics.BankBalanceSheetBenchmarkExport", parsedArgs)
      }
      .evaluated,
    bankFailureAblations := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<bank failure ablation args>").parsed
        runCliMain("com.boombustgroup.amorfati.diagnostics.BankFailureAblationExport", parsedArgs)
      }
      .evaluated,
    hhBankLeadLagDiagnostics := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<HH-bank lead-lag args>").parsed
        runCliMain("com.boombustgroup.amorfati.diagnostics.HhBankLeadLagDiagnosticsExport", parsedArgs)
      }
      .evaluated,
    loanOriginationQuality := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<loan-origination quality args>").parsed
        runCliMain("com.boombustgroup.amorfati.diagnostics.LoanOriginationQualityExport", parsedArgs)
      }
      .evaluated,
    productionSectorGvaShareBridge := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<production-sector GVA share bridge args>").parsed
        runCliMain("com.boombustgroup.amorfati.diagnostics.ProductionSectorGvaShareBridgeExport", parsedArgs)
      }
      .evaluated,
    productionSectorLaborSourceBridge := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<production-sector labor source bridge args>").parsed
        runCliMain("com.boombustgroup.amorfati.diagnostics.ProductionSectorLaborSourceBridgeExport", parsedArgs)
      }
      .evaluated,
    ioTechnicalCoefficientBridge := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<I-O technical-coefficient bridge args>").parsed
        runCliMain("com.boombustgroup.amorfati.diagnostics.IoTechnicalCoefficientBridgeExport", parsedArgs)
      }
      .evaluated,
    openingBankBalanceProfileBridge := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<opening bank balance profile bridge args>").parsed
        runCliMain("com.boombustgroup.amorfati.diagnostics.OpeningBankBalanceProfileBridgeExport", parsedArgs)
      }
      .evaluated,
  )
