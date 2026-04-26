import complete.DefaultParsers.spaceDelimited

lazy val ledger = ProjectRef(file("modules/ledger"), "root")

lazy val sfcMatrices = inputKey[Unit]("Generate symbolic SFC BSM/TFM matrix artifacts")
lazy val robustnessReport = inputKey[Unit]("Generate lightweight sensitivity and robustness artifacts")

lazy val baseScalacOptions = Seq(
  "-Werror",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-explain",
  "-Wunused:all",
)

lazy val testDeps = Seq(
  "org.scalatest"     %% "scalatest"       % "3.2.19"   % Test,
  "org.scalacheck"    %% "scalacheck"      % "1.18.1"   % Test,
  "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
  "dev.zio"           %% "zio-test"        % "2.1.16"   % Test,
  "dev.zio"           %% "zio-test-sbt"    % "2.1.16"   % Test,
)

lazy val commonProjectSettings = Seq(
  organization := "com.boombustgroup",
  scalaVersion := "3.8.2",
  scalacOptions ++= baseScalacOptions,
)

lazy val root = project
  .in(file("."))
  .dependsOn(ledger)
  .settings(commonProjectSettings)
  .settings(
    name                       := "amor-fati",
    mainClass                  := Some("com.boombustgroup.amorfati.Main"),
    assembly / assemblyJarName := "amor-fati.jar",
    Compile / resourceGenerators += Def.task {
      import scala.sys.process.*
      val hash = try "git rev-parse --short HEAD".!!.trim
      catch { case _: Exception => "unknown" }
      val dirty = try {
        val s = "git status --porcelain".!!.trim; if (s.nonEmpty) "-dirty" else ""
      } catch { case _: Exception => "" }
      val file = (Compile / resourceManaged).value / "version.properties"
      IO.write(file, s"git.commit=$hash$dirty\n")
      Seq(file)
    },
    libraryDependencies ++= Seq(
      "dev.zio"           %% "zio"             % "2.1.16",
      "dev.zio"           %% "zio-streams"     % "2.1.16",
      "com.github.lalyos"  % "jfiglet"         % "0.0.9",
    ) ++ testDeps,
    sfcMatrices := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<sfc matrix args>").parsed
        (Compile / runMain)
          .toTask(" com.boombustgroup.amorfati.diagnostics.SfcMatrixExport " + parsedArgs.mkString(" "))
      }
      .evaluated,
    robustnessReport := Def
      .inputTaskDyn {
        val parsedArgs = spaceDelimited("<robustness args>").parsed
        (Compile / runMain)
          .toTask(" com.boombustgroup.amorfati.diagnostics.SensitivityRobustnessExport " + parsedArgs.mkString(" "))
      }
      .evaluated,
    Test / testOptions ++= {
      val heavyTag         = "com.boombustgroup.amorfati.tags.Heavy"
      // Local `sbt test` skips @Heavy suites by default.
      // Run the full suite locally with: `sbt -DamorFati.includeHeavyTests=true test`
      val includeHeavy     = sys.props.get("amorFati.includeHeavyTests").exists { value =>
        val normalized = value.trim.toLowerCase
        normalized == "true" || normalized == "1" || normalized == "yes"
      }
      val runningOnCi      = sys.env.get("CI").contains("true") || sys.env.get("GITHUB_ACTIONS").contains("true")
      val excludeHeavyByDefault = !includeHeavy && !runningOnCi
      if (excludeHeavyByDefault) Seq(Tests.Argument(TestFrameworks.ScalaTest, "-l", heavyTag))
      else Nil
    },
  )

lazy val integrationTests = project
  .in(file("integration-tests"))
  .dependsOn(root)
  .settings(commonProjectSettings)
  .settings(
    name := "amor-fati-integration-tests",
    libraryDependencies ++= testDeps,
  )
