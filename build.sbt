val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "sfc-abm-core",
    version      := "0.2.0",
    scalaVersion := scala3Version,
    Compile / mainClass := Some("sfc.sfcMonteCarlo"),
    assembly / mainClass := Some("sfc.sfcMonteCarlo"),
    assembly / assemblyJarName := "sfc-abm.jar",
    // Disable parallel test execution: production code uses global scala.util.Random,
    // so concurrent suites would cause non-deterministic interleaving (breaks reproducibility test)
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.scalatest"     %% "scalatest"       % "3.2.19"   % Test,
      "org.scalacheck"    %% "scalacheck"      % "1.18.1"   % Test,
      "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test
    )
  )
