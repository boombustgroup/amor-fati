val scala3Version = "3.8.2"

lazy val root = project
  .in(file("."))
  .settings(
    organization := "com.boombustgroup",
    name         := "amor-fati",
    version      := "0.2.0",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-Werror",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-explain",
      "-Wunused:all",
      "-Wconf:msg=Alphanumeric method.*is not declared infix:s",
    ),
    Compile / mainClass := Some("com.boombustgroup.amorfati.sfcMonteCarlo"),
    assembly / mainClass := Some("com.boombustgroup.amorfati.sfcMonteCarlo"),
    assembly / assemblyJarName := "amor-fati.jar",
    // Disable parallel test execution: production code uses global scala.util.Random,
    // so concurrent suites would cause non-deterministic interleaving (breaks reproducibility test)
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.scalatest"     %% "scalatest"       % "3.2.19"   % Test,
      "org.scalacheck"    %% "scalacheck"      % "1.18.1"   % Test,
      "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test
    )
  )
