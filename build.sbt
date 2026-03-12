lazy val root = project
  .in(file("."))
  .settings(
    organization               := "com.boombustgroup",
    name                       := "amor-fati",
    scalaVersion               := "3.8.2",
    scalacOptions ++= Seq(
      "-Werror",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-explain",
      "-Wunused:all",
    ),
    mainClass                  := Some("com.boombustgroup.amorfati.simulate"),
    assembly / assemblyJarName := "amor-fati.jar",
    libraryDependencies ++= Seq(
      "org.scalatest"     %% "scalatest"       % "3.2.19"   % Test,
      "org.scalacheck"    %% "scalacheck"      % "1.18.1"   % Test,
      "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
    ),
  )
