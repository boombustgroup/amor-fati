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
    mainClass                  := Some("com.boombustgroup.amorfati.Main"),
    assembly / assemblyJarName := "amor-fati.jar",
    libraryDependencies ++= Seq(
      "dev.zio"           %% "zio"             % "2.1.16",
      "dev.zio"           %% "zio-streams"     % "2.1.16",
      "org.scalatest"     %% "scalatest"       % "3.2.19"   % Test,
      "org.scalacheck"    %% "scalacheck"      % "1.18.1"   % Test,
      "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
      "dev.zio"           %% "zio-test"        % "2.1.16"   % Test,
      "dev.zio"           %% "zio-test-sbt"    % "2.1.16"   % Test,
    ),
  )
