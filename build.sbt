lazy val ledger = ProjectRef(file("modules/ledger"), "root")

lazy val root = project
  .in(file("."))
  .dependsOn(ledger)
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
      "org.scalatest"     %% "scalatest"       % "3.2.19"   % Test,
      "org.scalacheck"    %% "scalacheck"      % "1.18.1"   % Test,
      "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
      "dev.zio"           %% "zio-test"        % "2.1.16"   % Test,
      "dev.zio"           %% "zio-test-sbt"    % "2.1.16"   % Test,
      "com.github.lalyos"  % "jfiglet"         % "0.0.9",
    ),
  )
