val scala3Version = "3.7.4"

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

lazy val root = project
  .in(file("."))
  .settings(
    name := "minifumo-lang",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test
    ),

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Wunused:all",
      "-Wvalue-discard"
    ),

    run / fork := true,
    Test / parallelExecution := false
  )
