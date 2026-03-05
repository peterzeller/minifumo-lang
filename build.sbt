import org.scalajs.linker.interface.ModuleKind
import sbt.io.IO

val scala3Version = "3.8.2"

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

lazy val commonSettings = Seq(
  scalaVersion := scala3Version,
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Wunused:all",
    "-Wvalue-discard"
  )
)

lazy val coreJvm = project
  .in(file("."))
  .settings(commonSettings)
  .settings(
    name := "minifumo-lang",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test
    ),
    run / fork := true,
    Test / parallelExecution := false
  )

lazy val compilerJs = project
  .in(file("compiler-js"))
  .enablePlugins(org.scalajs.sbtplugin.ScalaJSPlugin)
  .settings(commonSettings)
  .settings(
    name := "minifumo-compiler-js",
    Compile / unmanagedSourceDirectories += (coreJvm / Compile / scalaSource).value,
    Compile / unmanagedSources := {
      val coreStandard = (coreJvm / baseDirectory).value.getAbsolutePath + "/src/main/scala/com/github/peterzeller/minifumo/builtins/Standard.scala"
      val coreMain = (coreJvm / baseDirectory).value.getAbsolutePath + "/src/main/scala/com/github/peterzeller/minifumo/Main.scala"
      (Compile / unmanagedSources).value.filterNot { source =>
        val path = source.getAbsolutePath
        path == coreStandard || path == coreMain || path.contains("/backends/lean/")
      }
    },
    Compile / sourceGenerators += Def.task {
      val sourceFile = (coreJvm / baseDirectory).value / "src/main/resources/com/github/peterzeller/minifumo/builtins/standard.minifumo"
      val managedDir = (Compile / sourceManaged).value / "com/github/peterzeller/minifumo/builtins"
      val outputFile = managedDir / "StandardSource.scala"
      val standardText = IO.read(sourceFile).replace("\"\"\"", "\"\"\" + \"\"\"\" + \"\"\"")
      IO.write(
        outputFile,
        s"""package com.github.peterzeller.minifumo.builtins
           |
           |/** Stores the bundled Minifumo standard library source for Scala.js builds. */
           |object StandardSource:
           |  // Returns the embedded standard library text for browser compilation.
           |  val text: String =
           |    \"\"\"$standardText\"\"\"
           |""".stripMargin
      )
      Seq(outputFile)
    }.taskValue,
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule).withSourceMap(false))
  )
