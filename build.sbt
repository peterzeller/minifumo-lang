import scala.sys.process._

val scala3Version = "3.7.4"
val antlr4Version = "4.13.2"

lazy val generateAntlr = taskKey[Seq[File]]("Generate ANTLR sources from grammar")

lazy val root = project
  .in(file("."))
  .settings(
    name := "minifumo-lang",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.antlr" % "antlr4" % antlr4Version,
      "org.antlr" % "antlr4-runtime" % antlr4Version,
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
    Test / parallelExecution := false,

    Compile / sourceGenerators += generateAntlr.taskValue,
    generateAntlr := {
      val log = streams.value.log
      val antlrSourceDir = (Compile / sourceDirectory).value / "antlr4"
      val grammar = antlrSourceDir / "Minifumo.g4"
      val outDir = (Compile / sourceManaged).value / "antlr4"
      val cp = (Compile / dependencyClasspath).value.files
      val javaBin = sys.props
        .get("java.home")
        .map(home => file(home) / "bin" / "java")
        .getOrElse(file("java"))

      IO.createDirectory(outDir)

      val args = Seq(
        javaBin.getAbsolutePath,
        "-cp",
        cp.mkString(java.io.File.pathSeparator),
        "org.antlr.v4.Tool",
        "-package",
        "com.github.peterzeller.minifumo.antlr",
        "-o",
        outDir.getAbsolutePath,
        grammar.getAbsolutePath
      )
      log.info(s"Generating ANTLR sources from ${grammar.getAbsolutePath} into ${outDir.getAbsolutePath}")
      val exitCode = Process(args, baseDirectory.value).!
      if (exitCode != 0) sys.error(s"ANTLR generation failed with exit code $exitCode")
      (outDir ** "*.java").get
    }
  )
