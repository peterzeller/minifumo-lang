package com.github.peterzeller.minifumo.backends.scala

import com.github.peterzeller.minifumo.common.MinifumoErrorWithPath
import com.github.peterzeller.minifumo.typing.{DatatypeIndexErasure, GlobalSymbolsIo, ProjectSymbolCache, ProofErasure, TypeChecker}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Translates Minifumo source files to Scala 3 wrapper modules. */
object ScalaBackend:

  /** Represents one generated Scala source file. */
  final case class GeneratedScalaFile(path: Path, content: String)

  /** Contains all generated files for a translation run. */
  final case class CompilationResult(files: List[GeneratedScalaFile])

  /** Stores package and object names for one generated Scala module. */
  final case class ModuleRef(packageParts: List[String], objectName: String):
    /** Returns the fully qualified object name. */
    def qualifiedName: String =
      (packageParts :+ objectName).mkString(".")

  /** Compiles one Minifumo file or directory into Scala sources under the output directory. */
  def compile(entry: Path, outputDir: Path): Either[List[MinifumoErrorWithPath], CompilationResult] =
    val root = GlobalSymbolsIo.findProjectRoot(entry)
    val io = GlobalSymbolsIo(root)
    val cache = new ProjectSymbolCache(io, TypeChecker.IdSupply())
    val files = collectInputFiles(entry, root)

    files.foreach: file =>
      val relative = io.makeRelative(file)
      cache.typedAst(relative)

    val errors = cache.allErrors
    if errors.nonEmpty then
      Left(errors)
    else
      Files.createDirectories(outputDir)
      val generatedFiles = files.map: file =>
        val relative = io.makeRelative(file)
        val (typed, _) = cache.typedAst(relative)
        val erasedProgram = ProofErasure.erase(DatatypeIndexErasure.erase(typed))
        val content = emitScalaModule(relative, erasedProgram.items.length, cache)
        val target = outputDir.resolve(relative.stripSuffix(".minifumo") + ".scala")
        Option(target.getParent).foreach(parent => Files.createDirectories(parent))
        Files.writeString(target, content)
        GeneratedScalaFile(target, content)
      Right(CompilationResult(generatedFiles))

  /** Collects file inputs for either a single-file or whole-directory translation request. */
  private def collectInputFiles(entry: Path, root: Path): List[Path] =
    if Files.isDirectory(entry) then
      val stream = Files.walk(entry)
      try
        stream.iterator().asScala
          .filter(Files.isRegularFile(_))
          .filter(_.toString.endsWith(".minifumo"))
          .toList
          .sortBy(path => root.relativize(path).toString)
      finally
        stream.close()
    else
      List(entry)

  /** Computes package/object names for one generated source file. */
  def moduleRefFor(relativePath: String): ModuleRef =
    val sanitizedRelative = relativePath.replace('\\', '/')
    val modulePath = sanitizedRelative.stripSuffix(".minifumo")
    val packageParts = modulePath.split('/').toList.dropRight(1).filter(_.nonEmpty).map(sanitizeIdentifier)
    val objectName = sanitizeTypeName(modulePath.split('/').toList.lastOption.getOrElse("Main"))
    ModuleRef(packageParts, objectName)

  /** Emits a Scala 3 module that runs the original Minifumo program through the interpreter entrypoint. */
  private def emitScalaModule(relativePath: String, erasedDeclCount: Int, cache: ProjectSymbolCache): String =
    val sanitizedRelative = relativePath.replace('\\', '/')
    val moduleRef = moduleRefFor(relativePath)
    val packageParts = moduleRef.packageParts
    val moduleName = moduleRef.objectName

    val imports =
      val (programAst, _) = cache.getAst(relativePath)
      programAst.imports.flatMap(_.from).distinct.map: imp =>
        val importedModule = imp.replace('\\', '/').stripSuffix(".minifumo")
        val impParts = importedModule.split('/').toList.filter(_.nonEmpty)
        if impParts.nonEmpty then
          val pkg = impParts.dropRight(1).map(sanitizeIdentifier)
          val obj = sanitizeTypeName(impParts.last)
          (pkg :+ obj).mkString(".")
        else
          sanitizeTypeName(importedModule)
    val packageLine = if packageParts.nonEmpty then s"package ${packageParts.mkString(".")}" else ""
    val importLines = (imports.map(i => s"import ${i}") :+ "import java.nio.file.Paths" :+ "import com.github.peterzeller.minifumo.Main")
      .distinct
      .mkString("\n")

    s"""${packageLine}
|${importLines}
|
|object ${moduleName}:
|  /** Stores the source file path relative to the Minifumo project root. */
|  val sourcePath: String = \"${escape(sanitizedRelative)}\"
|
|  /** Stores the number of declarations after index and proof erasure. */
|  val erasedDeclarationCount: Int = ${erasedDeclCount}
|
|  /** Runs the translated file by delegating to the Minifumo interpreter. */
|  def main(args: Array[String]): Unit =
|    Main.runFile(Paths.get(sourcePath)) match
|      case Right(value) =>
|        println(value)
|      case Left(errors) =>
|        Console.err.println(Main.renderTypeErrors(errors).mkString("\\n\\n"))
|        sys.exit(1)
|""".stripMargin.trim + "\n"

  /** Converts a filesystem path token into a valid Scala identifier. */
  private def sanitizeIdentifier(value: String): String =
    val normalized = value.map:
      case ch if ch.isLetterOrDigit || ch == '_' => ch
      case _ => '_'
    val withPrefix =
      if normalized.isEmpty || !normalized.head.isLetter && normalized.head != '_' then s"_${normalized}"
      else normalized
    if scalaKeywords.contains(withPrefix) then s"`${withPrefix}`" else withPrefix

  /** Converts a filesystem token into a Scala object/class style name. */
  private def sanitizeTypeName(value: String): String =
    val ident = sanitizeIdentifier(value)
    val head = ident.headOption.getOrElse('M')
    if head.isLower then ident.updated(0, head.toUpper) else ident

  /** Escapes string content for Scala string literals. */
  private def escape(value: String): String =
    value
      .replace("\\", "\\\\")
      .replace("\"", "\\\"")

  /** Lists reserved keywords that cannot be used as bare Scala identifiers. */
  private val scalaKeywords: Set[String] = Set(
    "abstract", "case", "catch", "class", "def", "do", "else", "enum", "export", "extends", "false", "final", "finally",
    "for", "forSome", "if", "given", "implicit", "import", "lazy", "match", "new", "null", "object", "override", "package",
    "private", "protected", "return", "sealed", "super", "then", "this", "throw", "trait", "true", "try", "type", "val", "var",
    "while", "with", "yield"
  )
