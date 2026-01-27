package com.github.peterzeller.minifumo

import com.github.peterzeller.minifumo.ast.{AstTransform, ProgramFile, SourcePos, SourceRange}
import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.interpreter.Interpreter
import com.github.peterzeller.minifumo.parser.{SyntaxError, parseInput}
import com.github.peterzeller.minifumo.typing.{TypeChecker, TypedAst}
import com.github.peterzeller.minifumo.typing.TypeChecker.TypeError

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.collection.mutable
import scala.util.{Try, Using}

object Main:
  // Stores shared caches for parsing and export resolution across multiple files.
  final case class GlobalInfo(
      parseCache: mutable.Map[Path, (ProgramFile, List[SyntaxError])],
      exportCache: mutable.Map[Path, TypeChecker.ExportEnv],
      resolvedExportCache: mutable.Map[Path, (TypeChecker.ExportEnv, List[TypeError])],
      typedProgramCache: mutable.Map[Path, (TypedAst.Program, List[TypeError])],
      resolving: mutable.Set[Path]
    )

  // Creates a new cache container for resolving imports within a project.
  private def newGlobalInfo(): GlobalInfo =
    GlobalInfo(mutable.Map.empty, mutable.Map.empty, mutable.Map.empty, mutable.Map.empty, mutable.Set.empty)

  // Entry point for the CLI.
  def main(args: Array[String]): Unit =
    args.toList match
      case "run" :: filename :: Nil =>
        val path = Paths.get(filename)
        if Files.isDirectory(path) then
          Console.err.println(s"minifumo run expects a file, got directory: $filename")
          System.exit(2)
        runFile(path) match
          case Right(value) => println(value)
          case Left(messages) =>
            messages.foreach(Console.err.println)
            System.exit(1)
      case "check" :: filename :: Nil =>
        val path = Paths.get(filename)
        val failures =
          if Files.isDirectory(path) then
            checkDirectory(path)
          else
            checkFile(path)
        if failures.nonEmpty then
          failures.foreach(Console.err.println)
          System.exit(1)
      case _ =>
        println(s"Unknown command ${args.mkString(" ")}")
        printUsage()
        System.exit(2)

  // Prints command-line usage information.
  private def printUsage(): Unit =
    Console.err.println(
      """Usage:
        |  minifumo run <filename>
        |  minifumo check <filename-or-directory>""".stripMargin
    )

  // Runs a program file and returns either error messages or the evaluated value.
  def runFile(path: Path): Either[List[String], Interpreter.Value] =
    val (program, syntaxErrors) = parseProgram(path)
    if syntaxErrors.nonEmpty then
      Left(renderSyntaxErrors(path, syntaxErrors))
    else
      val info = newGlobalInfo()
      val (importedExports, importErrors) = resolveImports(path, program, info)
      val (typedProgram, typeErrors) = TypeChecker.checkProgram(program, importedExports)
      val rootOpt = findProjectRoot(path.toAbsolutePath.getParent)
      val (importedItems, importItemErrors) = collectImportedTypedItems(program, rootOpt, info)
      val allErrors = importErrors ++ importItemErrors ++ typeErrors
      if allErrors.nonEmpty then
        Left(renderTypeErrors(path, allErrors))
      else
        val combinedProgram =
          TypedAst.Program(Standard.typedProgram.items ++ importedItems ++ typedProgram.items)(typedProgram.source)
        Right(Interpreter.evalProg(combinedProgram, "main"))

  // Checks a directory of examples, reusing cached parse/import info across files.
  def checkDirectory(path: Path): List[String] =
    if !Files.exists(path) then
      List(s"Directory not found: ${path.toString}")
    else
      val info = newGlobalInfo()
      Try {
        Using.resource(Files.list(path)) { stream =>
          stream.iterator().asScala.toList
            .filter(Files.isRegularFile(_))
            .filter(_.toString.endsWith(".minifumo"))
            .sortBy(_.toString)
            .flatMap(checkFile(_, info))
        }
      }.getOrElse(List(s"Failed reading directory: ${path.toString}"))

  // Checks a single file for syntax and type errors, including imports.
  def checkFile(path: Path): List[String] =
    checkFile(path, newGlobalInfo())

  // Checks a file using shared caches to avoid reparsing across a project.
  private def checkFile(path: Path, info: GlobalInfo): List[String] =
    val (program, syntaxErrors) = loadProgram(path, info)
    if syntaxErrors.nonEmpty then
      renderSyntaxErrors(path, syntaxErrors)
    else
      val (importedExports, importErrors) = resolveImports(path, program, info)
      val (_, errors) = TypeChecker.checkProgram(program, importedExports)
      val allErrors = importErrors ++ errors
      if allErrors.isEmpty then
        Nil
      else
        allErrors.map(err => s"${path.toString}:${renderSourceRange(err.source)}: ${err.message}")

  // Represents an empty program when parsing fails.
  private val emptyProgramFile = ProgramFile(List(), List())(SourceRange(SourcePos(0, 0), SourcePos(0, 0)))

  // Parses a program file into an AST and syntax errors.
  private def parseProgram(path: Path): (ProgramFile, List[SyntaxError]) =
    if !Files.exists(path) then
      (emptyProgramFile, List(SyntaxError(SourcePos(0, 0), s"File not found: ${path.toString}")))
    else
      val content = Try {
        Using.resource(scala.io.Source.fromFile(path.toFile))(_.mkString)
      }
      content match
        case scala.util.Failure(exception) =>
          (emptyProgramFile, List(SyntaxError(SourcePos(0, 0), s"Failed reading file ${path.toString}: ${exception.getMessage}")) )
        case scala.util.Success(input) =>
          val (cst, syntaxErrors) = parseInput(input)
          val ast = AstTransform.program(cst)
          (ast, syntaxErrors)

  // Resolves imports for a program using the file path and a shared cache.
  private def resolveImports(path: Path, program: ProgramFile, info: GlobalInfo): (TypeChecker.ExportEnv, List[TypeError]) =
    if program.imports.isEmpty then
      (TypeChecker.emptyExportEnv, Nil)
    else
      findProjectRoot(path.toAbsolutePath.getParent) match
        case None =>
          val errors = program.imports.map { stmt =>
            TypeError("Project root not found (minifumo.yml).", stmt.source)
          }
          (TypeChecker.emptyExportEnv, errors)
        case Some(root) =>
          resolveImportsForProgram(program, root, info)

  // Represents the kinds of importable symbols without relying on string literals.
  enum ImportKind:
    case Function, Data, TypeClass

    // Returns a human-readable label for the kind.
    def label: String =
      this match
        case Function => "function"
        case Data => "data"
        case TypeClass => "typeclass"

  // Resolves import statements in a program, returning merged exports and errors.
  private def resolveImportsForProgram(
      program: ProgramFile,
      root: Path,
      info: GlobalInfo
    ): (TypeChecker.ExportEnv, List[TypeError]) =
    val errors = scala.collection.mutable.ListBuffer.empty[TypeError]
    var imports = TypeChecker.emptyExportEnv
    program.imports.foreach { stmt =>
      val (updatedImports, importErrors) =
        resolveImportStatement(stmt, root, info, imports)
      imports = updatedImports
      errors ++= importErrors
    }
    val memberIndex = imports.typeClasses.values
      .flatMap(tc => tc.members.map(_.name).distinct.map(_ -> tc))
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).toList)
      .toMap
    (imports.copy(memberIndex = memberIndex), errors.toList)

  // Resolves a single import statement with cached exports for local files.
  private def resolveImportStatement(
      stmt: com.github.peterzeller.minifumo.ast.ImportStatement,
      root: Path,
      info: GlobalInfo,
      currentImports: TypeChecker.ExportEnv
    ): (TypeChecker.ExportEnv, List[TypeError]) =
    if stmt.inRepo.nonEmpty then
      (currentImports, List(TypeError("External imports are not supported yet.", stmt.source)))
    else
      stmt.from match
        case None =>
          (currentImports, List(TypeError("Import is missing a source path.", stmt.source)))
        case Some(pathText) =>
          val importPath = resolveImportPath(root, pathText)
          if !Files.exists(importPath) then
            (currentImports, List(TypeError(s"Imported file not found: ${importPath.toString}.", stmt.source)))
          else
            val (exportEnv, exportErrors) =
              loadResolvedExportEnv(importPath, root, info)
            val wrappedErrors = exportErrors.map { err =>
              TypeError(s"Failed to import ${importPath.toString}: ${err.message}", stmt.source)
            }
            val (updatedImports, symbolErrors) = addImportedSymbol(stmt, exportEnv, currentImports)
            (updatedImports, wrappedErrors ++ symbolErrors)

  // Loads and caches syntax parsing results for a file path.
  private def loadProgram(path: Path, info: GlobalInfo): (ProgramFile, List[SyntaxError]) =
    info.parseCache.getOrElseUpdate(path, parseProgram(path))

  // Collects export symbols without resolving imports, caching the result.
  private def loadStage1Exports(path: Path, info: GlobalInfo): (TypeChecker.ExportEnv, List[TypeError]) =
    val (program, syntaxErrors) = loadProgram(path, info)
    if syntaxErrors.nonEmpty then
      (TypeChecker.emptyExportEnv, syntaxErrors.map(err => TypeError(err.message, err.source)))
    else
      val exports =
        info.exportCache.getOrElseUpdate(
          path,
          TypeChecker.extractExports(
            program,
            TypeChecker.withStandardExports(TypeChecker.emptyExportEnv),
            includeNonExported = false,
            shadowedTypes = Standard.standardExports.types.keySet,
            shadowedCtors = Standard.standardExports.ctors.keySet,
            shadowedTypeClasses = Standard.standardExports.typeClasses.keySet
          )._1
        )
      (exports, Nil)

  // Loads and caches exports with imports resolved, using stage 1 exports for cycles.
  private def loadResolvedExportEnv(
      path: Path,
      root: Path,
      info: GlobalInfo
    ): (TypeChecker.ExportEnv, List[TypeError]) =
    info.resolvedExportCache.getOrElseUpdate(
      path, {
        if info.resolving.contains(path) then
          loadStage1Exports(path, info)
        else
          info.resolving.add(path)
          val errors = scala.collection.mutable.ListBuffer.empty[TypeError]
          val (_, stage1Errors) = loadStage1Exports(path, info)
          errors ++= stage1Errors
          val (program, _) = loadProgram(path, info)
          val exports =
            if stage1Errors.nonEmpty then
              TypeChecker.emptyExportEnv
            else
              val (importedExports, importErrors) =
                resolveImportsForProgram(program, root, info)
              errors ++= importErrors
              val (resolvedExports, exportErrors) =
                TypeChecker.extractExports(
                  program,
                  TypeChecker.withStandardExports(importedExports),
                  includeNonExported = false,
                  shadowedTypes = Standard.standardExports.types.keySet,
                  shadowedCtors = Standard.standardExports.ctors.keySet,
                  shadowedTypeClasses = Standard.standardExports.typeClasses.keySet
                )
              errors ++= exportErrors
              resolvedExports
          info.resolving.remove(path)
          (exports, errors.toList)
      }
    )

  // Loads and caches a fully typed program for runtime evaluation.
  private def loadTypedProgram(path: Path, info: GlobalInfo): (TypedAst.Program, List[TypeError]) =
    info.typedProgramCache.getOrElseUpdate(
      path, {
        val (program, syntaxErrors) = loadProgram(path, info)
        if syntaxErrors.nonEmpty then
          val errors = syntaxErrors.map(err => TypeError(err.message, err.source))
          (TypedAst.Program(Nil)(program.source), errors)
        else
          val (importedExports, importErrors) = resolveImports(path, program, info)
          val (typedProgram, typeErrors) = TypeChecker.checkProgram(program, importedExports)
          (typedProgram, importErrors ++ typeErrors)
      }
    )

  // Collects typed items for imported symbols so they can be evaluated at runtime.
  private def collectImportedTypedItems(
      program: ProgramFile,
      rootOpt: Option[Path],
      info: GlobalInfo
    ): (List[TypedAst.TopLevel], List[TypeError]) =
    val errors = scala.collection.mutable.ListBuffer.empty[TypeError]
    val collected = scala.collection.mutable.ListBuffer.empty[TypedAst.TopLevel]
    val seen = scala.collection.mutable.Set.empty[(ImportKind, String)]
    program.imports.foreach { stmt =>
      if stmt.inRepo.nonEmpty then
        errors += TypeError("External imports are not supported yet.", stmt.source)
      else
        rootOpt match
          case None =>
            errors += TypeError("Project root not found (minifumo.yml).", stmt.source)
          case Some(root) =>
            stmt.from match
              case None =>
                errors += TypeError("Import is missing a source path.", stmt.source)
              case Some(pathText) =>
                val resolvedPath = resolveImportPath(root, pathText)
                if !Files.exists(resolvedPath) then
                  errors += TypeError(s"Imported file not found: ${resolvedPath.toString}.", stmt.source)
                else
                  val (importProgram, syntaxErrors) = loadProgram(resolvedPath, info)
                  errors ++= syntaxErrors.map(err => TypeError(err.message, err.source))
                  val exportKinds = exportedKinds(importProgram, stmt.name)
                  if exportKinds.isEmpty then
                    errors += TypeError(s"Symbol ${stmt.name} is not exported from the imported file.", stmt.source)
                  else if exportKinds.size > 1 then
                    errors += TypeError(
                      s"Symbol ${stmt.name} is exported as multiple kinds (${exportKinds.map(_.label).mkString(", ")}).",
                      stmt.source
                    )
                  else
                    val kind = exportKinds.head
                    if !seen.contains((kind, stmt.name)) then
                      val (typedProgram, typeErrors) = loadTypedProgram(resolvedPath, info)
                      errors ++= typeErrors
                      findTypedItem(typedProgram, kind, stmt.name).foreach { item =>
                        collected += item
                        seen += ((kind, stmt.name))
                      }
    }
    (collected.toList, errors.toList)

  // Finds exported kinds for a given symbol name in an AST program.
  private def exportedKinds(program: ProgramFile, name: String): List[ImportKind] =
    program.items.flatMap {
      case ast.TopLevel.DataDecl(itemName, _, _, exported) if exported && itemName == name => List(ImportKind.Data)
      case ast.TopLevel.FunDecl(itemName, _, _, _, _, _, exported) if exported && itemName == name => List(ImportKind.Function)
      case ast.TopLevel.TypeClassDecl(itemName, _, _, exported) if exported && itemName == name => List(ImportKind.TypeClass)
      case _ => Nil
    }

  // Finds a typed item matching the imported name and kind.
  private def findTypedItem(
      typedProgram: TypedAst.Program,
      kind: ImportKind,
      name: String
    ): Option[TypedAst.TopLevel] =
    typedProgram.items.collectFirst {
      case item: TypedAst.TopLevel.DataDecl if kind == ImportKind.Data && item.name == name => item
      case item: TypedAst.TopLevel.FunDecl if kind == ImportKind.Function && item.symbol.name == name => item
      case item: TypedAst.TopLevel.TypeClassDecl if kind == ImportKind.TypeClass && item.name == name => item
    }

  // Adds an imported symbol into the current import environment, enforcing duplicate checks.
  private def addImportedSymbol(
      stmt: com.github.peterzeller.minifumo.ast.ImportStatement,
      exportEnv: TypeChecker.ExportEnv,
      currentImports: TypeChecker.ExportEnv
    ): (TypeChecker.ExportEnv, List[TypeError]) =
    val errors = scala.collection.mutable.ListBuffer.empty[TypeError]
    val matches =
      List(
        exportEnv.functions.get(stmt.name).map(_ => ImportKind.Function),
        exportEnv.types.get(stmt.name).map(_ => ImportKind.Data),
        exportEnv.typeClasses.get(stmt.name).map(_ => ImportKind.TypeClass)
      ).flatten
    if matches.isEmpty then
      errors += TypeError(s"Symbol ${stmt.name} is not exported from the imported file.", stmt.source)
      (currentImports, errors.toList)
    else if matches.size > 1 then
      errors += TypeError(s"Symbol ${stmt.name} is exported as multiple kinds (${matches.map(_.label).mkString(", ")}).", stmt.source)
      (currentImports, errors.toList)
    else
      matches.head match
        case ImportKind.Function =>
          if currentImports.functions.contains(stmt.name) then
            errors += TypeError(s"Duplicate function: ${stmt.name}", stmt.source)
            (currentImports, errors.toList)
          else
            val fun = exportEnv.functions(stmt.name)
            (currentImports.copy(functions = currentImports.functions + (stmt.name -> fun)), errors.toList)
        case ImportKind.Data =>
          if currentImports.types.contains(stmt.name) then
            errors += TypeError(s"Duplicate data type: ${stmt.name}", stmt.source)
            (currentImports, errors.toList)
          else
            val dataType = exportEnv.types(stmt.name)
            val ctorErrors = dataType.ctors.collect {
              case ctor if currentImports.ctors.contains(ctor.name) =>
                TypeError(s"Duplicate constructor: ${ctor.name}", stmt.source)
            }
            errors ++= ctorErrors
            val mergedCtors = currentImports.ctors ++ dataType.ctors.filterNot(ctor => currentImports.ctors.contains(ctor.name)).map(
              ctor => ctor.name -> ctor
            )
            (
              currentImports.copy(types = currentImports.types + (stmt.name -> dataType), ctors = mergedCtors),
              errors.toList
            )
        case ImportKind.TypeClass =>
          if currentImports.typeClasses.contains(stmt.name) then
            errors += TypeError(s"Duplicate typeclass: ${stmt.name}", stmt.source)
            (currentImports, errors.toList)
          else
            val tc = exportEnv.typeClasses(stmt.name)
            (currentImports.copy(typeClasses = currentImports.typeClasses + (stmt.name -> tc)), errors.toList)

  // Resolves an import path relative to the project root, enforcing the .minifumo extension.
  private def resolveImportPath(root: Path, pathText: String): Path =
    val rawPath = Paths.get(pathText)
    val withExtension =
      if rawPath.toString.endsWith(".minifumo") then rawPath else Paths.get(s"${rawPath.toString}.minifumo")
    root.resolve(withExtension).normalize()

  // Finds the project root by walking up to locate minifumo.yml.
  private def findProjectRoot(start: Path): Option[Path] =
    Iterator.iterate(start)(_.getParent).takeWhile(_ != null).find { candidate =>
      Files.exists(candidate.resolve("minifumo.yml"))
    }

  // Renders a source range for error reporting.
  private def renderSourceRange(range: com.github.peterzeller.minifumo.ast.SourceRange): String =
    val start = range.start
    val end = range.end
    if start == end then
      s"${start.line}:${start.column}"
    else
      s"${start.line}:${start.column}-${end.line}:${end.column}"

  // Formats syntax errors with the given file path.
  private def renderSyntaxErrors(path: Path, errors: List[SyntaxError]): List[String] =
    errors.map { err =>
      s"${path.toString}:${err.pos.line}:${err.pos.column}: ${err.message}"
    }

  // Formats type errors with the given file path.
  private def renderTypeErrors(path: Path, errors: List[TypeError]): List[String] =
    errors.map { err =>
      s"${path.toString}:${renderSourceRange(err.source)}: ${err.message}"
    }
