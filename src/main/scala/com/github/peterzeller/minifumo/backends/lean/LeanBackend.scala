package com.github.peterzeller.minifumo.backends.lean

import com.github.peterzeller.minifumo.ast.SourceRange
import com.github.peterzeller.minifumo.common.{MinifumoError, MinifumoErrorWithPath}
import com.github.peterzeller.minifumo.typing.{ProjectSymbolCache, TypeChecker, findProjectRoot}

import java.nio.file.{Files, Path}
import scala.collection.mutable

object LeanBackend:
  // Represents one generated Lean source file and its source-map metadata.
  final case class GeneratedLeanFile(path: Path, content: String, lineMap: Vector[SourceMapEntry])

  // Stores a generated-line interval that maps back to a Minifumo source declaration.
  final case class SourceMapEntry(startLine: Int, endLine: Int, sourcePath: Path, sourceRange: SourceRange)

  // Represents backend-level errors with source location information.
  final case class BackendError(message: String, source: SourceRange) extends MinifumoError

  // Captures all metadata needed for Lean checks.
  final case class CompilationResult(files: List[GeneratedLeanFile], manifest: Map[String, List[String]])

  // Compiles one Minifumo entry file or directory to Lean and runs Lean checks.
  def compileAndCheck(entry: Path, outputDir: Path): Either[List[MinifumoErrorWithPath], CompilationResult] =
    val root = findProjectRoot(entry)
    val idSupply = TypeChecker.IdSupply()
    val cache = new ProjectSymbolCache(root, idSupply)
    val entryFile = if Files.isDirectory(entry) then root.resolve("main.minifumo") else entry
    val entryImportPath = cache.fromPath(entryFile)
    val projectFiles = collectReachableFiles(entryImportPath, cache)

    projectFiles.foreach(cache.typedAst)
    val existingErrors = cache.allErrors
    if existingErrors.nonEmpty then
      Left(existingErrors)
    else
      val result = emitProject(projectFiles, cache, outputDir)
      LeanRunner.checkGeneratedFiles(result.files) match
        case Nil => Right(result)
        case leanErrors => Left(LeanErrorMapper.mapLeanErrors(leanErrors, result.files))

  // Emits Lean files for the reachable project files grouped by SCC of file imports.
  private def emitProject(projectFiles: Set[String], cache: ProjectSymbolCache, outputDir: Path): CompilationResult =
    val depGraph = buildFileDependencyGraph(projectFiles, cache)
    val groups = LeanDependencyPlanner.topologicalSccs(depGraph)
    val groupByFile = groups.zipWithIndex.flatMap((set, index) => set.map(_ -> index)).toMap

    val groupModuleName = groups.zipWithIndex.map: (group, groupIndex) =>
      val files = group.toList.sorted
      val moduleName = if files.size == 1 then LeanNameMangler.moduleNameForPath(files.head) else s"CycleGroup${groupIndex + 1}"
      groupIndex -> moduleName
    .toMap

    // Maps each source file to the Lean module generated for its SCC.
    val moduleNameByFile = groupByFile.map: (file, groupIndex) =>
      file -> groupModuleName(groupIndex)

    val groupImports = mutable.Map[Int, Set[String]]().withDefaultValue(Set.empty)
    depGraph.foreach: (dep, dependents) =>
      val depGroup = groupByFile(dep)
      dependents.foreach: dependent =>
        val dependentGroup = groupByFile(dependent)
        if depGroup != dependentGroup then
          val current = groupImports(dependentGroup)
          groupImports.update(dependentGroup, current + groupModuleName(depGroup))

    Files.createDirectories(outputDir)
    val emitted = mutable.ListBuffer[GeneratedLeanFile]()
    var manifest = Map[String, List[String]]()

    groups.zipWithIndex.foreach: (group, groupIndex) =>
      val files = group.toList.sorted
      val moduleName = groupModuleName(groupIndex)
      val outputPath = outputDir.resolve(s"${moduleName}.lean")
      val imports = groupImports(groupIndex).toList.sorted
      val generated = LeanEmitter.emitModule(moduleName, imports, files, cache, moduleNameByFile)
      Files.writeString(outputPath, generated.content)
      val generatedFile = generated.copy(path = outputPath)
      emitted += generatedFile
      files.foreach: file =>
        val old = manifest.getOrElse(file, Nil)
        manifest += file -> (old :+ outputPath.toString)

    CompilationResult(emitted.toList, manifest)

  // Collects all files reachable from the entry file via imports.
  private def collectReachableFiles(entryImportPath: String, cache: ProjectSymbolCache): Set[String] =
    val visited = mutable.Set[String]()
    val queue = mutable.Queue[String](entryImportPath)
    while queue.nonEmpty do
      val current = queue.dequeue()
      if !visited.contains(current) then
        visited += current
        val (ast, _) = cache.getAst(current)
        ast.imports.foreach: imp =>
          imp.from.foreach: importPath =>
            queue.enqueue(importPath)
    visited.toSet

  // Builds a file-level dependency graph as edges dependency -> dependent.
  private def buildFileDependencyGraph(projectFiles: Set[String], cache: ProjectSymbolCache): Map[String, Set[String]] =
    val outgoing = mutable.Map[String, Set[String]]().withDefaultValue(Set.empty)
    projectFiles.foreach(file => outgoing.update(file, Set.empty))
    projectFiles.foreach: file =>
      val (ast, _) = cache.getAst(file)
      val deps = ast.imports.flatMap(_.from).filter(projectFiles.contains).toSet
      deps.foreach: dep =>
        outgoing.update(dep, outgoing(dep) + file)
    outgoing.toMap
