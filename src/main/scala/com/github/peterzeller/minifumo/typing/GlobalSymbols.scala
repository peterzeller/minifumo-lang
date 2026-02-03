package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.{AstTransform, SourceRange}
import com.github.peterzeller.minifumo.parser.{SyntaxError, parseFile, parseInput}
import com.github.peterzeller.minifumo.typing.TypeChecker.TypeError
import com.github.peterzeller.minifumo.typing.TypedAst.Expr

import java.nio.file.Path
import scala.collection.mutable.ListBuffer


case class GlobalSymbols(
  symbols: Map[String, GlobalSymbol] = Map()
)


case class GlobalName(file: Path, name: String)

case class GlobalSymbol(file: Path, name: String, symbolSignature: SymbolSignature)

enum SymbolSignature:
  case Def(tpe: Expr)
  case Datatype(implicitParams: List[Expr])

object GlobalSymbols:
  // build a map of global names in a program file
  def buildGlobalNames(file: Path, prog: ast.ProgramFile, onlyExported: Boolean): Map[String, GlobalName] =
    prog.items.flatMap(topLevelToGlobalNames(file, onlyExported)).toMap

  private def topLevelToGlobalNames(file: Path, onlyExported: Boolean)(t: ast.TopLevel): Iterable[(String, GlobalName)] =
    t match
      case ast.TopLevel.DataDecl(name, _, _, exported) if exported || !onlyExported =>
        // TODO add also the constructors, eliminator, accessors
        List(name -> GlobalName(file, name))
      case ast.TopLevel.FunDecl(sig, _, exported) if exported || !onlyExported =>
        List(sig.name -> GlobalName(file, sig.name))
      case _ =>
        List()

  def buildGlobalSymbols(file: Path, prog: ast.ProgramFile, symbolCache: NameCache, onlyExported: Boolean): (Map[String, GlobalSymbol], List[TypeError]) =
    val (imports, errors1) = resolveImports(prog, symbolCache)
    val ownNames = buildGlobalNames(file, prog, false)
    val preEnv = PreEnv(names = imports ++ ownNames)
    val errors = ListBuffer[TypeError](errors1*)
    var res = Map[String, GlobalSymbol]()
    for (symbols, es) <- prog.items.map(topLevelToGlobalSymbols(file, onlyExported, preEnv)) do
      errors.addAll(es)
      for (name, source, sym) <- symbols do
        if res.contains(name) then
          errors.addOne(TypeError(s"Name ${name} is already defined", source))
        else
          res += name -> sym

    (res, errors.toList)

  // environment for type checking signatures.
  // Has only limited information compared to real type checking
  case class PreEnv(names: Map[String, GlobalName])

  // performs basic checks (like name resolution) on an expression appearing in a signature.
  // Only basic language constructs are allowed in signatures, like Pi expressions, names, constants, literals, and function applications.
  // This means, that we don't need to do complex type checking in function signatures.
  // We also don't check function application, only translate the terms to the typed AST.
  def checkSignatureExpr(e: ast.Expr, env: PreEnv): (Expr, List[TypeError]) =
    ???

  private def topLevelToGlobalSymbols(file: Path, onlyExported: Boolean, env: PreEnv)(t: ast.TopLevel): (Iterable[(String, SourceRange, GlobalSymbol)], Iterable[TypeError]) =
    t match
      case ast.TopLevel.DataDecl(name, _, _, exported) if exported || !onlyExported =>
        // TODO add symbols for datatypes
        ???
      case ast.TopLevel.FunDecl(sig, _, exported) if exported || !onlyExported =>
        // TODO add symbol for function
        ???
      case _ =>
        (List(), List())

  def resolveImports(prog: ast.ProgramFile, symbolCache: NameCache): (Map[String, GlobalName], List[TypeError]) =
    val errors = ListBuffer[TypeError]()
    var res = Map[String, GlobalName]()

    for i <- prog.imports do
      i.from match
        case None =>
        case Some(f) =>
          symbolCache.globalNames(f).get(i.name) match
            case Some(importedSymbol) =>
              if res.contains(i.name) then
                errors.addOne(TypeError(s"Symbol ${i.name} is already defined", i.source))
              else
                res += i.name -> importedSymbol
            case None =>
              errors.addOne(TypeError(s"Could not find ${i.name}", i.source))
    (res, errors.toList)



trait NameCache:
  def globalNames(path: String): Map[String, GlobalName]

class ProjectSymbolCache(projectRoot: Path) extends NameCache:
  private var astCache: Map[Path, (ast.ProgramFile, List[SyntaxError])] = Map()
  private var namesCache: Map[Path, Map[String, GlobalName]] = Map()
  private var symbolCache: Map[Path, GlobalSymbols] = Map()

  def toPath(importPath: String): Path =
    projectRoot.resolve(importPath)

  def getAst(path: Path): (ast.ProgramFile, List[SyntaxError]) =
    astCache.get(path) match
      case Some(a) => a
      case None =>
        val (cst, syntaxErrors) = parseFile(path)
        val ast = AstTransform.program(cst)
        val r = (ast, syntaxErrors)
        astCache += path -> r
        r

  def globalNames(importPath: String): Map[String, GlobalName] =
    globalNames(toPath(importPath))

  def globalNames(path: Path): Map[String, GlobalName] =
    namesCache.get(path) match
      case Some(m) => m
      case None =>
        val r = GlobalSymbols.buildGlobalNames(path, getAst(path)._1, true)
        namesCache += path -> r
        r