package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.AstTransform
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
  def buildGlobalNames(file: Path, prog: ast.ProgramFile): Map[String, GlobalName] =
    prog.items.flatMap(topLevelToGlobalNames(file)).toMap

  private def topLevelToGlobalNames(file: Path)(t: ast.TopLevel): Iterable[(String, GlobalName)] =
    t match
      case ast.TopLevel.DataDecl(name, _, _, true) =>
        List(name -> GlobalName(file, name))
      case ast.TopLevel.DataDecl(_, _, _, false) =>
        List()
      case ast.TopLevel.FunDecl(sig, _, true) =>
        List(sig.name -> GlobalName(file, sig.name))
      case ast.TopLevel.FunDecl(_, _, false) =>
        List()

  def buildGlobalSymbols(file: Path, prog: ast.ProgramFile, symbolCache: NameCache): Map[String, GlobalSymbol] =
    var errors = ListBuffer[TypeError]()
    for i <- prog.imports do
      i.from match
        case None =>
        case Some(f) =>
          symbolCache.globalNames(f).get(i.name) match
            case Some(importedSymbol) => ???
            case None => ???

    ???



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
        val r = GlobalSymbols.buildGlobalNames(path, getAst(path)._1)
        namesCache += path -> r
        r