package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.TopLevel.DataDecl
import com.github.peterzeller.minifumo.ast.{FunParam, FunSig, SourceRange, TopLevel}
import com.github.peterzeller.minifumo.parser.{SyntaxError, parseFile}
import com.github.peterzeller.minifumo.typing.TypeChecker.{GlobalEnv, MetaStore, TypeContext, TypeError, checkProgram}
import com.github.peterzeller.minifumo.typing.TypedAst.Expr.{Sort, UnknownType}
import com.github.peterzeller.minifumo.typing.TypedAst.{ErrorSymbol, Expr, LocalSymbol, TermSymbol}

import java.nio.file.{Path, Paths}
import scala.collection.mutable.ListBuffer
import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.common.MinifumoErrorWithPath
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.typing.GlobalSymbolDecl.{Constructor, Fun}
import com.github.peterzeller.minifumo.typing.TypedAst.TopLevel.FunDecl

import scala.annotation.tailrec


case class GlobalSymbols(
  symbols: Map[String, GlobalSymbol] = Map()
)


case class GlobalName(file: String, name: String)

enum GlobalSymbolState:
  case Init
  case ComputingSignature
  case SignatureComputed
  case ComputingBody
  case BodyComputed

enum GlobalSymbolDecl:
  case Fun(f: ast.TopLevel.FunDecl)
  case Data(d: ast.TopLevel.DataDecl)
  case Constructor(datatypeSymbol: GlobalSymbol, constructorName: String)

case class GlobalSymbolContinuationData(
  declAst: GlobalSymbolDecl,
  globalNames: NameCache & SymbolCache,
  idSupply: TypeChecker.IdSupply,
)

enum GlobalSymbolCheckState:
  case Init
  case SymbolCalculated(
    sym: TypedAst.Symbol,
    sig: TypedAst.FunSig,
    continuationContext: TypeContext,
    metaStore: MetaStore,
  )
  case BodyCalculated(
    sym: TypedAst.Symbol,
    ast: Option[TypedAst.TopLevel],
    body: Option[TypedAst.Expr],
  )


case class GlobalSymbol(
  file: String,
  name: String,
)(
  // data to continue calculating the type and typed body of this symbol
  val continuationData: Option[GlobalSymbolContinuationData],
):

  // TODO add caching for results
  var allErrors: Vector[TypeError] = Vector()
  var state: GlobalSymbolCheckState = GlobalSymbolCheckState.Init


  def toSymbol: TypedAst.Symbol =
    state match {
      case GlobalSymbolCheckState.Init =>
        val data = continuationData.get
        data.declAst match {
          case GlobalSymbolDecl.Data(d) =>
            // datatype symbol
            val globals = data.globalNames.globalEnv(file)
            val typedDt = TypeChecker.buildDataDecl(d, globals, data.idSupply)
            state = GlobalSymbolCheckState.BodyCalculated(typedDt.symbol, Some(typedDt), None)
            typedDt.symbol
          case GlobalSymbolDecl.Constructor(dt, constructorName) =>
            // force dt symbol to be computed
            dt.toSymbol
            val data = dt.state.asInstanceOf[GlobalSymbolCheckState.BodyCalculated]
            val decl = data.ast.get.asInstanceOf[TypedAst.TopLevel.DataDecl]
            decl.ctors.find(_.symbol.name == constructorName).get.symbol
          case GlobalSymbolDecl.Fun(f) =>
            val globals = data.globalNames.globalEnv(file)
            val context1 = TypeContext(globals, Map())
            val itemMetaStore = MetaStore()
            val (sig, ctx2, errors) = TypeChecker.checkFunSig(this, f.sig)(using context1, itemMetaStore, data.idSupply)
            allErrors ++= errors
            state = GlobalSymbolCheckState.SymbolCalculated(sig.symbol, sig, ctx2, itemMetaStore)
            sig.symbol
        }
      case s: GlobalSymbolCheckState.SymbolCalculated =>
        s.sym
      case s: GlobalSymbolCheckState.BodyCalculated =>
        s.sym
    }


  // calculates the typed body for the symbol
  def typedBody: Option[TypedAst.Expr] = {
    // force calculation of the symbol
    toSymbol
    state match {
      case GlobalSymbolCheckState.Init =>
        throw new IllegalStateException()
      case s: GlobalSymbolCheckState.SymbolCalculated =>
        val data = continuationData.get
        data.declAst match {
          case GlobalSymbolDecl.Fun(f) =>
            val (body, errors) = TypeChecker.check(f.body, s.sig.returnType)(using s.continuationContext, s.metaStore, data.idSupply)
            allErrors ++= errors
            state = GlobalSymbolCheckState.BodyCalculated(s.sym, Some(TypedAst.TopLevel.FunDecl(s.sig, body)(f.source)), Some(body))
            Some(body)
          case GlobalSymbolDecl.Data(d) =>
            None
          case GlobalSymbolDecl.Constructor(datatypeSymbol, constructorName) =>
            None
        }
      case s: GlobalSymbolCheckState.BodyCalculated =>
        s.body
    }
  }

object GlobalSymbol:
  def error(name: String): GlobalSymbol =
    GlobalSymbol("", name)(None)

enum SymbolSignature:
  case Def(tpe: Expr)
  case Datatype(implicitParams: List[LocalSymbol])

object GlobalSymbols:
  // Collects variable names referenced from a constructor signature expression.
  private def collectReferencedVariables(expr: ast.Expr): Set[String] =
    expr match
      case ast.Expr.Var(name) => Set(name)
      case ast.Expr.Lit(_) => Set.empty
      case ast.Expr.Call(callee, arg) => collectReferencedVariables(callee) ++ collectReferencedVariables(arg)
      case ast.Expr.CallImplicit(callee, arg) => collectReferencedVariables(callee) ++ collectReferencedVariables(arg)
      case ast.Expr.Lambda(param, body) =>
        param.tpe.map(collectReferencedVariables).getOrElse(Set.empty) ++ collectReferencedVariables(body)
      case ast.Expr.Pi(param, body) => collectReferencedVariables(param.tpe) ++ collectReferencedVariables(body)
      case ast.Expr.LetIn(_, tpe, value, body) =>
        tpe.map(collectReferencedVariables).getOrElse(Set.empty) ++ collectReferencedVariables(value) ++ collectReferencedVariables(body)
      case ast.Expr.Match(scrutinee, cases) =>
        collectReferencedVariables(scrutinee) ++ cases.flatMap(c => collectReferencedVariables(c.body)).toSet
      case ast.Expr.Hole() => Set.empty

  // Keeps only datatype implicit parameters that are needed by one constructor signature.
  private def usedCtorImplicitParams(implicitParams: List[ast.FunParam], ctor: ast.CtorDecl, ctorReturnType: ast.Expr): List[ast.FunParam] =
    val referencedNames = (ctor.fields.map(_.tpe) :+ ctorReturnType).flatMap(collectReferencedVariables).toSet
    implicitParams.filter(param => referencedNames.contains(param.name))

  // build a map of global names in a program file
  def buildGlobalNames(file: String, prog: ast.ProgramFile, onlyExported: Boolean): Map[String, GlobalName] =
    prog.items.flatMap(topLevelToGlobalNames(file, onlyExported)).toMap

  private def topLevelToGlobalNames(file: String, onlyExported: Boolean)(t: ast.TopLevel): Iterable[(String, GlobalName)] =
    t match
      case ast.TopLevel.DataDecl(name, _, ctors, exported) if exported || !onlyExported =>
        val dataTypeName = List(name -> GlobalName(file, name))
        val constructorNames = ctors.map(ctor => ctor.name -> GlobalName(file, ctor.name))
        dataTypeName ++ constructorNames
      case ast.TopLevel.FunDecl(sig, _, exported) if exported || !onlyExported =>
        List(sig.name -> GlobalName(file, sig.name))
      case _ =>
        List()

  def buildGlobalSymbols(file: String, prog: ast.ProgramFile, symbolCache: NameCache&SymbolCache, onlyExported: Boolean, ids: TypeChecker.IdSupply): (Map[String, GlobalSymbol], List[TypeError]) =
    val (imports, errors1) = resolveImports(prog, symbolCache)
    val ownNames = buildGlobalNames(file, prog, false)
    val standardLibraryNames = buildGlobalNames("standard.minifumo", Standard.standardProgram, true)
    val errors = ListBuffer[TypeError](errors1*)
    var res = Map[String, GlobalSymbol]()

    if !onlyExported then
      // also add imported ones
      for i <- prog.imports do
        i.from match
          case Some(path) =>
            val symbolMap = symbolCache.globalSymbols(path)
            symbolMap.get(i.name) match
              case Some(is) =>
                if res.contains(i.name) then
                  errors.addOne(TypeError(s"Name ${i.name} is already defined", i.source))
                res += i.name -> is
              case None =>
                errors.addOne(TypeError(s"Name ${i.name} not found in ${path}", i.source))
          case None =>
            errors.addOne(TypeError(s"Import without from clause not supported", i.source))

    for symbols <- prog.items.map(topLevelToGlobalSymbols(file, onlyExported, symbolCache, ids)) do
      for (name, source, sym) <- symbols do
        if res.contains(name) then
          errors.addOne(TypeError(s"Name ${name} is already defined", source))
        else
          res += name -> sym

    (res, errors.toList)


  private def topLevelToGlobalSymbols(file: String, onlyExported: Boolean, globalNames: NameCache & SymbolCache, idSupply: TypeChecker.IdSupply)(t: ast.TopLevel): Iterable[(String, SourceRange, GlobalSymbol)] =
    t match
      case d@ast.TopLevel.DataDecl(name, implicitParams, ctors, exported) if exported || !onlyExported =>
        val errors = ListBuffer[TypeError]()
        val symbols = ListBuffer[(String, SourceRange, GlobalSymbol)]()
        // add the type symbol
        val dtSymbol = GlobalSymbol(file, name)(Some(GlobalSymbolContinuationData(GlobalSymbolDecl.Data(d), globalNames, idSupply)))
        symbols.addOne((name, t.source, dtSymbol))


        // add symbols for the constructors
        for ctor <- ctors do
          symbols.addOne((ctor.name, ctor.source, GlobalSymbol(file, ctor.name)(Some(GlobalSymbolContinuationData(Constructor(dtSymbol, ctor.name), globalNames, idSupply)))))
        symbols
      case f@ast.TopLevel.FunDecl(sig, _, exported) if exported || !onlyExported =>
        symbolsForFunDef(f, file, globalNames, idSupply)
      case _ =>
        List()

  private def symbolsForFunDef(decl: ast.TopLevel.FunDecl, file: String, globalNames: NameCache & SymbolCache, idSupply: TypeChecker.IdSupply): Iterable[(String, SourceRange, GlobalSymbol)] =
    val sig = decl.sig
    val symbol = GlobalSymbol(file, sig.name)(Some(GlobalSymbolContinuationData(Fun(decl), globalNames, idSupply)))
    List((sig.name, sig.source, symbol))

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


trait SymbolCache:
  def globalSymbols(path: String): Map[String, GlobalSymbol]
  def globalEnv(path: String): GlobalEnv

// find the folder that contains minifumo.yml
@tailrec
def findProjectRoot(path: Path): Path =
  if path.toFile.isDirectory then
    if path.resolve("minifumo.yml").toFile.exists() then
      return path
  val parent = path.getParent
  if parent == path then
    throw new RuntimeException("could not minifumo project root")
  findProjectRoot(path.getParent)



class ProjectSymbolCache(projectRoot: Path, val ids: TypeChecker.IdSupply) extends NameCache with SymbolCache:
  private var astCache: Map[String, (ast.ProgramFile, List[SyntaxError])] = Map()
  private var namesCache: Map[String, Map[String, GlobalName]] = Map()
  private var symbolCache: Map[String, Map[String, GlobalSymbol]] = Map()
  private var globalEnvCache: Map[String, GlobalEnv] = Map()
  private var typedAstCache: Map[String, (TypedAst.Program, List[TypeError])] = Map()

  def toPath(importPath: String): Path =
    if importPath.endsWith("standard.minifumo") then
      return Paths.get("standard.minifumo")
    projectRoot.resolve(importPath)

  def fromPath(p: Path): String =
    projectRoot.relativize(p).normalize().toString

  def makeRelative(path: Path): String =
    fromPath(path)


  def getAst(path: String): (ast.ProgramFile, List[SyntaxError]) =
    astCache.get(path) match
      case Some(a) => a
      case None =>
        val (ast, syntaxErrors) =
          if path == "standard.minifumo" then
            parseInput(Standard.loadStandardSource())
          else
            parseFile(toPath(path))
        val r = (ast, syntaxErrors)
        astCache += path -> r
        r


  def globalNames(path: String): Map[String, GlobalName] =
    namesCache.get(path) match
      case Some(m) => m
      case None =>
        val r = GlobalSymbols.buildGlobalNames(path, getAst(path)._1, true)
        namesCache += path -> r
        r

  def globalSymbols(path: String): Map[String, GlobalSymbol] =
    symbolCache.get(path) match
      case Some(m) => m
      case None =>
        val (r,_) = GlobalSymbols.buildGlobalSymbols(path, getAst(path)._1, this, true, ids)
        symbolCache += path -> r
        r

  def globalEnv(path: String): GlobalEnv =
    globalEnvCache.get(path) match
      case Some(m) => m
      case None =>
        val (r, _) = GlobalSymbols.buildGlobalSymbols(path, getAst(path)._1, this, false, ids)
        // TODO maybe we need to include imports here
        val e = GlobalEnv(r)
        globalEnvCache += path -> e
        e


  def allPaths: Set[String] = astCache.keySet + "standard.minifumo"

  def typedAst(path: String): (TypedAst.Program, List[TypeError]) =
    typedAstCache.get(path) match
      case Some(m) => m
      case None =>
        val (ast, _) = getAst(path)
        val r = checkProgram(path, ast, this, path != "standard.minifumo", ids)
        typedAstCache += path -> r
        r

  def allErrors: List[MinifumoErrorWithPath] =
    val syntaxErrors = astCache.flatMap(p =>  p._2._2.map(MinifumoErrorWithPath(toPath(p._1), _)))
    val typeErrors = astCache.keysIterator.flatMap(p => typedAst(p)._2.map(MinifumoErrorWithPath(toPath(p), _)))
    (syntaxErrors ++ typeErrors).toList
