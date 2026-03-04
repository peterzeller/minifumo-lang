package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.TopLevel.DataDecl
import com.github.peterzeller.minifumo.ast.{SourceRange, TopLevel}
import com.github.peterzeller.minifumo.parser.{SyntaxError, parseFile}
import com.github.peterzeller.minifumo.typing.TypeChecker.{GlobalEnv, MetaStore, TypeContext, TypeError, checkProgram}
import com.github.peterzeller.minifumo.typing.TypedAst.Expr.Pi
import com.github.peterzeller.minifumo.typing.TypedAst.Expr

import java.nio.file.{Path, Paths}
import scala.collection.mutable.ListBuffer
import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.common.MinifumoErrorWithPath
import com.github.peterzeller.minifumo.parser.parseInput
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

case class FunctionSymbolContinuationData(
  declAst: ast.TopLevel.FunDecl,
  globalNames: NameCache & SymbolCache,
  idSupply: TypeChecker.IdSupply,
)

case class DatatypeSymbolContinuationData(
  declAst: ast.TopLevel.DataDecl,
  globalNames: NameCache & SymbolCache,
  idSupply: TypeChecker.IdSupply,
)

case class CtorSymbolContinuationData(
  dtSymbol: DatatypeSymbol,
  globalNames: NameCache & SymbolCache,
  idSupply: TypeChecker.IdSupply,
)

enum FunctionSymbolCheckState:
  case Init
  case SymbolCalculated(
    sig: TypedAst.FunSig,
    continuationContext: TypeContext,
    metaStore: MetaStore,
  )
  case BodyCalculated(
    fun: TypedAst.TopLevel.FunDecl,
  )

sealed trait Symbol:
  def name: String

  def tpe: Expr

sealed trait TermSymbol extends Symbol


sealed trait GlobalSymbol extends Symbol:
  def file: String

final case class LocalSymbol(name: String, tpe: Expr, id: Int) extends TermSymbol

final case class BuiltinValueSymbol(name: String, tpe: Expr) extends TermSymbol // TODO do we need this?

final case class ErrorSymbol(name: String, tpe: Expr) extends Symbol

object DatatypeSymbol:
    case class TypeCalculated(params: List[LocalSymbol], tpe: Expr)


final case class DatatypeSymbol(file: String, name: String)(val continuationData: Option[DatatypeSymbolContinuationData]) extends GlobalSymbol:

  private var allErrors: List[TypeError] = List()
  private var typeCalculated: Option[DatatypeSymbol.TypeCalculated] = None
  None

  val ctorSymbols: List[CtorSymbol] = continuationData match {
    case Some(data) =>
      for c <- data.declAst.ctors yield
        CtorSymbol(this, c.name)
    case None => List()
  }

  /** Calculate the type of a datatype constructor.
   * For example for datatype List[T: Type], the constructor would be Pi(T: Type, Sort, explicit = false) */
  def tpe: TypedAst.Expr =
    if typeCalculated.isDefined then
      return typeCalculated.get.tpe

    val data = continuationData match {
      case None => return TypedAst.Expr.UnknownType()(SourceRange.empty)
      case Some(d) => d
    }
    val globals = data.globalNames.globalEnv(file)
    var ctx = TypeContext(globals, Map())
    val metas = MetaStore()
    val implicitParams: List[LocalSymbol] =
      for p <- data.declAst.implicitParams yield
        val (t, errors) = TypeChecker.checkAndElaborate(p.tpe, TypedAst.Expr.Sort()(SourceRange.empty))(using ctx, metas, data.idSupply)
        allErrors ++= errors
        val pSym = LocalSymbol(p.name, t, data.idSupply.freshLocalId())
        ctx = ctx.withLocal(pSym)
        pSym

    val t = buildPiType(implicitParams, List(), Expr.Sort()(SourceRange.empty))
    typeCalculated = Some(DatatypeSymbol.TypeCalculated(implicitParams, t))
    t


  /** Calculates the typed toplevel declaration
   **/
  def typed: TypedAst.TopLevel.DataDecl = {
    tpe
    val data = continuationData.get
    val typeInfo = typeCalculated.get
    val globals = data.globalNames.globalEnv(file)
    var ctx = TypeContext(globals, Map())
    val metas = MetaStore()
    val ctors: List[TypedAst.CtorDecl] =
      for (ctor, ctorSym) <- data.declAst.ctors.zip(ctorSymbols) yield
        val fields: List[LocalSymbol] =
          for p <- ctor.fields yield
            val (t, errors) = TypeChecker.checkAndElaborate(p.tpe, TypedAst.Expr.Sort()(SourceRange.empty))(using ctx, metas, data.idSupply)
            allErrors ++= errors
            val pSym = LocalSymbol(p.name, t, data.idSupply.freshLocalId())
            ctx = ctx.withLocal(pSym)
            pSym

        val returnType: TypedAst.Expr =
          ctor.returnType match {
            case Some(rType) =>
                val (t, errors) = TypeChecker.checkAndElaborate(rType, TypedAst.Expr.Sort()(SourceRange.empty))(using ctx, metas, data.idSupply)
                allErrors ++= errors
                t
            case None =>
              // by default return standard type
              var r = TypedAst.Expr.Var(this)(ctor.source)
              var pis = typeInfo.tpe
              for p <- typeInfo.params do {
                val Pi(c, d, _) = pis: @unchecked
                r = TypedAst.Expr.App(r, TypedAst.Expr.Var(p)(ctor.source), d)(ctor.source)
                pis = d
              }

              r
          }
        val referencedLocals: Set[Symbol] = TypeChecker.collectReferencedSymbols(returnType) ++ fields.flatMap(f => TypeChecker.collectReferencedSymbols(f.tpe))
        val implicitFields = typeInfo.params.filter(referencedLocals.contains)
        
        val tpe = buildPiType(implicitFields, fields, returnType)
        TypedAst.CtorDecl(ctorSym, implicitFields, fields, returnType, tpe)(ctor.source)


    TypedAst.TopLevel.DataDecl(
      this,
      typeInfo.params,
      ctors
    )(data.declAst.source)
  }

def buildPiType(implicitParams: List[LocalSymbol], explicitParams: List[LocalSymbol], result: TypedAst.Expr): TypedAst.Expr =
  implicitParams match {
    case x:: xs =>
      TypedAst.Expr.Pi(x, buildPiType(xs, explicitParams, result), false)(result.source)
    case Nil =>
      explicitParams match {
        case x::xs =>
          TypedAst.Expr.Pi(x, buildPiType(List(), xs, result), true)(result.source)
        case Nil => result
      }
  }

final case class CtorSymbol(dt: DatatypeSymbol, name: String) extends GlobalSymbol:
  def file: String = dt.file
  /** Calculates the type of the constructor.
   * For example with
   * datatype List[T: Type] = Nil | Cons(head: T, tail: List[T])
   *
   * The type of Cons would be: Pi(T: Type, Pi(head: T, Pi(tail: List[T], List[T]), true), true), false)
   * */
  def tpe: TypedAst.Expr = {
    val ctor = dt.typed.ctors.find(_.symbol.name == name).get
    ctor.tpe
  }


final case class FunctionSymbol(
  file: String,
  name: String,
)(
   // data to continue calculating the type and typed body of this symbol
   val continuationData: Option[FunctionSymbolContinuationData],
) extends GlobalSymbol:

  // TODO add caching for results
  var allErrors: Vector[TypeError] = Vector()
  var state: FunctionSymbolCheckState = FunctionSymbolCheckState.Init


  /** Calculate the type of the function */
  def tpe: TypedAst.Expr =
    state match {
      case FunctionSymbolCheckState.Init =>
        val data = continuationData.get
        val f = data.declAst
        val globals = data.globalNames.globalEnv(file)
        val context1 = TypeContext(globals, Map())
        val itemMetaStore = MetaStore()
        val (sig, ctx2, errors) = TypeChecker.checkFunSig(this, f.sig)(using context1, itemMetaStore, data.idSupply)
        allErrors ++= errors
        state = FunctionSymbolCheckState.SymbolCalculated(sig, ctx2, itemMetaStore)
        // TODO build pi type from signature
        sig.returnType
      case s: FunctionSymbolCheckState.SymbolCalculated =>
        s.sig.functionType
      case s: FunctionSymbolCheckState.BodyCalculated =>
        s.fun.sig.functionType
    }


  // calculates the typed body for the symbol
  def typedBody: Option[TypedAst.Expr] = {
    // force calculation of the type
    tpe
    state match {
      case FunctionSymbolCheckState.Init =>
        throw new IllegalStateException()
      case s: FunctionSymbolCheckState.SymbolCalculated =>
        val data = continuationData.get
        val f = data.declAst
        val (body, errors) = TypeChecker.check(f.body, s.sig.returnType)(using s.continuationContext, s.metaStore, data.idSupply)
        allErrors ++= errors
        state = FunctionSymbolCheckState.BodyCalculated(TypedAst.TopLevel.FunDecl(s.sig, body)(f.source))
        Some(body)
      case s: FunctionSymbolCheckState.BodyCalculated =>
        Some(s.fun.body)
    }
  }

object ErrorSymbols:
  def fun(name: String): FunctionSymbol =
    FunctionSymbol("", "")(None)
  def datatype(name: String): DatatypeSymbol =
    DatatypeSymbol("", name)(None)
  def constructor(name: String): CtorSymbol =
    CtorSymbol(datatype("unknown"), name)

enum SymbolSignature:
  case Def(tpe: Expr)
  case Datatype(implicitParams: List[LocalSymbol])

object GlobalSymbols:
  // Collects variable names referenced from a constructor signature expression.
  

  // Keeps only datatype implicit parameters that are needed by one constructor signature.
  

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
    buildGlobalNames(file, prog, false)
    buildGlobalNames("standard.minifumo", Standard.standardProgram, true)
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
        ListBuffer[TypeError]()
        val symbols = ListBuffer[(String, SourceRange, GlobalSymbol)]()
        // add the type symbol
        val dtSymbol = DatatypeSymbol(file, name)(Some(DatatypeSymbolContinuationData(d, globalNames, idSupply)))
        symbols.addOne((name, t.source, dtSymbol))


        // add symbols for the constructors
        for ctor <- ctors do
          symbols.addOne((ctor.name, ctor.source, CtorSymbol(dtSymbol, ctor.name)))
        symbols
      case f@ast.TopLevel.FunDecl(sig, _, exported) if exported || !onlyExported =>
        symbolsForFunDef(f, file, globalNames, idSupply)
      case _ =>
        List()

  private def symbolsForFunDef(decl: ast.TopLevel.FunDecl, file: String, globalNames: NameCache & SymbolCache, idSupply: TypeChecker.IdSupply): Iterable[(String, SourceRange, GlobalSymbol)] =
    val sig = decl.sig
    val symbol = FunctionSymbol(file, sig.name)(Some(FunctionSymbolContinuationData(decl, globalNames, idSupply)))
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
