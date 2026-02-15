package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.{AstTransform, FunParam, FunSig, SourceRange}
import com.github.peterzeller.minifumo.parser.{SyntaxError, parseFile}
import com.github.peterzeller.minifumo.typing.TypeChecker.{TypeError, checkProgram}
import com.github.peterzeller.minifumo.typing.TypedAst.Expr.UnknownType
import com.github.peterzeller.minifumo.typing.TypedAst.{ErrorSymbol, Expr, GlobalNameSymbol, GlobalSymbolSymbol, LocalSymbol}

import java.nio.file.Path
import scala.collection.mutable.ListBuffer
import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.common.MinifumoErrorWithPath
import com.github.peterzeller.minifumo.parser.parseInput
import com.github.peterzeller.minifumo.typing.SymbolSignature.Def

import scala.annotation.tailrec


case class GlobalSymbols(
  symbols: Map[String, GlobalSymbol] = Map()
)


case class GlobalName(file: Path, name: String)

case class GlobalSymbol(file: Path, name: String, symbolSignature: SymbolSignature):
  def toSymbol: GlobalSymbolSymbol =
    GlobalSymbolSymbol(name, file, this)

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

  def buildGlobalSymbols(file: Path, prog: ast.ProgramFile, symbolCache: NameCache&SymbolCache, onlyExported: Boolean): (Map[String, GlobalSymbol], List[TypeError]) =
    val (imports, errors1) = resolveImports(prog, symbolCache)
    val ownNames = buildGlobalNames(file, prog, false)
    val standardLibraryNames = buildGlobalNames(Path.of("standard.minifumo"), Standard.standardProgram, true)
    val preEnv = PreEnv(globalNames = standardLibraryNames ++ imports ++ ownNames)
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
  case class PreEnv(
    globalNames: Map[String, GlobalName],
    localNames: Map[String, LocalSymbol] = Map(),
  )

  // performs basic checks (like name resolution) on an expression appearing in a signature.
  // Only basic language constructs are allowed in signatures, like Pi expressions, names, constants, literals, and function applications.
  // This means, that we don't need to do complex type checking in function signatures.
  // We also don't check function application, only translate the terms to the typed AST.
  def checkSignatureExpr(expr: ast.Expr, env: PreEnv): (Expr, List[TypeError]) =
    expr match
      case ast.Expr.Lit(value) =>
        (Expr.Lit(value)(expr.source), List())
      case ast.Expr.Var("Type") =>
        (Expr.Sort()(expr.source), List())
      case ast.Expr.Var(name) =>
        // first lookup in local env
        env.localNames.get(name) match
          case Some(s) =>
            (Expr.Var(s)(expr.source), List())
          case None =>
            // then lookup in global env
            env.globalNames.get(name) match
              case Some(n) =>
                val s = GlobalNameSymbol(n.name, n.file)
                (Expr.Var(s)(expr.source), List())
              case None =>
                (Expr.Var(ErrorSymbol(name, UnknownType()(SourceRange.empty)))(expr.source), List(TypeError(s"Could not find ${name}", expr.source)))
      case ast.Expr.CallImplicit(callee, arg) =>
        val (c, e1) = checkSignatureExpr(callee, env)
        val (a, e2) = checkSignatureExpr(arg, env)
        val t = TypedAst.Expr.UnknownType()(expr.source)
        (TypedAst.Expr.AppImplicit(c, a, t)(expr.source), e1++e2)
      case ast.Expr.Call(callee, arg) =>
        val (c, e1) = checkSignatureExpr(callee, env)
        val (a, e2) = checkSignatureExpr(arg, env)
        val t = TypedAst.Expr.UnknownType()(expr.source)
        (TypedAst.Expr.App(c, a, t)(expr.source), e1++e2)
      case ast.Expr.Lambda(param, body) =>
        (TypedAst.Expr.UnknownType()(expr.source), List(TypeError("Cannot use lambda expressions in function signatures", expr.source)))
      case ast.Expr.Pi(param, body) =>
        val (dom, errors1) = checkSignatureExpr(param.tpe, env)
        val (cod, errors2) = checkSignatureExpr(body, env)
        val s = LocalSymbol(param.name, dom, 0) // TODO proper index
        (TypedAst.Expr.Pi(s, cod, isImplicit = false)(expr.source), errors1 ++ errors2)
      case ast.Expr.LetIn(name, tpe, value, body) =>
        (TypedAst.Expr.UnknownType()(expr.source), List(TypeError("Cannot use let expressions in function signatures", expr.source)))
      case ast.Expr.Match(scrutinee, cases) =>
        (TypedAst.Expr.UnknownType()(expr.source), List(TypeError("Cannot use match expressions in function signatures", expr.source)))
      case ast.Expr.Hole() =>
        (TypedAst.Expr.UnknownType()(expr.source), List())


  private def topLevelToGlobalSymbols(file: Path, onlyExported: Boolean, env: PreEnv)(t: ast.TopLevel): (Iterable[(String, SourceRange, GlobalSymbol)], Iterable[TypeError]) =
    t match
      case ast.TopLevel.DataDecl(name, implicitParams, ctors, exported) if exported || !onlyExported =>
        // TODO add symbols for datatypes
        var nextId = 0
        var localNames = env.localNames
        val errors = ListBuffer[TypeError]()
        val typeParamTypes = ListBuffer[Expr]()
        for param <- implicitParams do
          val (paramType, paramErrors) = checkSignatureExpr(param.tpe, env.copy(localNames = localNames))
          errors.addAll(paramErrors)
          val symbol = LocalSymbol(param.name, paramType, nextId)
          nextId += 1
          localNames = localNames + (param.name -> symbol)
          typeParamTypes.addOne(symbol.tpe)
        val symbols = ListBuffer[(String, SourceRange, GlobalSymbol)]()
        // add the type symbol
        val symbol = GlobalSymbol(file, name, SymbolSignature.Datatype(typeParamTypes.toList))
        symbols.addOne((name, t.source, symbol))

        // build the type expression refering to this data type
        var dt: ast.Expr = ast.Expr.Var(name)(t.source)
        for i <- implicitParams do
          dt = ast.Expr.CallImplicit(dt, ast.Expr.Var(i.name)(t.source))(t.source)

        // add symbols for the constructors
        for ctor <- ctors do
          // create a dummy fun decl for the constructor
          val sig = FunSig(ctor.name, implicitParams, ctor.fields.map(f => FunParam(f.name, f.tpe)(f.source)), dt)(ctor.source)
          val f: ast.TopLevel.FunDecl = ast.TopLevel.FunDecl(sig, ast.Expr.Hole()(ctor.source), true)(ctor.source)

          val (syms, errs) = symbolsForFunDef(f, file, env)
          errors.addAll(errs)
          symbols.addAll(syms)

        (symbols.toList, errors.toList)
      case f@ast.TopLevel.FunDecl(sig, _, exported) if exported || !onlyExported =>
        symbolsForFunDef(f, file, env)
      case _ =>
        (List(), List())

  def symbolsForFunDef(decl: ast.TopLevel.FunDecl, file: Path, env: PreEnv): (Iterable[(String, SourceRange, GlobalSymbol)], Iterable[TypeError]) =
    val sig = decl.sig
    var nextId = 0
    var localNames = env.localNames
    val errors = ListBuffer[TypeError]()
    val implicitParamTypes = ListBuffer[LocalSymbol]()
    val paramTypes = ListBuffer[LocalSymbol]()
    for param <- sig.implicitParams do
      val (paramType, paramErrors) = checkSignatureExpr(param.tpe, env.copy(localNames = localNames))
      errors.addAll(paramErrors)
      val symbol = LocalSymbol(param.name, paramType, nextId)
      nextId += 1
      localNames = localNames + (param.name -> symbol)
      implicitParamTypes.addOne(symbol)
    for param <- sig.params do
      val (paramType, paramErrors) = checkSignatureExpr(param.tpe, env.copy(localNames = localNames))
      errors.addAll(paramErrors)
      val symbol = LocalSymbol(param.name, paramType, nextId)
      nextId += 1
      localNames = localNames + (param.name -> symbol)
      paramTypes.addOne(symbol)
    val (returnType, returnErrors) = checkSignatureExpr(sig.returnType, env.copy(localNames = localNames))
    errors.addAll(returnErrors)
    val funType = (implicitParamTypes.toList, paramTypes.toList).match
      case (Nil, Nil) => returnType
      case _ =>
        val explicitPis = paramTypes.foldRight(returnType) { (dom, cod) =>
          TypedAst.Expr.Pi(dom, cod, isImplicit = false)(sig.source)
        }
        val implicitPis = implicitParamTypes.foldRight(explicitPis) { (dom, cod) =>
          TypedAst.Expr.Pi(dom, cod, isImplicit = true)(sig.source)
        }
        implicitPis
    val symbol = GlobalSymbol(file, sig.name, SymbolSignature.Def(funType))
    (List((sig.name, sig.source, symbol)), errors.toList)

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



class ProjectSymbolCache(projectRoot: Path) extends NameCache with SymbolCache:
  private var astCache: Map[String, (ast.ProgramFile, List[SyntaxError])] = Map()
  private var namesCache: Map[String, Map[String, GlobalName]] = Map()
  private var symbolCache: Map[String, Map[String, GlobalSymbol]] = Map()
  private var typedAstCache: Map[String, (TypedAst.Program, List[TypeError])] = Map()

  def toPath(importPath: String): Path =
    projectRoot.resolve(importPath)

  def fromPath(p: Path): String =
    projectRoot.relativize(p).normalize().toString

  def makeRelative(path: Path): String =
    fromPath(path)


  def getAst(path: String): (ast.ProgramFile, List[SyntaxError]) =
    astCache.get(path) match
      case Some(a) => a
      case None =>
        val (cst, syntaxErrors) =
          if path == "standard.minifumo" then
            parseInput(Standard.loadStandardSource())
          else
            parseFile(toPath(path))
        val ast = AstTransform.program(cst)
        val r = (ast, syntaxErrors)
        astCache += path -> r
        r


  def globalNames(path: String): Map[String, GlobalName] =
    namesCache.get(path) match
      case Some(m) => m
      case None =>
        val r = GlobalSymbols.buildGlobalNames(toPath(path), getAst(path)._1, true)
        namesCache += path -> r
        r

  def globalSymbols(path: String): Map[String, GlobalSymbol] =
    symbolCache.get(path) match
      case Some(m) => m
      case None =>
        val (r,_) = GlobalSymbols.buildGlobalSymbols(toPath(path), getAst(path)._1, this, true)
        symbolCache += path -> r
        r

  def allPaths: Set[String] = astCache.keySet + "standard.minifumo"

  def typedAst(path: String): (TypedAst.Program, List[TypeError]) =
    typedAstCache.get(path) match
      case Some(m) => m
      case None =>
        val (ast, _) = getAst(path)
        val r = checkProgram(toPath(path), ast, this, path != "standard.minifumo")
        typedAstCache += path -> r
        r

  def allErrors: List[MinifumoErrorWithPath] =
    val syntaxErrors = astCache.flatMap(p =>  p._2._2.map(MinifumoErrorWithPath(toPath(p._1), _)))
    val typeErrors = astCache.keysIterator.flatMap(p => typedAst(p)._2.map(MinifumoErrorWithPath(toPath(p), _)))
    (syntaxErrors ++ typeErrors).toList