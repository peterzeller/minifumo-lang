package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.{AstTransform, SourceRange}
import com.github.peterzeller.minifumo.parser.{SyntaxError, parseFile}
import com.github.peterzeller.minifumo.typing.TypeChecker.TypeError
import com.github.peterzeller.minifumo.typing.TypedAst.Expr.UnknownType
import com.github.peterzeller.minifumo.typing.TypedAst.{ErrorSymbol, Expr, GlobalNameSymbol, LocalSymbol}

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
    val preEnv = PreEnv(globalNames = imports ++ ownNames)
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
        (TypedAst.Expr.Pi(dom, cod, isImplicit = false)(expr.source), errors1 ++ errors2)
      case ast.Expr.LetIn(name, tpe, value, body) =>
        (TypedAst.Expr.UnknownType()(expr.source), List(TypeError("Cannot use let expressions in function signatures", expr.source)))
      case ast.Expr.Match(scrutinee, cases) =>
        (TypedAst.Expr.UnknownType()(expr.source), List(TypeError("Cannot use match expressions in function signatures", expr.source)))
      case ast.Expr.Hole() =>
        (TypedAst.Expr.UnknownType()(expr.source), List())


  private def topLevelToGlobalSymbols(file: Path, onlyExported: Boolean, env: PreEnv)(t: ast.TopLevel): (Iterable[(String, SourceRange, GlobalSymbol)], Iterable[TypeError]) =
    t match
      case ast.TopLevel.DataDecl(name, implicitParams, _, exported) if exported || !onlyExported =>
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
        val symbol = GlobalSymbol(file, name, SymbolSignature.Datatype(typeParamTypes.toList))
        (List((name, t.source, symbol)), errors.toList)
      case ast.TopLevel.FunDecl(sig, _, exported) if exported || !onlyExported =>
        // TODO add symbol for function
        var nextId = 0
        var localNames = env.localNames
        val errors = ListBuffer[TypeError]()
        val implicitParamTypes = ListBuffer[Expr]()
        val paramTypes = ListBuffer[Expr]()
        for param <- sig.implicitParams do
          val (paramType, paramErrors) = checkSignatureExpr(param.tpe, env.copy(localNames = localNames))
          errors.addAll(paramErrors)
          val symbol = LocalSymbol(param.name, paramType, nextId)
          nextId += 1
          localNames = localNames + (param.name -> symbol)
          implicitParamTypes.addOne(paramType)
        for param <- sig.params do
          val (paramType, paramErrors) = checkSignatureExpr(param.tpe, env.copy(localNames = localNames))
          errors.addAll(paramErrors)
          val symbol = LocalSymbol(param.name, paramType, nextId)
          nextId += 1
          localNames = localNames + (param.name -> symbol)
          paramTypes.addOne(paramType)
        val (returnType, returnErrors) = checkSignatureExpr(sig.returnType, env.copy(localNames = localNames))
        errors.addAll(returnErrors)
        val funType = (implicitParamTypes.toList, paramTypes.toList).match
          case (Nil, Nil) => returnType
          case _ =>
            val implicitPis = implicitParamTypes.foldRight(returnType) { (dom, cod) =>
              TypedAst.Expr.Pi(dom, cod, isImplicit = true)(sig.source)
            }
            paramTypes.foldRight(implicitPis) { (dom, cod) =>
              TypedAst.Expr.Pi(dom, cod, isImplicit = false)(sig.source)
            }
        val symbol = GlobalSymbol(file, sig.name, SymbolSignature.Def(funType))
        (List((sig.name, sig.source, symbol)), errors.toList)
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
