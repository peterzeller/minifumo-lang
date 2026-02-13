package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.ast.SourceRange
import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.common.MinifumoError
import com.github.peterzeller.minifumo.typing.TypedAst.*
import com.github.peterzeller.minifumo.typing.TypedAst.Expr.{Sort, UnknownType}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import java.nio.file.Path

object TypeChecker:
  final case class TypeError(message: String, source: ast.SourceRange) extends MinifumoError:
    throw new RuntimeException(s"Constructed type error $message at line ${source.start.line}")


  /** Type-checks a program */
  def checkProgram(path: Path, program: ast.ProgramFile, globalNames: NameCache & SymbolCache, importStandard: Boolean): (TypedAst.Program, List[TypeError]) =
    try
      val errors = ListBuffer[TypeError]()
      val idSupply = IdSupply()
      val metaStore = MetaStore()
      var (symbolMap, importErrors) = GlobalSymbols.buildGlobalSymbols(path, program, globalNames, false)
      if importStandard then
        // import the standard library symbols into the program file scope
        val (standardLibSymbolMap, standardLibImportErrors) = GlobalSymbols.buildGlobalSymbols(Path.of("standard.minifumo"), Standard.standardProgram, globalNames, false)
        symbolMap ++= standardLibSymbolMap
        importErrors ++= standardLibImportErrors
      errors.addAll(importErrors)
      val globals = GlobalEnv(names = symbolMap)
      val typedItems = program.items.map {
        case decl: ast.TopLevel.DataDecl =>
          buildDataDecl(decl, globals, idSupply)
        case decl: ast.TopLevel.FunDecl =>
          val context1 = TypeContext(globals, Map())
          val (typedSig, context2, errs) = checkFunSig(decl.sig)(using context1, metaStore, idSupply)
          errors.addAll(errs)

          val typedBody = checkFunctionBody(decl.body, typedSig.returnType, context2, metaStore, idSupply, errors)
          TypedAst.TopLevel.FunDecl(typedSig, typedBody)(decl.source)
      }
      (TypedAst.Program(typedItems)(program.source), errors.toList)
    catch
      case e: Exception =>
        throw new RuntimeException(s"error checking $path", e)


  private def checkFunSig(sig: ast.FunSig)(implicit ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.FunSig, TypeContext,  List[TypeError]) = {
//    throw new RuntimeException(s"Checking of fun sig $sig is not yet implemented")
    var mEnv = ctx
    val errors = mutable.ListBuffer[TypeError]()
    val typedImplicitParams = mutable.ListBuffer[TypedAst.LocalSymbol]()
    val typedParams = mutable.ListBuffer[TypedAst.LocalSymbol]()
    // first check the implicit params
    for p <- sig.implicitParams do
      val (paramType, errs) = check(p.tpe, Sort()(SourceRange.empty))(using mEnv)
      errors.addAll(errs)

      val localSymbol = LocalSymbol(p.name, paramType, ids.freshLocalId())
      mEnv = mEnv.withLocal(localSymbol, Some(paramType))
      typedImplicitParams.addOne(localSymbol)
    // next check the explicit params
    for p <- sig.params do
      val (paramType, errs) = check(p.tpe, Sort()(SourceRange.empty))(using mEnv)
      errors.addAll(errs)
      val localSymbol = LocalSymbol(p.name, paramType, ids.freshLocalId())
      mEnv = mEnv.withLocal(localSymbol, Some(paramType))
      typedParams.addOne(localSymbol)

    val (returnType, errs) = check(sig.returnType, Sort()(SourceRange.empty))(using mEnv)
    errors.addAll(errs)

    // construct function type as Pi type by going backwards
    var fnType: Expr = returnType
    for p <- typedParams.reverseIterator do
      fnType = TypedAst.Expr.Pi(p, fnType, false)(p.tpe.source.merge(fnType.source))
    for p <- typedImplicitParams.reverseIterator do
      fnType = TypedAst.Expr.Pi(p, fnType, true)(p.tpe.source.merge(fnType.source))

    val sym = FunctionSymbol(sig.name, fnType)
    (TypedAst.FunSig(sym, typedImplicitParams.toList, typedParams.toList, returnType), mEnv, errors.toList)
  }

  /** Provides a lookup interface for local and global symbols during type checking. */
  trait Context:
    def lookupSymbol(name: String): Option[TypedAst.Symbol]
    def lookupValue(symbol: TypedAst.TermSymbol): Option[TypedAst.Expr]

  /** Provides a mutable store for meta-variable assignments. */
  trait MetaContext:
    def assign(metaId: Int, term: TypedAst.Expr): Unit
    def getAssignment(metaId: Int): Option[TypedAst.Expr]

  /** Represents a binding in the local context. */
  private final case class LocalBinding(symbol: TypedAst.TermSymbol, value: Option[TypedAst.Expr])

  /** Stores global symbols for type checking. */
  private final case class GlobalEnv(
      names: Map[String, GlobalSymbol]
    )

  /** Implements a context with local bindings and global symbols. */
  private final case class TypeContext(globals: GlobalEnv, locals: Map[String, LocalBinding]) extends Context:
    override def lookupSymbol(name: String): Option[TypedAst.Symbol] =
      locals.get(name).map(_.symbol)
        .orElse(globals.names.get(name).map(g => TypedAst.GlobalSymbolSymbol(g.name, g.file, g)))

    override def lookupValue(symbol: TypedAst.TermSymbol): Option[TypedAst.Expr] =
      locals.values.find(_.symbol == symbol).flatMap(_.value)

    /** Adds a local binding to the context. */
    def withLocal(symbol: TypedAst.TermSymbol, value: Option[TypedAst.Expr] = None): TypeContext =
      copy(locals = locals + (symbol.name -> LocalBinding(symbol, value)))

    /** Adds multiple local bindings to the context. */
    def withLocals(symbols: List[TypedAst.TermSymbol]): TypeContext =
      symbols.foldLeft(this) { (ctx, symbol) => ctx.withLocal(symbol) }

  /** Stores meta-variable assignments during unification. */
  private final case class MetaStore(assignments: mutable.Map[Int, TypedAst.Expr] = mutable.Map()) extends MetaContext:
    override def assign(metaId: Int, term: TypedAst.Expr): Unit =
      assignments.update(metaId, term)

    override def getAssignment(metaId: Int): Option[TypedAst.Expr] =
      assignments.get(metaId)

  /** Tracks identifier allocation for local symbols and metas. */
  private final case class IdSupply(var nextId: Int = 0, var nextMeta: Int = 0):
    /** Creates a fresh local symbol id. */
    def freshLocalId(): Int =
      val id = nextId
      nextId += 1
      id

    /** Creates a fresh meta-variable id. */
    def freshMetaId(): Int =
      val id = nextMeta
      nextMeta += 1
      id

  /** Describes a function signature after outline typing. */
  private final case class FunctionInfo(
      symbol: TypedAst.FunctionSymbol,
      implicitParams: List[TypedAst.LocalSymbol],
      params: List[TypedAst.LocalSymbol],
      returnType: TypedAst.Expr
    )

  /** Builds a typed data declaration. */
  private def buildDataDecl(
      decl: ast.TopLevel.DataDecl,
      globals: GlobalEnv,
      idSupply: IdSupply
    ): TypedAst.TopLevel =
    val typeParams = decl.implicitParams.map(_.name)
    val localTypeParams = decl.implicitParams.map { param =>
      val paramType = signatureExpr(param.tpe, globals, Map())
      param.name -> TypedAst.LocalSymbol(param.name, paramType, idSupply.freshLocalId())
    }.toMap
    val ctorDecls = decl.ctors.map { ctor =>
      val fields = ctor.fields.map { field =>
        val fieldType = signatureExpr(field.tpe, globals, localTypeParams)
        TypedAst.CtorField(field.name, fieldType)(field.source)
      }
      val symbol: CtorSymbol =
        globals.names.get(ctor.name) match {
          case Some(symbol) =>
            val (s, _) = globalSymbolToCtorSymbol(symbol)
            s
          case None =>
            TypedAst.CtorSymbol(ctor.name, TypedAst.Expr.UnknownType()(ctor.source))
        }
      TypedAst.CtorDecl(symbol, fields)(ctor.source)
    }
    TypedAst.TopLevel.DataDecl(decl.name, typeParams, ctorDecls)(decl.source)

  /** Type-checks a function body against its return type. */
  private def checkFunctionBody(
      body: ast.Expr,
      returnType: TypedAst.Expr,
      context: TypeContext,
      metas: MetaContext,
      idSupply: IdSupply,
      errors: ListBuffer[TypeError]): TypedAst.Expr =
    val (typedBody, errs) = check(body, returnType)(using context, metas, idSupply)
    errors.addAll(errs)
    typedBody

  /** Translates a signature expression to a typed expression. */
  private def signatureExpr(expr: ast.Expr, globals: GlobalEnv, locals: Map[String, TypedAst.TermSymbol]): TypedAst.Expr =
    expr match
      case ast.Expr.Lit(value) => TypedAst.Expr.Lit(value)(expr.source)
      case ast.Expr.Var(name) =>
        if name == "unit" then
          TypedAst.Expr.UnknownType()(expr.source)
        else
          locals.get(name) match
            case Some(symbol) =>
              TypedAst.Expr.Var(symbol)(expr.source)
            case None =>
              globals.names.get(name) match
                case Some(symbol) =>
                  TypedAst.Expr.Var(symbol.toSymbol)(expr.source)
                case None =>
                  TypedAst.Expr.Var(TypedAst.ErrorSymbol(name, TypedAst.Expr.UnknownType()(expr.source)))(expr.source)
      case ast.Expr.Call(callee, arg) =>
        val calleeExpr = signatureExpr(callee, globals, locals)
        val argExpr = signatureExpr(arg, globals, locals)
        TypedAst.Expr.App(calleeExpr, argExpr, TypedAst.Expr.UnknownType()(expr.source))(expr.source)
      case ast.Expr.CallImplicit(callee, arg) =>
        val calleeExpr = signatureExpr(callee, globals, locals)
        val argExpr = signatureExpr(arg, globals, locals)
        TypedAst.Expr.AppImplicit(calleeExpr, argExpr, TypedAst.Expr.UnknownType()(expr.source))(expr.source)
      case ast.Expr.Pi(param, body) =>
        val dom = signatureExpr(param.tpe, globals, locals)
        val cod = signatureExpr(body, globals, locals)
        val domSym = LocalSymbol(param.name, dom, 0) // TODO create ID
        TypedAst.Expr.Pi(domSym, cod, isImplicit = false)(expr.source)
      case ast.Expr.Hole() =>
        TypedAst.Expr.UnknownType()(expr.source)
      case _ =>
        TypedAst.Expr.UnknownType()(expr.source)


  /** Checks if two types are definitionally equal, solving metas as needed. */
  def isDefEq(t1: TypedAst.Expr, t2: TypedAst.Expr)
             (implicit ctx: Context, metas: MetaContext): Boolean =
    val norm1 = whnf(t1)
    val norm2 = whnf(t2)
    (norm1, norm2) match
      case (TypedAst.Expr.UnknownType(), _) => true
      case (_, TypedAst.Expr.UnknownType()) => true
      case (TypedAst.Expr.Meta(id, _), other) =>
        solveMeta(id, other)
      case (other, TypedAst.Expr.Meta(id, _)) =>
        solveMeta(id, other)
      case (TypedAst.Expr.Var(s1), TypedAst.Expr.Var(s2)) if symbolsEqual(s1, s2) => true
      case (TypedAst.Expr.Var(p1: TypedAst.LocalSymbol), TypedAst.Expr.Var(p2: TypedAst.LocalSymbol))
          if p1.name == p2.name => true // TODO name equality is not enough
      case (TypedAst.Expr.Lit(v1), TypedAst.Expr.Lit(v2)) if v1 == v2 => true
      case (TypedAst.Expr.Sort(), TypedAst.Expr.Sort()) => true
      case (TypedAst.Expr.App(c1, a1, _), TypedAst.Expr.App(c2, a2, _)) =>
        isDefEq(c1, c2) && isDefEq(a1, a2)
      case (TypedAst.Expr.AppImplicit(c1, a1, _), TypedAst.Expr.AppImplicit(c2, a2, _)) =>
        isDefEq(c1, c2) && isDefEq(a1, a2)
      case (TypedAst.Expr.Lambda(p1, b1, _), TypedAst.Expr.Lambda(p2, b2, _)) =>
        isDefEq(p1.tpe, p2.tpe) && isDefEq(b1, b2) // TODO this might not be correct, since we do not consider names; better use de-bruijn indices
      case (TypedAst.Expr.Pi(d1, c1, i1), TypedAst.Expr.Pi(d2, c2, i2)) if i1 == i2 =>
        isDefEq(d1.tpe, d2.tpe) && isDefEq(c1, c2) // TODO this might not be correct, since we do not consider names; better use de-bruijn indices
      case _ => false

  def symbolsEqual(a: Symbol, b: Symbol): Boolean =
    if a == b then
      true
    else
      (a,b) match
        case (GlobalSymbolSymbol(an, af, _), GlobalNameSymbol(bn, bf)) =>
          an == bn && af == bf
        case (GlobalNameSymbol(an, af), GlobalSymbolSymbol(bn, bf, _)) =>
          an == bn && af == bf
        case _ =>
          false

  /** Reduces a term to weak head normal form. */
  def whnf(term: TypedAst.Expr)(implicit ctx: Context, metas: MetaContext): TypedAst.Expr =
    instantiate(term) match
      case TypedAst.Expr.App(callee, arg, tpe) =>
        whnf(callee) match
          case TypedAst.Expr.Lambda(param, body, _) => whnf(substitute(body, param, arg))
          case other => TypedAst.Expr.App(other, arg, tpe)(term.source)
      case TypedAst.Expr.AppImplicit(callee, arg, tpe) =>
        whnf(callee) match
          case TypedAst.Expr.Lambda(param, body, _) => whnf(substitute(body, param, arg))
          case other => TypedAst.Expr.AppImplicit(other, arg, tpe)(term.source)
      case TypedAst.Expr.LetIn(symbol, _, _, value, body) =>
        whnf(substitute(body, symbol, value))
      case other => other

  /** Replaces solved metavariables in a term with their assigned values. */
  def instantiate(term: TypedAst.Expr)(implicit metas: MetaContext): TypedAst.Expr =
    term match
      case TypedAst.Expr.Meta(id, _) =>
        metas.getAssignment(id).map(instantiate).getOrElse(term)
      case TypedAst.Expr.App(callee, arg, tpe) =>
        TypedAst.Expr.App(instantiate(callee), instantiate(arg), instantiate(tpe))(term.source)
      case TypedAst.Expr.AppImplicit(callee, arg, tpe) =>
        TypedAst.Expr.AppImplicit(instantiate(callee), instantiate(arg), instantiate(tpe))(term.source)
      case TypedAst.Expr.Lambda(param, body, tpe) =>
        TypedAst.Expr.Lambda(param, instantiate(body), instantiate(tpe))(term.source)
      case TypedAst.Expr.LetIn(symbol, isConstant, declaredType, value, body) =>
        TypedAst.Expr.LetIn(symbol, isConstant, instantiate(declaredType), instantiate(value), instantiate(body))(term.source)
      case TypedAst.Expr.Pi(dom, cod, isImplicit) =>
        TypedAst.Expr.Pi(instantiateLocalSymbol(dom), instantiate(cod), isImplicit)(term.source)
      case TypedAst.Expr.Match(scrutinee, cases) =>
        val newCases = cases.map(c => TypedAst.MatchCase(c.pattern, instantiate(c.body))(c.source))
        TypedAst.Expr.Match(instantiate(scrutinee), newCases)(term.source)
      case other => other

  def instantiateLocalSymbol(s: LocalSymbol)(implicit metas: MetaContext): LocalSymbol =
    LocalSymbol(s.name, instantiate(s.tpe), s.id)

  /** Infers the type of an expression, producing a typed expression alongside its type. */
  private def infer(expr: ast.Expr)
                   (implicit ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    expr match
      case ast.Expr.Var("Type") =>
        (TypedAst.Expr.Sort()(expr.source), TypedAst.Expr.Sort()(expr.source), List())
      case ast.Expr.Var(name) =>
        ctx.lookupSymbol(name) match
          case Some(symbol) =>
            (TypedAst.Expr.Var(symbol)(expr.source), symbol.tpe, List())
          case None =>
            val errs = List(TypeError(s"Unknown symbol ${name}", expr.source))
            (TypedAst.Expr.Var(???)(expr.source), ???, errs)
      case ast.Expr.Lit(value) =>
        val typed = TypedAst.Expr.Lit(value)(expr.source)
        val tpe = literalType(value, ctx)
        (typed, tpe, List())
      case ast.Expr.Call(callee, arg) =>
        val (typedCallee, calleeType, errs1) = infer(callee)
        whnf(calleeType) match {
          case TypedAst.Expr.Pi(dom, cod, isImplicit) =>
            val (typedArg, errs2) = check(arg, dom.tpe)
            // replace
            val resultType = substitute(cod, dom, typedArg)
            val errs3 =
              if !isImplicit then List()
              else List(TypeError(s"Expected an implicit function argument", callee.source))
            (TypedAst.Expr.App(typedCallee, typedArg, resultType)(expr.source), resultType, errs1 ++ errs2 ++ errs3)
          case other =>
            val (typedArg, _, errs2) = infer(arg)
            val resultType = Expr.UnknownType()(SourceRange.empty)
            val errs3 =
              if other.isInstanceOf[UnknownType] then List()
              else List(TypeError(s"Expected a function but found expression of type $other", callee.source))
            (TypedAst.Expr.App(typedCallee, typedArg, resultType)(expr.source), resultType, errs1 ++ errs2 ++ errs3)
        }
      case ast.Expr.CallImplicit(callee, arg) =>
        val (typedCallee, calleeType, errs1) = infer(callee)
        whnf(calleeType) match {
          case TypedAst.Expr.Pi(dom, cod, isImplicit) =>
            val (typedArg, errs2) = check(arg, dom.tpe)
            // replace
            val resultType = substitute(cod, dom, typedArg)
            val errs3 =
              if isImplicit then List()
              else List(TypeError(s"Expected an explicit function argument", callee.source))
            (TypedAst.Expr.AppImplicit(typedCallee, typedArg, resultType)(expr.source), resultType, errs1 ++ errs2 ++ errs3)
          case other =>
            val (typedArg, _, errs2) = infer(arg)
            val resultType = Expr.UnknownType()(SourceRange.empty)
            val errs3 =
              if other.isInstanceOf[UnknownType] then List()
              else List(TypeError(s"Expected a function but found expression of type $other", callee.source))
            (TypedAst.Expr.AppImplicit(typedCallee, typedArg, resultType)(expr.source), resultType, errs1 ++ errs2 ++ errs3)
        }
      case ast.Expr.Lambda(param, body) =>
        val paramType = param.tpe.map(t => signatureExpr(t, ctx.globals, Map())).getOrElse(TypedAst.Expr.UnknownType()(expr.source))
        val LocalSymbol = TypedAst.LocalSymbol(param.name, paramType, ids.freshLocalId())
        val LocalSymbol2 = TypedAst.LocalSymbol(param.name, paramType, ids.freshLocalId())
        val bodyCtx = ctx.withLocal(LocalSymbol)
        val (bodyExpr, bodyType, errs) = infer(body)(using bodyCtx, metas, ids)
        val fnType = TypedAst.Expr.Pi(LocalSymbol2, bodyType, isImplicit = false)(expr.source)
        (TypedAst.Expr.Lambda(LocalSymbol, bodyExpr, fnType)(expr.source), fnType, errs)
      case ast.Expr.LetIn(name, declaredType, value, body) =>
        val inferredValue = declaredType match
          case Some(tpe) =>
            val expected = signatureExpr(tpe, ctx.globals, Map())
            val (typedValue, errs) = check(value, expected)
            (typedValue, expected, errs)
          case None => infer(value)
        val (valueExpr, valueType, errs) = inferredValue
        val symbol = TypedAst.LocalSymbol(name, valueType, ids.freshLocalId())
        val bodyCtx = ctx.withLocal(symbol, Some(valueExpr))
        val (bodyExpr, bodyType, errs2) =  infer(body)(using bodyCtx, metas, ids)
        (TypedAst.Expr.LetIn(symbol, isConstant = false, valueType, valueExpr, bodyExpr)(expr.source), bodyType, errs ++ errs2)

      case ast.Expr.Pi(param, body) =>
        val dom = signatureExpr(param.tpe, ctx.globals, Map())
        val cod = signatureExpr(body, ctx.globals, Map())
        val sym = LocalSymbol(param.name, dom, ids.freshLocalId())
        val piExpr = TypedAst.Expr.Pi(sym, cod, isImplicit = false)(expr.source)
        (piExpr, TypedAst.Expr.Sort()(expr.source), List())
      case ast.Expr.Match(scrutinee, cases) =>
        val (scrutineeExpr, scrutineeType, errs) = infer(scrutinee)
        val resultMeta = freshMeta(TypedAst.Expr.UnknownType()(expr.source), expr.source)
        val typedCases = cases.map { case ast.MatchCase(pattern, body) =>
          val (typedPattern, patternCtx, patternErrors) = checkPattern(pattern, scrutineeType, ctx, ids)
          val caseCtx = ctx.copy(locals = ctx.locals ++ patternCtx)
          val (typedBody, bodyErrs) = check(body, resultMeta)(using caseCtx, metas, ids)
          (typedPattern, typedBody, patternErrors ++ bodyErrs)
        }
        val errors = typedCases.flatMap(_._3)
        val typedCasesExpr = typedCases.map { case (pat, bodyExpr, _) =>
          TypedAst.MatchCase(pat, bodyExpr)(bodyExpr.source)
        }
        (TypedAst.Expr.Match(scrutineeExpr, typedCasesExpr)(expr.source), resultMeta, errs++errors)
      case ast.Expr.Hole() =>
        val meta = freshMeta(TypedAst.Expr.UnknownType()(expr.source), expr.source)
        (meta, TypedAst.Expr.UnknownType()(expr.source), List())


  /** Checks an expression against an expected type. */
  private def check(expr: ast.Expr, expectedType: TypedAst.Expr)
                   (implicit ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, List[TypeError]) =
    val expectedNorm = whnf(expectedType)
    (expr, expectedNorm) match
      case (ast.Expr.Lambda(param, body), TypedAst.Expr.Pi(dom, cod, false)) =>
        val p = TypedAst.LocalSymbol(param.name, dom.tpe, ids.freshLocalId())
        val bodyCtx = ctx.withLocal(p)
        val (typedBody, errs) = check(body, cod)(using bodyCtx, metas, ids)
        (TypedAst.Expr.Lambda(p, typedBody, expectedNorm)(expr.source), errs)

      case _ =>
        // TODO for some expressions, like match-cases, it might make sense to transfer expected type into the cases to get better error messages and better inference
        // TODO consider adding expectedType as optional parameter to infer
        val (inferredExpr, inferredType, errs) = infer(expr)
        val errs2 =
          if isDefEq(inferredType, expectedNorm) then
            errs
          else
            TypeError(s"Expected ${prettyExpr(expectedNorm)} but got ${prettyExpr(inferredType)}", expr.source) :: errs
        (inferredExpr, errs2)

//  /** Inserts implicit arguments as metas before explicit application. */
//  private def insertImplicitArgs(
//      callee: TypedAst.Expr,
//      calleeType: TypedAst.Expr,
//      source: ast.SourceRange
//    )(implicit ctx: Context, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr) =
//    var currentExpr = callee
//    var currentType = calleeType
//    var keepGoing = true
//    while keepGoing do
//      whnf(currentType) match
//        case TypedAst.Expr.Pi(dom, cod, true) =>
//          val meta = freshMeta(dom.tpe, source)
//          currentExpr = TypedAst.Expr.AppImplicit(currentExpr, meta, cod)(source)
//          currentType = cod
//        case _ => keepGoing = false
//    (currentExpr, currentType)

  /** Creates a fresh meta-variable expression. */
  private def freshMeta(tpe: TypedAst.Expr, source: ast.SourceRange)(implicit ids: IdSupply): TypedAst.Expr =
    TypedAst.Expr.Meta(ids.freshMetaId(), tpe)(source)

  /** Checks whether a meta-variable can be solved with a term. */
  private def solveMeta(metaId: Int, term: TypedAst.Expr)
                       (implicit metas: MetaContext): Boolean =
    if occurs(metaId, term) then
      false
    else
      metas.assign(metaId, term)
      true

  /** Performs an occurs check for a meta-variable inside a term. */
  private def occurs(metaId: Int, term: TypedAst.Expr): Boolean =
    term match
      case TypedAst.Expr.Meta(id, _) => id == metaId
      case TypedAst.Expr.App(callee, arg, _) => occurs(metaId, callee) || occurs(metaId, arg)
      case TypedAst.Expr.AppImplicit(callee, arg, _) => occurs(metaId, callee) || occurs(metaId, arg)
      case TypedAst.Expr.Lambda(_, body, _) => occurs(metaId, body)
      case TypedAst.Expr.LetIn(_, _, _, value, body) => occurs(metaId, value) || occurs(metaId, body)
      case TypedAst.Expr.Pi(dom, cod, _) => occurs(metaId, dom.tpe) || occurs(metaId, cod)
      case TypedAst.Expr.Match(scrutinee, cases) =>
        occurs(metaId, scrutinee) || cases.exists(c => occurs(metaId, c.body))
      case _ => false

  /** Substitutes a local symbol with a value in a term. */
  private def substitute(term: TypedAst.Expr, symbol: TypedAst.LocalSymbol, value: TypedAst.Expr): TypedAst.Expr =
    term match
      case TypedAst.Expr.Var(sym: TypedAst.LocalSymbol) if sym.id == symbol.id => value
      case TypedAst.Expr.App(callee, arg, tpe) =>
        TypedAst.Expr.App(substitute(callee, symbol, value), substitute(arg, symbol, value), tpe)(term.source)
      case TypedAst.Expr.AppImplicit(callee, arg, tpe) =>
        TypedAst.Expr.AppImplicit(substitute(callee, symbol, value), substitute(arg, symbol, value), tpe)(term.source)
      case TypedAst.Expr.Lambda(param, body, tpe) if param.id == symbol.id =>
        TypedAst.Expr.Lambda(param, body, tpe)(term.source)
      case TypedAst.Expr.Lambda(param, body, tpe) =>
        TypedAst.Expr.Lambda(param, substitute(body, symbol, value), tpe)(term.source)
      case TypedAst.Expr.LetIn(sym, isConstant, declaredType, valExpr, body) if sym.id == symbol.id =>
        TypedAst.Expr.LetIn(sym, isConstant, declaredType, substitute(valExpr, symbol, value), body)(term.source)
      case TypedAst.Expr.LetIn(sym, isConstant, declaredType, valExpr, body) =>
        TypedAst.Expr.LetIn(sym, isConstant, declaredType, substitute(valExpr, symbol, value), substitute(body, symbol, value))(term.source)
      case TypedAst.Expr.Pi(dom, cod, isImplicit) =>
        TypedAst.Expr.Pi(substituteInLocalSymbol(dom, symbol, value), substitute(cod, symbol, value), isImplicit)(term.source)
      case TypedAst.Expr.Match(scrutinee, cases) =>
        val newCases = cases.map(c => TypedAst.MatchCase(c.pattern, substitute(c.body, symbol, value))(c.source))
        TypedAst.Expr.Match(substitute(scrutinee, symbol, value), newCases)(term.source)
      case other => other

  private def substituteInLocalSymbol(s: LocalSymbol, symbol: TypedAst.LocalSymbol, value: TypedAst.Expr): LocalSymbol =
    LocalSymbol(s.name, substitute(s.tpe, symbol, value), s.id)

  /** Exposes substitution for tests. */
  private[typing] def substituteForTest(term: TypedAst.Expr, symbol: TypedAst.LocalSymbol, value: TypedAst.Expr): TypedAst.Expr =
    substitute(term, symbol, value)

//  /** Infers an expression in a test context built from a program and exports. */
//  private[typing] def inferInTestContext(
//      program: ast.ProgramFile,
//      expr: ast.Expr,
//      exports: ExportEnv = emptyExportEnv
//    ): CheckResult[(TypedAst.Expr, TypedAst.Expr)] =
//    val globals = buildGlobals(program, exports)
//    given ctx: TypeContext = TypeContext(globals, Map())
//    given metas: MetaContext = MetaStore()
//    given ids: IdSupply = IdSupply()
//    infer(expr)

//  /** Checks an expression in a test context against a named type. */
//  private[typing] def checkInTestContext(
//      program: ast.ProgramFile,
//      expr: ast.Expr,
//      expectedTypeName: String,
//      exports: ProjectSymbolCache
//    ): CheckResult[TypedAst.Expr] =
//    val globals = buildGlobals(program, exports)
//    val expectedType = globals.types.get(expectedTypeName)
//      .map(sym => TypedAst.Expr.Var(sym)(expr.source))
//      .getOrElse(TypedAst.Expr.UnknownType()(expr.source))
//    given ctx: TypeContext = TypeContext(globals, Map())
//    given metas: MetaContext = MetaStore()
//    given ids: IdSupply = IdSupply()
//    check(expr, expectedType)

  // Renders typed expressions in a compact user-facing format for diagnostics.
  private def prettyExpr(expr: TypedAst.Expr): String =
    expr match
      case TypedAst.Expr.UnknownType() => "_"
      case TypedAst.Expr.Sort() => "Type"
      case TypedAst.Expr.Var(symbol) => symbol.name
      case TypedAst.Expr.Lit(ast.Literal.IntLit(_)) => "Int"
      case TypedAst.Expr.Lit(ast.Literal.BoolLit(_)) => "Bool"
      case TypedAst.Expr.Lit(ast.Literal.StringLit(_)) => "String"
      case TypedAst.Expr.Lit(ast.Literal.UnitLit()) => "unit"
      case TypedAst.Expr.Pi(dom, cod, true) => s"(implicit ${dom.name}: ${prettyExpr(dom.tpe)}) -> ${prettyExpr(cod)}"
      case TypedAst.Expr.Pi(dom, cod, false) => s"${dom.name}: ${prettyExpr(dom.tpe)} -> ${prettyExpr(cod)}"
      case TypedAst.Expr.App(callee, arg, _) => s"${prettyExpr(callee)}(${prettyExpr(arg)})"
      case TypedAst.Expr.AppImplicit(callee, arg, _) => s"${prettyExpr(callee)}[${prettyExpr(arg)}]"
      case TypedAst.Expr.Lambda(param, _, _) => s"(\\${param.name} => ...)"
      case TypedAst.Expr.LetIn(symbol, _, _, _, _) => s"(let ${symbol.name} = ...)"
      case TypedAst.Expr.Match(_, _) => "match"
      case TypedAst.Expr.Meta(_, tpe) => s"? : ${prettyExpr(tpe)}"

  /** Determines the type of a literal value. */
  private def literalType(value: ast.Literal, ctx: TypeContext): TypedAst.Expr =
    val typeName = value match
      case ast.Literal.IntLit(_) => "Int"
      case ast.Literal.BoolLit(_) => "Bool"
      case ast.Literal.StringLit(_) => "String"
      case ast.Literal.UnitLit() => "Unit"
    ctx.globals.names.get(typeName)
      .map(sym => TypedAst.Expr.Var(GlobalSymbolSymbol(sym.name, sym.file, sym))(value.source))
      .getOrElse(TypedAst.Expr.UnknownType()(value.source))

  /** Checks a pattern against the expected scrutinee type. */
  private def checkPattern(
      pattern: ast.Pattern,
      expectedType: TypedAst.Expr,
      ctx: TypeContext,
      ids: IdSupply
    ): (TypedAst.Pattern, Map[String, LocalBinding], List[TypeError]) =
    pattern match
      case ast.Pattern.Wildcard() =>
        (TypedAst.Pattern.Wildcard()(pattern.source), Map(), List())
      case ast.Pattern.Lit(value) =>
        val litType = literalType(value, ctx)
        val typed = TypedAst.Pattern.Lit(value)(pattern.source)
        val err = if litType == expectedType || expectedType.isInstanceOf[TypedAst.Expr.UnknownType] then Nil
        else List(TypeError("Pattern literal type mismatch", pattern.source))
        (typed, Map(), err)
      case ast.Pattern.BinderOrCtor0(name) =>
        ctx.globals.names.get(name) match
          case Some(symbol) =>
            // if the symbol exists and is a constructor, match against the constructor
            val (ctorSymbol, ctorErrors) = globalSymbolToCtorSymbol(symbol)
            val typed = TypedAst.Pattern.Ctor(ctorSymbol, Nil)(pattern.source)
            (typed, Map(), ctorErrors)
          case None =>
            // if the symbol does not exist, match against binder
            val symbol = TypedAst.LocalSymbol(name, expectedType, ids.freshLocalId())
            val typed = TypedAst.Pattern.Binder(symbol)(pattern.source)
            (typed, Map(name -> LocalBinding(symbol, None)), List())
      case ast.Pattern.Ctor(name, args) =>
        ctx.globals.names.get(name) match
          case Some(symbol) =>
            val (ctorSymbol, ctorErrors) = globalSymbolToCtorSymbol(symbol)
            val fieldTypes = extractCtorFieldTypes(ctorSymbol.tpe, expectedType)
            val paddedFieldTypes = fieldTypes.padTo(args.length, TypedAst.Expr.UnknownType()(pattern.source))
            val argResults = args.zip(paddedFieldTypes).map { (arg, fieldType) =>
              checkPattern(arg, fieldType, ctx, ids)
            }
            val errors = argResults.flatMap(_._3)
            val bindings = argResults.flatMap(_._2).toMap
            val typedArgs = argResults.map(_._1)
            (TypedAst.Pattern.Ctor(ctorSymbol, typedArgs)(pattern.source), bindings, errors ++ ctorErrors)
          case None =>
            val symbol = TypedAst.CtorSymbol(name, TypedAst.Expr.UnknownType()(pattern.source))
            (TypedAst.Pattern.Ctor(symbol, Nil)(pattern.source), Map(), List(TypeError(s"Unknown constructor ${name}", pattern.source)))

  private def globalSymbolToCtorSymbol(s: GlobalSymbol): (CtorSymbol, List[TypeError]) =
    (CtorSymbol(s.name, Expr.UnknownType()(SourceRange.empty)), List()) // TODO unknown type is wrong

  // Collects explicit field types and result type from a constructor signature.
  private def decomposeCtorType(ctorType: TypedAst.Expr): (List[TypedAst.Expr], TypedAst.Expr) =
    def loop(tpe: TypedAst.Expr, fields: List[TypedAst.Expr]): (List[TypedAst.Expr], TypedAst.Expr) =
      tpe match
        case TypedAst.Expr.Pi(dom, cod, false) =>
          loop(cod, fields :+ dom.tpe)
        case TypedAst.Expr.Pi(_, cod, true) =>
          loop(cod, fields)
        case _ =>
          (fields, tpe)
    loop(ctorType, Nil)

  // Collects parameter symbol ids that appear in a type expression.
  private def collectParamIds(expr: TypedAst.Expr): Set[Int] =
    expr match
      case TypedAst.Expr.Var(param: TypedAst.LocalSymbol) => Set(param.id)
      case TypedAst.Expr.App(callee, arg, tpe) => collectParamIds(callee) ++ collectParamIds(arg) ++ collectParamIds(tpe)
      case TypedAst.Expr.AppImplicit(callee, arg, tpe) => collectParamIds(callee) ++ collectParamIds(arg) ++ collectParamIds(tpe)
      case TypedAst.Expr.Pi(dom, cod, _) => Set(dom.id) ++ collectParamIds(dom.tpe) ++ collectParamIds(cod)
      case TypedAst.Expr.Lambda(_, body, tpe) => collectParamIds(body) ++ collectParamIds(tpe)
      case TypedAst.Expr.LetIn(_, _, declaredType, value, body) => collectParamIds(declaredType) ++ collectParamIds(value) ++ collectParamIds(body)
      case TypedAst.Expr.Match(scrutinee, cases) => collectParamIds(scrutinee) ++ cases.flatMap(c => collectParamIds(c.body)).toSet
      case _ => Set.empty

  // Derives substitutions for constructor type parameters by comparing template and expected result types.
  private def collectTypeParamSubst(
      template: TypedAst.Expr,
      expected: TypedAst.Expr,
      paramIds: Set[Int],
      acc: Map[Int, TypedAst.Expr]
    ): Map[Int, TypedAst.Expr] =
    (template, expected) match
      case (TypedAst.Expr.Var(param: TypedAst.LocalSymbol), other) if paramIds.contains(param.id) =>
        acc.get(param.id) match
          case Some(existing) if existing == other => acc
          case Some(_) => acc
          case None => acc + (param.id -> other)
      case (TypedAst.Expr.AppImplicit(tc, ta, _), TypedAst.Expr.AppImplicit(ec, ea, _)) =>
        val cAcc = collectTypeParamSubst(tc, ec, paramIds, acc)
        collectTypeParamSubst(ta, ea, paramIds, cAcc)
      case (TypedAst.Expr.App(tc, ta, _), TypedAst.Expr.App(ec, ea, _)) =>
        val cAcc = collectTypeParamSubst(tc, ec, paramIds, acc)
        collectTypeParamSubst(ta, ea, paramIds, cAcc)
      case _ =>
        acc

  // Replaces constructor type parameters with inferred concrete types from the scrutinee.
  private def substituteTypeParams(expr: TypedAst.Expr, subst: Map[Int, TypedAst.Expr]): TypedAst.Expr =
    expr match
      case TypedAst.Expr.Var(param: TypedAst.LocalSymbol) =>
        subst.getOrElse(param.id, expr)
      case TypedAst.Expr.App(callee, arg, tpe) =>
        TypedAst.Expr.App(substituteTypeParams(callee, subst), substituteTypeParams(arg, subst), substituteTypeParams(tpe, subst))(expr.source)
      case TypedAst.Expr.AppImplicit(callee, arg, tpe) =>
        TypedAst.Expr.AppImplicit(substituteTypeParams(callee, subst), substituteTypeParams(arg, subst), substituteTypeParams(tpe, subst))(expr.source)
      case TypedAst.Expr.Pi(dom, cod, isImplicit) =>
        TypedAst.Expr.Pi(substituteTypeParamsInLocalSymbol(dom, subst), substituteTypeParams(cod, subst), isImplicit)(expr.source)
      case TypedAst.Expr.Lambda(param, body, tpe) =>
        TypedAst.Expr.Lambda(param, substituteTypeParams(body, subst), substituteTypeParams(tpe, subst))(expr.source)
      case TypedAst.Expr.LetIn(symbol, isConstant, declaredType, value, body) =>
        TypedAst.Expr.LetIn(symbol, isConstant, substituteTypeParams(declaredType, subst), substituteTypeParams(value, subst), substituteTypeParams(body, subst))(expr.source)
      case TypedAst.Expr.Match(scrutinee, cases) =>
        val rewrittenCases = cases.map(c => TypedAst.MatchCase(c.pattern, substituteTypeParams(c.body, subst))(c.source))
        TypedAst.Expr.Match(substituteTypeParams(scrutinee, subst), rewrittenCases)(expr.source)
      case _ => expr

  def substituteTypeParamsInLocalSymbol(s: LocalSymbol, subst: Map[Int, TypedAst.Expr]): LocalSymbol =
    LocalSymbol(s.name, substituteTypeParams(s.tpe, subst), s.id)

  /** Extracts constructor field types from a constructor type. */
  private def extractCtorFieldTypes(ctorType: TypedAst.Expr, expectedType: TypedAst.Expr): List[TypedAst.Expr] =
    val (fieldTypes, resultType) = decomposeCtorType(ctorType)
    val paramIds = collectParamIds(resultType)
    val subst = collectTypeParamSubst(resultType, expectedType, paramIds, Map())
    fieldTypes.map(fieldType => substituteTypeParams(fieldType, subst))

  def formatError(path: String, sourceLines: Vector[String], error: TypeError): String =
    val lineNr = error.source.start.line
    val msgHead = s"Error in $path:${lineNr}:${error.source.start.column}"
    if 0 < lineNr && lineNr <= sourceLines.length then
      val errorLine = sourceLines(lineNr-1)
      val lineNrString = lineNr.toString()
      val startColumn = error.source.start.column
      val endColumn = if error.source.end.line == lineNr then startColumn + 1 else error.source.end.column
      s"""|$msgHead
          |$lineNrString | ${errorLine}
          |${" " * (lineNrString.length + endColumn)}${"^" * (endColumn - startColumn)}
          |${error.message}
      """.stripMargin('|')
    else
      s"$msgHead: ${error.message}"
