package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo
import com.github.peterzeller.minifumo.{ast, typing}
import com.github.peterzeller.minifumo.ast.SourceRange
import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.common.MinifumoError
import com.github.peterzeller.minifumo.typing.TypedAst.*
import com.github.peterzeller.minifumo.typing.TypedAst.Expr.{Sort, UnknownType}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object TypeChecker:
  private val throwOnError = false
  private val defaultConstraintFuel = 128

  final case class TypeError(message: String, source: ast.SourceRange) extends MinifumoError:
    if throwOnError then
      throw new RuntimeException(s"Constructed type error $message at line ${source.start.line}")


  /** Type-checks a program */
  def checkProgram(path: String, program: ast.ProgramFile, globalNames: NameCache & SymbolCache, importStandard: Boolean, idSupply: TypeChecker.IdSupply): (TypedAst.Program, List[TypeError]) =
    try
      val errors = ListBuffer[TypeError]()
      var (symbolMap, importErrors) = GlobalSymbols.buildGlobalSymbols(path, program, globalNames, false, idSupply)
      if importStandard then
        // import the standard library symbols into the program file scope
        val (standardLibSymbolMap, standardLibImportErrors) = GlobalSymbols.buildGlobalSymbols("standard.minifumo", Standard.standardProgram, globalNames, false, idSupply)
        symbolMap ++= standardLibSymbolMap
        importErrors ++= standardLibImportErrors
      errors.addAll(importErrors)
      val globals = GlobalEnv(names = symbolMap)
      val typedItems = ListBuffer[TypedAst.TopLevel]()
      for item <- program.items do
        val itemMetaStore = MetaStore()
        val typedItem = item match
          case decl: ast.TopLevel.DataDecl =>
            val ctorReturnTypeErrors = validateCtorReturnTypes(decl, globals, idSupply, itemMetaStore)
            errors.addAll(ctorReturnTypeErrors)
            val sym = globals.names.get(decl.name) match {
              case Some(d: DatatypeSymbol) =>
                d
              case _ =>
                // TODO can this happen?
                ???
            }
            val res = sym.typed
            errors.addAll(sym.allErrors)
            res
          case decl: ast.TopLevel.FunDecl =>
            TypeContext(globals, Map())
            val sym = symbolMap(decl.sig.name) match {
              case f: FunctionSymbol => f
              case other =>
                errors.addOne(TypeError(s"Element ${other.name} is not a function", decl.source))
                ErrorSymbols.fun(other.name)
            }
            val res = sym.typedDecl.get
            errors.addAll(sym.allErrors)
            res
        typedItems.addOne(typedItem)
      // Returns accumulated type errors to the caller instead of throwing.
      (TypedAst.Program(typedItems.toList)(program.source), errors.toList)
    catch
      case e: Exception =>
        throw new RuntimeException(s"error checking $path", e)


  private[typing] def checkFunSig(sym: FunctionSymbol, sig: ast.FunSig)(implicit ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.FunSig, TypeContext,  List[TypeError]) = {
//    throw new RuntimeException(s"Checking of fun sig $sig is not yet implemented")
    var mEnv = ctx
    val errors = mutable.ListBuffer[TypeError]()
    val typedImplicitParams = mutable.ListBuffer[LocalSymbol]()
    val typedParams = mutable.ListBuffer[LocalSymbol]()
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

    (TypedAst.FunSig(sym, typedImplicitParams.toList, typedParams.toList, returnType), mEnv, errors.toList)
  }

  /** Provides a lookup interface for local and global symbols during type checking. */
  trait Context:
    def lookupSymbol(name: String): Option[Symbol]
    def lookupValue(symbol: TermSymbol): Option[TypedAst.Expr]
    def lookupDefinition(symbol: Symbol): Option[TypedAst.Expr]

  /** Provides a mutable store for meta-variable assignments. */
  trait MetaContext:
    def assign(metaId: Int, term: TypedAst.Expr): Unit
    def getAssignment(metaId: Int): Option[TypedAst.Expr]
    def addEqualityConstraint(constraint: EqualityConstraint): Unit
    def equalityConstraints: List[EqualityConstraint]

  /** Represents a binding in the local context. */
  private[typing] final case class LocalBinding(symbol: TermSymbol, value: Option[TypedAst.Expr])

  /** Groups the result of type-checking one pattern node. */
  private[typing] final case class PatternCheckResult(
      typedPattern: TypedAst.Pattern,
      bindings: Map[String, LocalBinding],
      refinements: Map[Int, TypedAst.Expr],
      errors: List[TypeError]
    )

  /** Stores global symbols for type checking. */
  final case class GlobalEnv(
      names: Map[String, GlobalSymbol]
    )

  /** Implements a context with local bindings and global symbols. */
  private[typing] final case class TypeContext(globals: GlobalEnv, locals: Map[String, LocalBinding]) extends Context:
    override def lookupSymbol(name: String): Option[Symbol] = {
      locals.get(name).map(_.symbol)
        .orElse(globals.names.get(name))
    }

    override def lookupValue(symbol: TermSymbol): Option[TypedAst.Expr] =
      locals.values.find(_.symbol == symbol).flatMap(_.value)

    /** Resolves unfoldable global definitions on demand. */
    override def lookupDefinition(symbol: Symbol): Option[TypedAst.Expr] =
      symbol match {
        case symbol: TermSymbol =>
          symbol match {
            case LocalSymbol(name, tpe, id) =>
              // TODO should we be able to unfold definition of local variable?
              None
            case BuiltinValueSymbol(name, tpe) =>
              None
          }
        case ErrorSymbol(name, tpe) =>
          None
        case DatatypeSymbol(_, _) =>
          None
        case f: FunctionSymbol =>
          f.typedBody
        case CtorSymbol(_, _) =>
          None
      }

    /** Adds a local binding to the context. */
    def withLocal(symbol: TermSymbol, value: Option[TypedAst.Expr] = None): TypeContext =
      copy(locals = locals + (symbol.name -> LocalBinding(symbol, value)))

    /** Adds multiple local bindings to the context. */
    def withLocals(symbols: List[TermSymbol]): TypeContext =
      symbols.foldLeft(this) { (ctx, symbol) => ctx.withLocal(symbol) }

  /** Stores meta-variable assignments during unification. */
  private[typing] final case class MetaStore(assignments: mutable.Map[Int, TypedAst.Expr] = mutable.Map()) extends MetaContext:
    private val constraints: ListBuffer[EqualityConstraint] = ListBuffer.empty

    override def assign(metaId: Int, term: TypedAst.Expr): Unit =
      assignments.update(metaId, term)

    override def getAssignment(metaId: Int): Option[TypedAst.Expr] =
      assignments.get(metaId)

    /** Appends a new deferred equality constraint to the store. */
    override def addEqualityConstraint(constraint: EqualityConstraint): Unit =
      constraints.addOne(constraint)

    /** Returns the currently tracked deferred equality constraints. */
    override def equalityConstraints: List[EqualityConstraint] =
      constraints.toList

  /** Tracks identifier allocation for local symbols and metas. */
  final case class IdSupply(var nextId: Int = 0, var nextMeta: Int = 0):
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


  /** Builds a typed data declaration. */
  private[typing] def buildDataDecl(
      decl: ast.TopLevel.DataDecl,
      globals: GlobalEnv): TypedAst.TopLevel.DataDecl = {
    globals.names.get(decl.name) match {
      case Some(d: DatatypeSymbol) =>
        d.typed
      case _ =>
        // TODO can this happen?
        ???
    }
  }

  


  /** Validates that explicit constructor return types match the declared data type. */
  private def validateCtorReturnTypes(
      decl: ast.TopLevel.DataDecl,
      globals: GlobalEnv,
      idSupply: IdSupply,
      metas: MetaContext
    ): List[TypeError] =
    val errors = ListBuffer[TypeError]()
    var localCtx = TypeContext(globals, Map())
    val localTypeParams = ListBuffer[LocalSymbol]()

    for param <- decl.implicitParams do
      val (paramType, paramErrors) = check(param.tpe, Sort()(SourceRange.empty))(using localCtx, metas, idSupply)
      errors.addAll(paramErrors)
      val localSymbol = LocalSymbol(param.name, paramType, idSupply.freshLocalId())
      localTypeParams.addOne(localSymbol)
      localCtx = localCtx.withLocal(localSymbol)

    for ctor <- decl.ctors do
      ctor.returnType.foreach { returnType =>
        val (typedReturnType, returnTypeErrors) = check(returnType, Sort()(SourceRange.empty))(using localCtx, metas, idSupply)
        errors.addAll(returnTypeErrors)
        if returnTypeErrors.isEmpty && !hasDatatypeHead(typedReturnType, decl.name)(using localCtx, metas) then
          val message = s"Constructor ${ctor.name} must return ${decl.name} but got ${prettyExpr(typedReturnType)}"
          errors.addOne(TypeError(message, returnType.source))
      }

    errors.toList


  /** Checks whether an expression has the given datatype name at its application head. */
  private def hasDatatypeHead(expr: TypedAst.Expr, datatypeName: String)(using ctx: TypeContext, metas: MetaContext): Boolean =
    def loop(current: TypedAst.Expr): Boolean =
      whnf(current) match
        case TypedAst.Expr.App(callee, _, _) => loop(callee)
        case TypedAst.Expr.AppImplicit(callee, _, _) => loop(callee)
        case TypedAst.Expr.Var(symbol) => symbol.name == datatypeName
        case _ => false

    loop(expr)

  /** Type-checks a function body against its return type. */
  

  /** Resolves metas at the end of a declaration and reports unresolved placeholders. */
  private[typing] def finalizeTopLevelExpr(expr: TypedAst.Expr)
                               (implicit ctx: Context, metas: MetaContext): (TypedAst.Expr, List[TypeError]) =
    val unresolvedConstraints = solveOpenConstraints(defaultConstraintFuel)
    val unresolved = collectUnresolvedMetas(expr)
    val metaErrors = unresolved.toList.map(meta => TypeError(s"Could not infer implicit argument ${prettyExpr(meta)}", meta.source))
    val constraintErrors = unresolvedConstraints.map { case (constraint, reducedLeft, reducedRight) =>
      val message =
        s"Could not solve equality constraint ${prettyEqConstraint(constraint.left, constraint.right)}\n" +
          s"Reduced left: ${prettyExpr(reducedLeft)}\n" +
          s"Reduced right: ${prettyExpr(reducedRight)}"
      TypeError(message, constraint.source)
    }.distinctBy(err => (err.source.start.line, err.message))
    (instantiate(expr), metaErrors ++ constraintErrors)

  /** Translates a signature expression to a typed expression. */
  private[typing] def signatureExpr(expr: ast.Expr, globals: GlobalEnv, locals: Map[String, TermSymbol])(implicit ids: TypeChecker.IdSupply): TypedAst.Expr = {
    // TODO can't we just use the normal expression typing method?
    expr match
      case ast.Expr.Lit(value) => TypedAst.Expr.Lit(value)(expr.source)
      case ast.Expr.Var(name) =>
        if name == "unit" then
          TypedAst.Expr.UnknownType()(expr.source)
        else if name == "Type" then
          TypedAst.Expr.Sort()(expr.source)
        else
          locals.get(name) match
            case Some(symbol) =>
              TypedAst.Expr.Var(symbol)(expr.source)
            case None =>
              globals.names.get(name) match
                case Some(symbol) =>
                  TypedAst.Expr.Var(symbol)(expr.source)
                case None =>
                  TypedAst.Expr.Var(ErrorSymbol(name, TypedAst.Expr.UnknownType()(expr.source)))(expr.source)
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
        val domSym = LocalSymbol(param.name, dom, ids.freshLocalId())
        val cod = signatureExpr(body, globals, locals + (param.name -> domSym))
        TypedAst.Expr.Pi(domSym, cod, isImplicit = false)(expr.source)
      case ast.Expr.Hole() =>
        TypedAst.Expr.UnknownType()(expr.source)
      case _ =>
        TypedAst.Expr.UnknownType()(expr.source)
  }


  /** Checks if two types are definitionally equal, solving metas as needed. 
  */
  def isDefEq(t1: TypedAst.Expr, t2: TypedAst.Expr, source: SourceRange)
             (implicit ctx: Context, metas: MetaContext): Boolean =
    val norm1 = whnf(t1)
    val norm2 = whnf(t2)
    (norm1, norm2) match
      case (TypedAst.Expr.Meta(id, _), other) =>
        solveMeta(id, other)
      case (other, TypedAst.Expr.Meta(id, _)) =>
        solveMeta(id, other)
      case _ =>
        (extractHeadedExpr(norm1), extractHeadedExpr(norm2)) match
          case (Some(h1), Some(h2)) if h1.kind == h2.kind =>
            if symbolsEqual(h1.head, h2.head) then
              h1.args.length == h2.args.length && h1.args.zip(h2.args).forall((a, b) => isDefEq(a, b, source))
            else
              false
          case (Some(_), Some(_)) =>
            false
          case _ if syntacticallyEquivalent(norm1, norm2) =>
            true
          case _ =>
            metas.addEqualityConstraint(EqualityConstraint(t1, t2, source))
            true

  private def symbolsEqual(a: Symbol, b: Symbol): Boolean =
    a == b

  /** Renders a deferred equality constraint as an Eq(a, b) datatype application. */
  private def prettyEqConstraint(left: TypedAst.Expr, right: TypedAst.Expr): String =
    s"Eq(${prettyExpr(left)}, ${prettyExpr(right)})"

  /** Checks whether two reduced terms are structurally equivalent. */
  private def syntacticallyEquivalent(left: TypedAst.Expr, right: TypedAst.Expr): Boolean =
    (left, right) match
      case (TypedAst.Expr.UnknownType(), _) => true
      case (_, TypedAst.Expr.UnknownType()) => true
      case (TypedAst.Expr.Var(s1), TypedAst.Expr.Var(s2)) if symbolsEqual(s1, s2) => true
      case (TypedAst.Expr.Var(p1: LocalSymbol), TypedAst.Expr.Var(p2: LocalSymbol)) if p1.name == p2.name => true
      case (TypedAst.Expr.Lit(v1), TypedAst.Expr.Lit(v2)) => v1 == v2
      case (TypedAst.Expr.Sort(), TypedAst.Expr.Sort()) => true
      case (TypedAst.Expr.App(c1, a1, _), TypedAst.Expr.App(c2, a2, _)) => syntacticallyEquivalent(c1, c2) && syntacticallyEquivalent(a1, a2)
      case (TypedAst.Expr.AppImplicit(c1, a1, _), TypedAst.Expr.AppImplicit(c2, a2, _)) => syntacticallyEquivalent(c1, c2) && syntacticallyEquivalent(a1, a2)
      case (TypedAst.Expr.Lambda(p1, b1, _), TypedAst.Expr.Lambda(p2, b2, _)) =>
        val alignedBody = substitute(b2, p2, TypedAst.Expr.Var(p1)(b2.source))
        syntacticallyEquivalent(p1.tpe, p2.tpe) && syntacticallyEquivalent(b1, alignedBody)
      case (TypedAst.Expr.Pi(d1, c1, i1), TypedAst.Expr.Pi(d2, c2, i2)) if i1 == i2 =>
        val alignedCodomain = substitute(c2, d2, TypedAst.Expr.Var(d1)(c2.source))
        syntacticallyEquivalent(d1.tpe, d2.tpe) && syntacticallyEquivalent(c1, alignedCodomain)
      case (TypedAst.Expr.Meta(i1, _), TypedAst.Expr.Meta(i2, _)) => i1 == i2
      case _ => false

  /** Extracts constructor-like or datatype application heads from a term. */
  private def extractHeadedExpr(expr: TypedAst.Expr): Option[HeadedExpr] =
    val (head, args) = decomposeApplication(expr)
    head match
      case TypedAst.Expr.Var(symbol) =>
        classifyHead(symbol).map(kind => HeadedExpr(symbol, kind, args))
      case _ =>
        None

  /** Decomposes nested applications into head and ordered argument list. */
  private def decomposeApplication(expr: TypedAst.Expr): (TypedAst.Expr, List[TypedAst.Expr]) =
    def loop(current: TypedAst.Expr, acc: List[TypedAst.Expr]): (TypedAst.Expr, List[TypedAst.Expr]) =
      current match
        case TypedAst.Expr.App(callee, arg, _) => loop(callee, arg :: acc)
        case TypedAst.Expr.AppImplicit(callee, arg, _) => loop(callee, arg :: acc)
        case _ => (current, acc)
    loop(expr, Nil)

  /** Classifies heads that represent constructors or type constructors. */
  private def classifyHead(symbol: Symbol): Option[DefEqHeadKind] =
    symbol match
      case _: CtorSymbol => Some(DefEqHeadKind.Constructor)
      case _: DatatypeSymbol => Some(DefEqHeadKind.TypeConstructor)
      case _ => None

  /** Tries to solve deferred equality constraints by normalizing both sides. */
  private def solveOpenConstraints(fuel: Int)(implicit ctx: Context, metas: MetaContext): List[(EqualityConstraint, TypedAst.Expr, TypedAst.Expr)] =
    var pending = metas.equalityConstraints
    var changed = true
    while changed do
      changed = false
      val nextPending = ListBuffer[(EqualityConstraint, TypedAst.Expr, TypedAst.Expr)]()
      for constraint <- pending do
        trySolveConstraint(constraint, fuel) match
          case None =>
            changed = true
          case Some(unresolved) =>
            nextPending.addOne(unresolved)
      pending = nextPending.toList.map(_._1)
      if !changed then
        return nextPending.toList
    List.empty

  /** Attempts to solve one constraint by reducing it and assigning metas when possible. */
  private def trySolveConstraint(constraint: EqualityConstraint, fuel: Int)
                                (implicit ctx: Context, metas: MetaContext): Option[(EqualityConstraint, TypedAst.Expr, TypedAst.Expr)] =
    val reducedLeft = reduceExpr(constraint.left, fuel)
    val reducedRight = reduceExpr(constraint.right, fuel)
    if canSolveByUnification(reducedLeft, reducedRight) then
      None
    else
      Some((constraint, reducedLeft, reducedRight))

  /** Solves equality by recursively assigning metas and matching term structure. */
  private def canSolveByUnification(left: TypedAst.Expr, right: TypedAst.Expr)
                                   (implicit metas: MetaContext): Boolean =
    if syntacticallyEquivalent(left, right) then
      true
    else
      (left, right) match
        case (TypedAst.Expr.Meta(id, _), other) => solveMeta(id, other)
        case (other, TypedAst.Expr.Meta(id, _)) => solveMeta(id, other)
        case (TypedAst.Expr.App(c1, a1, _), TypedAst.Expr.App(c2, a2, _)) =>
          canSolveByUnification(c1, c2) && canSolveByUnification(a1, a2)
        case (TypedAst.Expr.AppImplicit(c1, a1, _), TypedAst.Expr.AppImplicit(c2, a2, _)) =>
          canSolveByUnification(c1, c2) && canSolveByUnification(a1, a2)
        case (TypedAst.Expr.Pi(d1, c1, i1), TypedAst.Expr.Pi(d2, c2, i2)) if i1 == i2 =>
          val alignedCodomain = substitute(c2, d2, TypedAst.Expr.Var(d1)(c2.source))
          canSolveByUnification(d1.tpe, d2.tpe) && canSolveByUnification(c1, alignedCodomain)
        case (TypedAst.Expr.Lambda(p1, b1, _), TypedAst.Expr.Lambda(p2, b2, _)) =>
          val alignedBody = substitute(b2, p2, TypedAst.Expr.Var(p1)(b2.source))
          canSolveByUnification(p1.tpe, p2.tpe) && canSolveByUnification(b1, alignedBody)
        case _ => false

  /** Fully reduces a term with a fuel budget to keep normalization bounded. */
  private def reduceExpr(term: TypedAst.Expr, fuel: Int)(implicit ctx: Context, metas: MetaContext): TypedAst.Expr =
    if fuel <= 0 then
      instantiate(term)
    else
      instantiate(term) match
        case TypedAst.Expr.App(callee, arg, tpe) =>
          val reducedCallee = reduceExpr(callee, fuel - 1)
          val reducedArg = reduceExpr(arg, fuel - 1)
          val unfoldedCallee = unfoldAppliedCallee(reducedCallee, reducedArg, fuel - 1)
          unfoldedCallee match
            case TypedAst.Expr.Lambda(param, body, _) =>
              reduceExpr(substitute(body, param, reducedArg), fuel - 1)
            case otherCallee =>
              TypedAst.Expr.App(otherCallee, reducedArg, reduceExpr(tpe, fuel - 1))(term.source)
        case TypedAst.Expr.AppImplicit(callee, arg, tpe) =>
          val reducedCallee = reduceExpr(callee, fuel - 1)
          val reducedArg = reduceExpr(arg, fuel - 1)
          val unfoldedCallee = unfoldAppliedCallee(reducedCallee, reducedArg, fuel - 1)
          unfoldedCallee match
            case TypedAst.Expr.Lambda(param, body, _) =>
              reduceExpr(substitute(body, param, reducedArg), fuel - 1)
            case otherCallee =>
              TypedAst.Expr.AppImplicit(otherCallee, reducedArg, reduceExpr(tpe, fuel - 1))(term.source)
        case TypedAst.Expr.LetIn(symbol, isConstant, declaredType, value, body) =>
          val reducedValue = reduceExpr(value, fuel - 1)
          reduceExpr(substitute(body, symbol, reducedValue), fuel - 1)
        case TypedAst.Expr.Match(scrutinee, _, cases) =>
          val reducedScrutinee = reduceExpr(scrutinee, fuel - 1)
          reduceMatchExpr(reducedScrutinee, cases, term.source, fuel - 1)
        case TypedAst.Expr.Lambda(param, body, tpe) =>
          TypedAst.Expr.Lambda(param, reduceExpr(body, fuel - 1), reduceExpr(tpe, fuel - 1))(term.source)
        case TypedAst.Expr.Pi(dom, cod, isImplicit) =>
          TypedAst.Expr.Pi(instantiateLocalSymbol(dom), reduceExpr(cod, fuel - 1), isImplicit)(term.source)
        case other =>
          other

  /** Unfolds a callee when it appears in application position and fuel is available. */
  /** Unfolds an applied callee when reduction is likely to make progress. */
  private def unfoldAppliedCallee(callee: TypedAst.Expr, appliedArg: TypedAst.Expr, fuel: Int)(implicit ctx: Context, metas: MetaContext): TypedAst.Expr =
    if fuel <= 0 then
      callee
    else
      callee match
        case TypedAst.Expr.Var(symbol: TermSymbol) =>
          ctx.lookupValue(symbol).map(value => reduceExpr(value, fuel - 1)).getOrElse(callee)
        case TypedAst.Expr.Var(symbol) =>
          ctx.lookupDefinition(symbol) match
            case Some(value) if shouldDeferUnfold(value, appliedArg) =>
              callee
            case Some(value) =>
              reduceExpr(value, fuel - 1)
            case None =>
              callee
        case _ =>
          callee

  /** Checks whether unfolding would get stuck on a non-constructor match scrutinee. */
  private def shouldDeferUnfold(definition: TypedAst.Expr, appliedArg: TypedAst.Expr): Boolean =
    definition match
      case TypedAst.Expr.Lambda(param, body, _) =>
        val inspectsParam = hasMatchOnParam(body, param.id)
        inspectsParam && !hasConstructorHead(appliedArg)
      case _ =>
        false

  /** Checks whether a body contains a match that scrutinizes the given parameter id. */
  private def hasMatchOnParam(body: TypedAst.Expr, paramId: Int): Boolean =
    body match
      case TypedAst.Expr.Match(TypedAst.Expr.Var(local: LocalSymbol), _, _) if local.id == paramId =>
        true
      case TypedAst.Expr.Lambda(_, nestedBody, _) =>
        hasMatchOnParam(nestedBody, paramId)
      case TypedAst.Expr.LetIn(_, _, _, _, nestedBody) =>
        hasMatchOnParam(nestedBody, paramId)
      case _ =>
        false

  /** Checks whether an expression has a constructor at its application head. */
  private def hasConstructorHead(expr: TypedAst.Expr): Boolean =
    decomposeApplication(expr)._1 match
      case TypedAst.Expr.Var(symbol) =>
        classifyHead(symbol).contains(DefEqHeadKind.Constructor)
      case _ =>
        false


  /** Reduces a match expression when the scrutinee head is a constructor. */
  private def reduceMatchExpr(scrutinee: TypedAst.Expr, cases: List[TypedAst.MatchCase], source: SourceRange, fuel: Int)
                            (implicit ctx: Context, metas: MetaContext): TypedAst.Expr =
    selectMatchCase(scrutinee, cases) match
      case Some(body) => reduceExpr(body, fuel)
      case None =>
        val reducedCases = cases.map(c => TypedAst.MatchCase(c.pattern, reduceExpr(c.body, fuel))(c.source))
        TypedAst.Expr.Match(scrutinee, TypedAst.Expr.UnknownType()(source), reducedCases)(source)

  /** Selects and specializes the first matching branch for a reduced scrutinee. */
  private def selectMatchCase(scrutinee: TypedAst.Expr, cases: List[TypedAst.MatchCase]): Option[TypedAst.Expr] =
    val (head, args) = decomposeApplication(scrutinee)
    head match
      case TypedAst.Expr.Var(symbol) if hasConstructorLikeHead(symbol, cases) =>
        cases.collectFirst(Function.unlift { c =>
          matchPattern(c.pattern, scrutinee, args).map(bindings => applyBindings(c.body, bindings))
        })
      case _ =>
        None

  /** Checks whether a scrutinee symbol can participate in constructor-pattern reduction. */
  private def hasConstructorLikeHead(symbol: Symbol, cases: List[TypedAst.MatchCase]): Boolean =
    classifyHead(symbol).contains(DefEqHeadKind.Constructor) ||
      cases.exists {
        case TypedAst.MatchCase(TypedAst.Pattern.Ctor(ctorSymbol, _), _) => symbolsEqual(symbol, ctorSymbol)
        case _ => false
      }

  /** Matches one typed pattern against a constructor-reduced scrutinee. */
  private def matchPattern(pattern: TypedAst.Pattern, scrutinee: TypedAst.Expr, constructorArgs: List[TypedAst.Expr]): Option[Map[LocalSymbol, TypedAst.Expr]] =
    pattern match
      case TypedAst.Pattern.Wildcard() => Some(Map.empty)
      case TypedAst.Pattern.Binder(symbol) => Some(Map(symbol -> scrutinee))
      case TypedAst.Pattern.Lit(value) =>
        scrutinee match
          case TypedAst.Expr.Lit(actual) if actual == value => Some(Map.empty)
          case _ => None
      case TypedAst.Pattern.Ctor(symbol, args) =>
        val (head, scrutineeArgs) = decomposeApplication(scrutinee)
        head match
          case TypedAst.Expr.Var(headSymbol) if symbolsEqual(headSymbol, symbol) && args.length == scrutineeArgs.length =>
            args.zip(scrutineeArgs).foldLeft(Option(Map.empty[LocalSymbol, TypedAst.Expr])) {
              case (Some(acc), (argPattern, argValue)) =>
                matchPattern(argPattern, argValue, constructorArgs).map(acc ++ _)
              case (None, _) => None
            }
          case _ => None

  /** Applies pattern bindings by substituting each binder with the matched value. */
  private def applyBindings(body: TypedAst.Expr, bindings: Map[LocalSymbol, TypedAst.Expr]): TypedAst.Expr =
    bindings.foldLeft(body) { case (current, (symbol, value)) => substitute(current, symbol, value) }

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
      case TypedAst.Expr.Match(scrutinee, motive, cases) =>
        val newCases = cases.map(c => TypedAst.MatchCase(c.pattern, instantiate(c.body))(c.source))
        TypedAst.Expr.Match(instantiate(scrutinee), instantiate(motive), newCases)(term.source)
      case other => other

  def instantiateLocalSymbol(s: LocalSymbol)(implicit metas: MetaContext): LocalSymbol =
    LocalSymbol(s.name, instantiate(s.tpe), s.id)

  /** Infers the type of an expression, producing a typed expression alongside its type. */
  private[typing] def infer(expr: ast.Expr)
                   (implicit ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    TypeCheckerExprDispatcher.infer(expr)


  /** Checks an expression against an expected type. */
  private[typing] def check(expr: ast.Expr, expectedType: TypedAst.Expr)
                   (implicit ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, List[TypeError]) =
    TypeCheckerExprDispatcher.check(expr, expectedType)

  /** Infers and elaborates an expression by inserting missing implicit arguments. */
  private[typing] def inferAndElaborate(expr: ast.Expr)
                               (implicit ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr, List[TypeError]) =
    val (typedExpr, inferredType, errs) = infer(expr)
    val (elaboratedExpr, elaboratedType) = insertImplicitArgs(typedExpr, inferredType, expr.source)
    (elaboratedExpr, elaboratedType, errs)

  /** Checks and elaborates an expression against an expected type. */
  private[typing] def checkAndElaborate(expr: ast.Expr, expectedType: TypedAst.Expr)
                               (implicit ctx: TypeContext, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, List[TypeError]) =
    val expectedNorm = whnf(expectedType)
    val (inferredExpr, inferredType, errs) = inferAndElaborate(expr)
    val (adaptedExpr, adaptedType, adaptedErrs) =
      if expectedNorm.isInstanceOf[TypedAst.Expr.Sort] && hasDatatypeHead(inferredType, "Bool") then
        inferAndElaborate(wrapBoolAsEqTrue(expr))
      else
        (inferredExpr, inferredType, errs)
    val errs2 =
      if isDefEq(adaptedType, expectedNorm, expr.source) then adaptedErrs
      else TypeError(s"Expected ${prettyExpr(expectedNorm)} but got ${prettyExpr(adaptedType)}\nIn expression ${prettyExpr(adaptedExpr)}", expr.source) :: adaptedErrs
    (adaptedExpr, errs2)

  /** Rewrites a proposition `p` into the type expression `Eq(p, True)`. */
  private def wrapBoolAsEqTrue(expr: ast.Expr): ast.Expr =
    val eqRef = ast.Expr.Var("Eq")(expr.source)
    val trueRef = ast.Expr.Var("True")(expr.source)
    ast.Expr.Call(ast.Expr.Call(eqRef, expr)(expr.source), trueRef)(expr.source)

  /** Inserts synthetic implicit arguments and placeholder metas before explicit arguments. */
  private def insertImplicitArgs(
      callee: TypedAst.Expr,
      calleeType: TypedAst.Expr,
      source: ast.SourceRange
    )(implicit ctx: Context, metas: MetaContext, ids: IdSupply): (TypedAst.Expr, TypedAst.Expr) =
    var currentExpr = callee
    var currentType = calleeType
    var keepGoing = true
    while keepGoing do
      whnf(currentType) match
        case TypedAst.Expr.Pi(dom, cod, true) =>
          val meta = searchImplicitArgument(dom.tpe, source).getOrElse(freshMeta(dom.name, dom.tpe, source))
          currentExpr = TypedAst.Expr.AppImplicit(currentExpr, meta, substitute(cod, dom, meta))(source)
          currentType = substitute(cod, dom, meta)
        case _ =>
          keepGoing = false
    (currentExpr, currentType)

  /** Placeholder hook for future typeclass-style implicit search. */
  private def searchImplicitArgument(expectedType: TypedAst.Expr, source: SourceRange)
                                    (implicit ctx: Context): Option[TypedAst.Expr] =
    TypeCheckerMetas.searchImplicitArgument(expectedType, source)

  /** Creates a fresh meta-variable expression. */
  private[typing] def freshMeta(name: String, tpe: TypedAst.Expr, source: ast.SourceRange)(implicit ids: IdSupply): TypedAst.Expr =
    TypeCheckerMetas.freshMeta(name, tpe, source)

  /** Checks whether a meta-variable can be solved with a term. */
  private def solveMeta(metaId: Int, term: TypedAst.Expr)
                       (implicit metas: MetaContext): Boolean =
    TypeCheckerMetas.solveMeta(metaId, term)

  /** Checks whether a meta-variable can be solved with a term. */
  

  /** Performs an occurs check for a meta-variable inside a term. */
  

  /** Collects unresolved metavariable ids from a typed expression. */
  private def collectUnresolvedMetas(term: TypedAst.Expr)(implicit metas: MetaContext): Set[TypedAst.Expr.Meta] =
    instantiate(term) match
      case m@TypedAst.Expr.Meta(id, _) if metas.getAssignment(id).isEmpty => Set(m)
      case TypedAst.Expr.Meta(_, _) => Set.empty
      case TypedAst.Expr.App(callee, arg, tpe) =>
        collectUnresolvedMetas(callee) ++ collectUnresolvedMetas(arg) ++ collectUnresolvedMetas(tpe)
      case TypedAst.Expr.AppImplicit(callee, arg, tpe) =>
        collectUnresolvedMetas(callee) ++ collectUnresolvedMetas(arg) ++ collectUnresolvedMetas(tpe)
      case TypedAst.Expr.Lambda(param, body, tpe) =>
        collectUnresolvedMetas(param.tpe) ++ collectUnresolvedMetas(body) ++ collectUnresolvedMetas(tpe)
      case TypedAst.Expr.LetIn(symbol, _, declaredType, value, body) =>
        collectUnresolvedMetas(symbol.tpe) ++ collectUnresolvedMetas(declaredType) ++ collectUnresolvedMetas(value) ++ collectUnresolvedMetas(body)
      case TypedAst.Expr.Pi(dom, cod, _) =>
        collectUnresolvedMetas(dom.tpe) ++ collectUnresolvedMetas(cod)
      case TypedAst.Expr.Match(scrutinee, motive, cases) =>
        collectUnresolvedMetas(scrutinee) ++ collectUnresolvedMetas(motive) ++ cases.flatMap(c => collectUnresolvedMetas(c.body)).toSet
      case _ => Set.empty

  /** Substitutes a local symbol with a value in a term. */
  private[typing] def substitute(term: TypedAst.Expr, symbol: LocalSymbol, value: TypedAst.Expr): TypedAst.Expr =
    term match
      case TypedAst.Expr.Var(sym: LocalSymbol) if sym.id == symbol.id => value
      case TypedAst.Expr.App(callee, arg, tpe) =>
        TypedAst.Expr.App(substitute(callee, symbol, value), substitute(arg, symbol, value), tpe)(term.source)
      case TypedAst.Expr.AppImplicit(callee, arg, tpe) =>
        TypedAst.Expr.AppImplicit(substitute(callee, symbol, value), substitute(arg, symbol, value), tpe)(term.source)
      case TypedAst.Expr.Lambda(param, body, tpe) =>
        TypedAst.Expr.Lambda(param, substitute(body, symbol, value), tpe)(term.source)
      case TypedAst.Expr.LetIn(sym, isConstant, declaredType, valExpr, body) =>
        TypedAst.Expr.LetIn(sym, isConstant, declaredType, substitute(valExpr, symbol, value), substitute(body, symbol, value))(term.source)
      case TypedAst.Expr.Pi(dom, cod, isImplicit) =>
        TypedAst.Expr.Pi(substituteInLocalSymbol(dom, symbol, value), substitute(cod, symbol, value), isImplicit)(term.source)
      case TypedAst.Expr.Match(scrutinee, motive, cases) =>
        val newCases = cases.map(c => TypedAst.MatchCase(c.pattern, substitute(c.body, symbol, value))(c.source))
        TypedAst.Expr.Match(substitute(scrutinee, symbol, value), substitute(motive, symbol, value), newCases)(term.source)
      case TypedAst.Expr.Lit(_) | TypedAst.Expr.Var(_) | TypedAst.Expr.Sort() | TypedAst.Expr.Meta(_, _) | TypedAst.Expr.UnknownType() =>
        term

  private def substituteInLocalSymbol(s: LocalSymbol, symbol: LocalSymbol, value: TypedAst.Expr): LocalSymbol =
    LocalSymbol(s.name, substitute(s.tpe, symbol, value), s.id)

  /** Exposes substitution for tests. */
  def substituteForTest(term: TypedAst.Expr, symbol: LocalSymbol, value: TypedAst.Expr): TypedAst.Expr =
    substitute(term, symbol, value)

  /** Exposes bounded reduction for tests. */
  def reduceExprForTest(term: TypedAst.Expr, fuel: Int)(using ctx: Context, metas: MetaContext): TypedAst.Expr =
    reduceExpr(term, fuel)


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

  private[typing] def collectReferencedSymbols(expr: TypedAst.Expr): Set[Symbol] =
    expr match {
      case Expr.Lit(value) => Set()
      case Expr.Var(symbol) => Set(symbol)
      case Expr.AppImplicit(callee, arg, tpe) =>
        collectReferencedSymbols(callee) ++ collectReferencedSymbols(arg)
      case Expr.App(callee, arg, tpe) =>
        collectReferencedSymbols(callee) ++ collectReferencedSymbols(arg)
      case Expr.Pi(dom, cod, isImplicit) =>
        collectReferencedSymbols(cod) -- Set(dom)
      case Expr.Sort() => Set()
      case Expr.Lambda(param, body, tpe) =>
        collectReferencedSymbols(body) -- Set(param)
      case Expr.LetIn(symbol, isConstant, declaredType, value, body) =>
        collectReferencedSymbols(value) ++ (collectReferencedSymbols(body) -- Set(symbol))
      case Expr.Meta(index, tpe) => Set()
      case Expr.UnknownType() => Set()
      case Expr.Match(scrutinee, motive, cases) =>
        collectReferencedSymbols(scrutinee) ++ cases.flatMap(c => 
          collectReferencedSymbols(c.body) ++ collectReferencedSymbolsInPattern(c.pattern))
    }

  private[typing] def collectReferencedSymbolsInPattern(expr: TypedAst.Pattern): Set[Symbol] =
    expr match {
      case Pattern.Wildcard() => Set()
      case Pattern.Lit(value) => Set()
      case Pattern.Binder(symbol) => Set()
      case Pattern.Ctor(symbol, args) => Set(symbol) ++ args.flatMap(arg => collectReferencedSymbolsInPattern(arg))
    }

  // Renders typed expressions in a compact user-facing format for diagnostics.
  private[typing] def prettyExpr(expr: TypedAst.Expr): String =
    expr match
      case TypedAst.Expr.UnknownType() => "_"
      case TypedAst.Expr.Sort() => "Type"
      case TypedAst.Expr.Var(symbol) => symbol.toString
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
      case TypedAst.Expr.Match(_, _, _) => "match"
      case m@TypedAst.Expr.Meta(index, tpe) => s"?${m.name}_$index : ${prettyExpr(tpe)}"

  /** Determines the type of a literal value. */
  private[typing] def literalType(value: ast.Literal, ctx: TypeContext): TypedAst.Expr =
    val typeName = value match
      case ast.Literal.IntLit(_) => "Int"
      case ast.Literal.BoolLit(_) => "Bool"
      case ast.Literal.StringLit(_) => "String"
      case ast.Literal.UnitLit() => "Unit"
    ctx.globals.names.get(typeName)
      .map(sym => TypedAst.Expr.Var(sym)(value.source))
      .getOrElse(TypedAst.Expr.UnknownType()(value.source))

  /** Checks a pattern against the expected scrutinee type. */
  private[typing] def checkPattern(
      pattern: ast.Pattern,
      expectedType: TypedAst.Expr,
      ctx: TypeContext,
      ids: IdSupply
    ): PatternCheckResult =
    pattern match
      case ast.Pattern.Wildcard() =>
        PatternCheckResult(TypedAst.Pattern.Wildcard()(pattern.source), Map(), Map(), List())
      case ast.Pattern.Lit(value) =>
        val litType = literalType(value, ctx)
        val typed = TypedAst.Pattern.Lit(value)(pattern.source)
        val err = if litType == expectedType || expectedType.isInstanceOf[TypedAst.Expr.UnknownType] then Nil
        else List(TypeError("Pattern literal type mismatch", pattern.source))
        PatternCheckResult(typed, Map(), Map(), err)
      case ast.Pattern.BinderOrCtor0(name) =>
        ctx.globals.names.get(name) match
          case Some(symbol) =>
            // Uses constructor matching if a global constructor of this name exists.
            val (ctorSymbol, ctorErrors) = globalSymbolToCtorSymbol(symbol, pattern.source)
            val typed = TypedAst.Pattern.Ctor(ctorSymbol, Nil)(pattern.source)
            val refinement = extractPatternRefinement(ctorSymbol.tpe, expectedType)
            PatternCheckResult(typed, Map(), refinement, ctorErrors)
          case None =>
            // Falls back to a local binder if no constructor with this name is visible.
            val symbol = LocalSymbol(name, expectedType, ids.freshLocalId())
            val typed = TypedAst.Pattern.Binder(symbol)(pattern.source)
            PatternCheckResult(typed, Map(name -> LocalBinding(symbol, None)), Map(), List())
      case ast.Pattern.Ctor(name, args) =>
        ctx.globals.names.get(name) match
          case Some(symbol) =>
            val (ctorSymbol, ctorErrors) = globalSymbolToCtorSymbol(symbol, pattern.source)
            val fieldTypes = extractCtorFieldTypes(ctorSymbol.tpe, expectedType)
            val paddedFieldTypes = fieldTypes.padTo(args.length, TypedAst.Expr.UnknownType()(pattern.source))
            val argResults = args.zip(paddedFieldTypes).map { (arg, fieldType) =>
              checkPattern(arg, fieldType, ctx, ids)
            }
            // Merges nested argument bindings/refinements so dependent information from
            // constructor arguments is available in the branch body.
            val errors = argResults.flatMap(_.errors)
            val bindings = argResults.flatMap(_.bindings).toMap
            val nestedRefinements = argResults.foldLeft(Map[Int, TypedAst.Expr]())((acc, result) => acc ++ result.refinements)
            val typedArgs = argResults.map(_.typedPattern)
            val ctorRefinement = extractPatternRefinement(ctorSymbol.tpe, expectedType)
            PatternCheckResult(
              TypedAst.Pattern.Ctor(ctorSymbol, typedArgs)(pattern.source),
              bindings,
              nestedRefinements ++ ctorRefinement,
              errors ++ ctorErrors
            )
          case None =>
            val symbol = ErrorSymbols.constructor(name)
            PatternCheckResult(
              TypedAst.Pattern.Ctor(symbol, Nil)(pattern.source),
              Map(),
              Map(),
              List(TypeError(s"Unknown constructor ${name}", pattern.source))
            )

  /** Extracts substitutions for scrutinee indices that become known from a constructor pattern. */
  private def extractPatternRefinement(ctorType: TypedAst.Expr, expectedType: TypedAst.Expr): Map[Int, TypedAst.Expr] =
    val (_, resultType) = decomposeCtorType(ctorType)
    val ctorParamIds = collectParamIds(resultType)
    val ctorTypeParamSubst = collectTypeParamSubst(resultType, expectedType, ctorParamIds, Map())
    val instantiatedResultType = substituteTypeParams(resultType, ctorTypeParamSubst)
    val expectedParamIds = collectParamIds(expectedType)
    collectTypeParamSubst(expectedType, instantiatedResultType, expectedParamIds, Map())

  /** Applies type refinements to all local bindings in a context. */
  private[typing] def applyTypeRefinements(ctx: TypeContext, refinements: Map[Int, TypedAst.Expr]): TypeContext =
    if refinements.isEmpty then
      ctx
    else
      val rewrittenLocals = ctx.locals.map { case (name, binding) =>
        val rewrittenBinding = binding.symbol match
          case symbol: LocalSymbol =>
            val rewrittenSymbol = substituteTypeParamsInLocalSymbol(symbol, refinements)
            LocalBinding(rewrittenSymbol, binding.value)
          case _ => binding
        name -> rewrittenBinding
      }
      ctx.copy(locals = rewrittenLocals)

  private def globalSymbolToCtorSymbol(s: GlobalSymbol, source: SourceRange): (CtorSymbol, List[TypeError]) = {
    // TODO remove errors return, it's always empty
    s match
      case c: CtorSymbol =>
        (c, List())
      case _ =>
        (ErrorSymbols.constructor(s.name), List(TypeError(s"Symbol $s is not a constructor", source)))
  }

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
      case TypedAst.Expr.Var(param: LocalSymbol) => Set(param.id)
      case TypedAst.Expr.App(callee, arg, tpe) => collectParamIds(callee) ++ collectParamIds(arg) ++ collectParamIds(tpe)
      case TypedAst.Expr.AppImplicit(callee, arg, tpe) => collectParamIds(callee) ++ collectParamIds(arg) ++ collectParamIds(tpe)
      case TypedAst.Expr.Pi(dom, cod, _) => Set(dom.id) ++ collectParamIds(dom.tpe) ++ collectParamIds(cod)
      case TypedAst.Expr.Lambda(_, body, tpe) => collectParamIds(body) ++ collectParamIds(tpe)
      case TypedAst.Expr.LetIn(_, _, declaredType, value, body) => collectParamIds(declaredType) ++ collectParamIds(value) ++ collectParamIds(body)
      case TypedAst.Expr.Match(scrutinee, motive, cases) => collectParamIds(scrutinee) ++ collectParamIds(motive) ++ cases.flatMap(c => collectParamIds(c.body)).toSet
      case _ => Set.empty

  // Derives substitutions for constructor type parameters by comparing template and expected result types.
  private def collectTypeParamSubst(
      template: TypedAst.Expr,
      expected: TypedAst.Expr,
      paramIds: Set[Int],
      acc: Map[Int, TypedAst.Expr]
    ): Map[Int, TypedAst.Expr] =
    (template, expected) match
      case (TypedAst.Expr.Var(param: LocalSymbol), other) if paramIds.contains(param.id) =>
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
      case (TypedAst.Expr.App(tc, ta, _), TypedAst.Expr.AppImplicit(ec, ea, _)) =>
        val cAcc = collectTypeParamSubst(tc, ec, paramIds, acc)
        collectTypeParamSubst(ta, ea, paramIds, cAcc)
      case (TypedAst.Expr.AppImplicit(tc, ta, _), TypedAst.Expr.App(ec, ea, _)) =>
        val cAcc = collectTypeParamSubst(tc, ec, paramIds, acc)
        collectTypeParamSubst(ta, ea, paramIds, cAcc)
      case _ =>
        acc

  // Replaces constructor type parameters with inferred concrete types from the scrutinee.
  private[typing] def substituteTypeParams(expr: TypedAst.Expr, subst: Map[Int, TypedAst.Expr]): TypedAst.Expr =
    expr match
      case TypedAst.Expr.Var(param: LocalSymbol) =>
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
      case TypedAst.Expr.Match(scrutinee, motive, cases) =>
        val rewrittenCases = cases.map(c => TypedAst.MatchCase(c.pattern, substituteTypeParams(c.body, subst))(c.source))
        TypedAst.Expr.Match(substituteTypeParams(scrutinee, subst), substituteTypeParams(motive, subst), rewrittenCases)(expr.source)
      case _ => expr

  def substituteTypeParamsInLocalSymbol(s: LocalSymbol, subst: Map[Int, TypedAst.Expr]): LocalSymbol =
    LocalSymbol(s.name, substituteTypeParams(s.tpe, subst), s.id)

  /** Extracts constructor field types from a constructor type. */
  private def extractCtorFieldTypes(ctorType: TypedAst.Expr, expectedType: TypedAst.Expr): List[TypedAst.Expr] =
    val (fieldTypes, resultType) = decomposeCtorType(ctorType)
    val paramIds = collectParamIds(resultType)
    val subst = collectTypeParamSubst(resultType, expectedType, paramIds, Map())
    fieldTypes.map(fieldType => substituteTypeParams(fieldType, subst))

  /** Converts a typed function declaration into a lambda term for unfolding. */
  

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
  /** Stores one deferred equality problem that should be solved after elaboration. */
  final case class EqualityConstraint(left: TypedAst.Expr, right: TypedAst.Expr, source: SourceRange)

  /** Classifies constructor-like application heads used by definitional equality. */
  private enum DefEqHeadKind:
    case Constructor, TypeConstructor

  /** Captures an application head and all explicit/implicit arguments. */
  private final case class HeadedExpr(head: Symbol, kind: DefEqHeadKind, args: List[TypedAst.Expr])
