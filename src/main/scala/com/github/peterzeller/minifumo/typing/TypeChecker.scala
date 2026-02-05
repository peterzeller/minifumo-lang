package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast
import com.github.peterzeller.minifumo.builtins.Standard
import com.github.peterzeller.minifumo.common.MinifumoError
import com.github.peterzeller.minifumo.typing.TypedAst.*

import java.nio.file.Paths
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object TypeChecker:
  final case class TypeError(message: String, source: ast.SourceRange) extends MinifumoError


  /** Type-checks a program */
  def checkProgram(program: ast.ProgramFile, globalNames: NameCache & SymbolCache, importStandard: Boolean): (TypedAst.Program, List[TypeError]) =
    val errors = ListBuffer[TypeError]()
    val idSupply = IdSupply()
    val metaStore = MetaStore()
    val (symbolMap, importErrors) = GlobalSymbols.resolveImports(program, globalNames)
    val globals = GlobalEnv(names = symbolMap)
    val typedItems = program.items.map {
      case decl: ast.TopLevel.DataDecl =>
        buildDataDecl(decl, globals, idSupply)
      case decl: ast.TopLevel.FunDecl =>
        val info = signatureInfo(decl.sig.name)
        val context = TypeContext(globals, Map())
          .withLocals(info.implicitParams)
          .withLocals(info.params)
        val typedBody = checkFunctionBody(decl.body, info.returnType, context, metaStore, idSupply, errors)
        TypedAst.TopLevel.FunDecl(info.symbol, decl.sig.implicitParams.map(_.name), info.params, typedBody)(decl.source)
    }
    (TypedAst.Program(typedItems)(program.source), errors.toList)


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
        .orElse(globals.functions.get(name))
        .orElse(globals.ctors.get(name))
        .orElse(globals.types.get(name))

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
      implicitParams: List[TypedAst.ParamSymbol],
      params: List[TypedAst.ParamSymbol],
      returnType: TypedAst.Expr
    )


  // Builds typed function signature information from a parsed function signature.
  private def buildFunctionInfoFromSignature(
      sig: ast.FunSig,
      globals: GlobalEnv,
      idSupply: IdSupply
    ): FunctionInfo =
    val implicitParams = sig.implicitParams.map { param =>
      val paramType = signatureExpr(param.tpe, globals, Map())
      TypedAst.ParamSymbol(param.name, paramType, idSupply.freshLocalId())
    }
    val implicitEnv = implicitParams.map(p => p.name -> p).toMap
    val explicitParams = sig.params.map { param =>
      val paramType = signatureExpr(param.tpe, globals, implicitEnv)
      TypedAst.ParamSymbol(param.name, paramType, idSupply.freshLocalId())
    }
    val returnType = signatureExpr(sig.returnType, globals, implicitEnv ++ explicitParams.map(p => p.name -> p).toMap)
    val funType = buildPiType(implicitParams.map(_.tpe), explicitParams.map(_.tpe), returnType, sig.source)
    val symbol = globals.functions.getOrElse(sig.name, TypedAst.FunctionSymbol(sig.name, funType)).copy(tpe = funType)
    FunctionInfo(symbol, implicitParams, explicitParams, returnType)

  // Replaces references to implicit parameter symbols with unknown types for first-order checking.
  private def eraseImplicitParamRefs(expr: TypedAst.Expr, implicitParamIds: Set[Int]): TypedAst.Expr =
    expr match
      case TypedAst.Expr.Var(sym: TypedAst.ParamSymbol) if implicitParamIds.contains(sym.id) =>
        TypedAst.Expr.UnknownType()(expr.source)
      case TypedAst.Expr.App(callee, arg, tpe) =>
        TypedAst.Expr.App(
          eraseImplicitParamRefs(callee, implicitParamIds),
          eraseImplicitParamRefs(arg, implicitParamIds),
          eraseImplicitParamRefs(tpe, implicitParamIds)
        )(expr.source)
      case TypedAst.Expr.AppImplicit(callee, arg, tpe) =>
        TypedAst.Expr.AppImplicit(
          eraseImplicitParamRefs(callee, implicitParamIds),
          eraseImplicitParamRefs(arg, implicitParamIds),
          eraseImplicitParamRefs(tpe, implicitParamIds)
        )(expr.source)
      case TypedAst.Expr.Pi(dom, cod, isImplicit) =>
        TypedAst.Expr.Pi(
          eraseImplicitParamRefs(dom, implicitParamIds),
          eraseImplicitParamRefs(cod, implicitParamIds),
          isImplicit
        )(expr.source)
      case TypedAst.Expr.Lambda(param, body, tpe) =>
        TypedAst.Expr.Lambda(
          param,
          eraseImplicitParamRefs(body, implicitParamIds),
          eraseImplicitParamRefs(tpe, implicitParamIds)
        )(expr.source)
      case TypedAst.Expr.LetIn(symbol, isConstant, declaredType, value, body) =>
        TypedAst.Expr.LetIn(
          symbol,
          isConstant,
          eraseImplicitParamRefs(declaredType, implicitParamIds),
          eraseImplicitParamRefs(value, implicitParamIds),
          eraseImplicitParamRefs(body, implicitParamIds)
        )(expr.source)
      case TypedAst.Expr.Match(scrutinee, cases) =>
        val rewrittenCases = cases.map(c =>
          TypedAst.MatchCase(c.pattern, eraseImplicitParamRefs(c.body, implicitParamIds))(c.source)
        )
        TypedAst.Expr.Match(eraseImplicitParamRefs(scrutinee, implicitParamIds), rewrittenCases)(expr.source)
      case other => other

  /** Builds outline signatures for all functions and constructors. */
  private def buildSignatures(
      program: ast.ProgramFile,
      globals: GlobalEnv,
      idSupply: IdSupply
    ): Map[String, FunctionInfo] =
    val functionInfos = mutable.Map[String, FunctionInfo]()
    val localFunctionNames = program.items.collect { case decl: ast.TopLevel.FunDecl => decl.sig.name }.toSet

    for (name, sig) <- standardFunctionSignatures if globals.functions.contains(name) && !localFunctionNames.contains(name) do
      val info = buildFunctionInfoFromSignature(sig, globals, idSupply)
      val implicitParamIds = info.implicitParams.map(_.id).toSet
      val erasedType = eraseImplicitParamRefs(info.symbol.tpe, implicitParamIds)
      val erasedSymbol = info.symbol.copy(tpe = erasedType)
      globals.functions.update(name, erasedSymbol)
      functionInfos.update(name, info.copy(symbol = erasedSymbol))

    for item <- program.items do
      item match
        case decl: ast.TopLevel.DataDecl =>
          val typeParams = decl.implicitParams.map { param =>
            val paramType = signatureExpr(param.tpe, globals, Map())
            TypedAst.ParamSymbol(param.name, paramType, idSupply.freshLocalId())
          }
          val dataType = TypedAst.Expr.Var(globals.types(decl.name))(decl.source)
          val appliedType = typeParams.foldLeft(dataType) { (acc, param) =>
            TypedAst.Expr.AppImplicit(acc, TypedAst.Expr.Var(param)(decl.source), TypedAst.Expr.UnknownType()(decl.source))(decl.source)
          }
          for ctor <- decl.ctors do
            val fieldTypes = ctor.fields.map { field => signatureExpr(field.tpe, globals, typeParams.map(p => p.name -> p).toMap) }
            val ctorType = buildPiType(typeParams.map(_.tpe), fieldTypes, appliedType, decl.source)
            globals.ctors.get(ctor.name).foreach { symbol =>
              globals.ctors.update(ctor.name, symbol.copy(tpe = ctorType))
            }
        case decl: ast.TopLevel.FunDecl =>
          val info = buildFunctionInfoFromSignature(decl.sig, globals, idSupply)
          globals.functions.update(decl.sig.name, info.symbol)
          functionInfos.update(decl.sig.name, info)
    functionInfos.toMap

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
      val symbol = globals.ctors.getOrElse(ctor.name, TypedAst.CtorSymbol(ctor.name, TypedAst.Expr.UnknownType()(ctor.source)))
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
      errors: ListBuffer[TypeError]
    ): TypedAst.Expr =
    check(body, returnType)(using context, metas, idSupply) match
      case Right(expr) => expr
      case Left(errs) =>
        errors.addAll(errs)
        TypedAst.Expr.UnknownType()(body.source)

  /** Translates a signature expression to a typed expression. */
  private def signatureExpr(expr: ast.Expr, globals: GlobalEnv, locals: Map[String, TypedAst.TermSymbol]): TypedAst.Expr =
    expr match
      case ast.Expr.Lit(value) => TypedAst.Expr.Lit(value)(expr.source)
      case ast.Expr.Var(name) =>
        if name == "unit" then
          TypedAst.Expr.UnknownType()(expr.source)
        else
          locals.get(name)
            .orElse(globals.types.get(name))
            .orElse(globals.functions.get(name))
            .orElse(globals.ctors.get(name))
            .map(symbol => TypedAst.Expr.Var(symbol)(expr.source))
            .getOrElse(TypedAst.Expr.Var(TypedAst.ErrorSymbol(name, TypedAst.Expr.UnknownType()(expr.source)))(expr.source))
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
        TypedAst.Expr.Pi(dom, cod, isImplicit = false)(expr.source)
      case ast.Expr.Hole() =>
        TypedAst.Expr.UnknownType()(expr.source)
      case _ =>
        TypedAst.Expr.UnknownType()(expr.source)

  /** Builds a Pi-type chain for implicit and explicit parameters. */
  private def buildPiType(
      implicitParams: List[TypedAst.Expr],
      explicitParams: List[TypedAst.Expr],
      resultType: TypedAst.Expr,
      source: ast.SourceRange
    ): TypedAst.Expr =
    val explicitPis = explicitParams.foldRight(resultType) { (dom, cod) =>
      TypedAst.Expr.Pi(dom, cod, isImplicit = false)(source)
    }
    implicitParams.foldRight(explicitPis) { (dom, cod) =>
      TypedAst.Expr.Pi(dom, cod, isImplicit = true)(source)
    }

  /** Checks if two types are definitionally equal, solving metas as needed. */
  def isDefEq(t1: TypedAst.Expr, t2: TypedAst.Expr)
             (implicit ctx: Context, metas: MetaContext): CheckResult[Unit] =
    val norm1 = whnf(t1)
    val norm2 = whnf(t2)
    (norm1, norm2) match
      case (TypedAst.Expr.UnknownType(), _) => Right(())
      case (_, TypedAst.Expr.UnknownType()) => Right(())
      case (TypedAst.Expr.Meta(id, _), other) =>
        solveMeta(id, other)
      case (other, TypedAst.Expr.Meta(id, _)) =>
        solveMeta(id, other)
      case (TypedAst.Expr.Var(s1), TypedAst.Expr.Var(s2)) if s1 == s2 => Right(())
      case (TypedAst.Expr.Var(p1: TypedAst.ParamSymbol), TypedAst.Expr.Var(p2: TypedAst.ParamSymbol))
          if p1.name == p2.name => Right(())
      case (TypedAst.Expr.Lit(v1), TypedAst.Expr.Lit(v2)) if v1 == v2 => Right(())
      case (TypedAst.Expr.Sort(), TypedAst.Expr.Sort()) => Right(())
      case (TypedAst.Expr.App(c1, a1, _), TypedAst.Expr.App(c2, a2, _)) =>
        combine(isDefEq(c1, c2), isDefEq(a1, a2))
      case (TypedAst.Expr.AppImplicit(c1, a1, _), TypedAst.Expr.AppImplicit(c2, a2, _)) =>
        combine(isDefEq(c1, c2), isDefEq(a1, a2))
      case (TypedAst.Expr.Lambda(p1, b1, _), TypedAst.Expr.Lambda(p2, b2, _)) =>
        combine(isDefEq(p1.tpe, p2.tpe), isDefEq(b1, b2))
      case (TypedAst.Expr.Pi(d1, c1, i1), TypedAst.Expr.Pi(d2, c2, i2)) if i1 == i2 =>
        combine(isDefEq(d1, d2), isDefEq(c1, c2))
      case _ => Left(List(TypeError("Type mismatch", norm1.source.merge(norm2.source))))

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
        TypedAst.Expr.Pi(instantiate(dom), instantiate(cod), isImplicit)(term.source)
      case TypedAst.Expr.Match(scrutinee, cases) =>
        val newCases = cases.map(c => TypedAst.MatchCase(c.pattern, instantiate(c.body))(c.source))
        TypedAst.Expr.Match(instantiate(scrutinee), newCases)(term.source)
      case other => other

  /** Infers the type of an expression, producing a typed expression alongside its type. */
  private def infer(expr: ast.Expr)
                   (implicit ctx: TypeContext, metas: MetaContext, ids: IdSupply): CheckResult[(TypedAst.Expr, TypedAst.Expr)] =
    expr match
      case ast.Expr.Var(name) =>
        ctx.lookupSymbol(name) match
          case Some(symbol) =>
            println(s"Looking up symbol ${name} returns symbol ${symbol} ")
            Right(TypedAst.Expr.Var(symbol)(expr.source) -> symbol.tpe)
          case None =>
            Left(List(TypeError(s"Unknown symbol ${name}", expr.source)))
      case ast.Expr.Lit(value) =>
        val typed = TypedAst.Expr.Lit(value)(expr.source)
        val tpe = literalType(value, ctx)
        Right(typed -> tpe)
      case ast.Expr.Call(callee, arg) =>
        infer(callee).flatMap { (calleeExpr, calleeType) =>
          whnf(calleeType) match
            case TypedAst.Expr.Pi(_, _, true) if calleeExpr.isInstanceOf[TypedAst.Expr.Var] && calleeExpr.asInstanceOf[TypedAst.Expr.Var].symbol.isInstanceOf[TypedAst.CtorSymbol] =>
              Left(List(TypeError(s"Missing type argument for ${prettyExpr(calleeExpr)}", expr.source)))
            case _ =>
              val (appliedExpr, appliedType) = insertImplicitArgs(calleeExpr, calleeType, expr.source)
              whnf(appliedType) match
                case TypedAst.Expr.Pi(dom, cod, false) =>
                  check(arg, dom).map { argExpr =>
                    TypedAst.Expr.App(appliedExpr, argExpr, cod)(expr.source) -> cod
                  }
                case TypedAst.Expr.UnknownType() =>
                  val argResult = infer(arg)
                  argResult.map { (argExpr, _) =>
                    TypedAst.Expr.App(appliedExpr, argExpr, TypedAst.Expr.UnknownType()(expr.source))(expr.source) -> TypedAst.Expr.UnknownType()(expr.source)
                  }
                case other =>
                  Left(List(TypeError(s"Expected a function, but expression has type ${prettyExpr(other)}", expr.source)))
        }
      case ast.Expr.CallImplicit(callee, arg) =>
        infer(callee).flatMap { (calleeExpr, calleeType) =>
          whnf(calleeType) match
            case TypedAst.Expr.Pi(dom, cod, true) =>
              check(arg, dom).map { argExpr =>
                TypedAst.Expr.AppImplicit(calleeExpr, argExpr, cod)(expr.source) -> cod
              }
            case TypedAst.Expr.UnknownType() =>
              infer(arg).map { (argExpr, _) =>
                TypedAst.Expr.AppImplicit(calleeExpr, argExpr, TypedAst.Expr.UnknownType()(expr.source))(expr.source) -> TypedAst.Expr.UnknownType()(expr.source)
              }
            case other => Left(List(TypeError(s"Expected an implicit function, but expression ${prettyExpr(calleeExpr)} has type ${prettyExpr(other)}", expr.source)))
        }
      case ast.Expr.Lambda(param, body) =>
        val paramType = param.tpe.map(t => signatureExpr(t, ctx.globals, Map())).getOrElse(TypedAst.Expr.UnknownType()(expr.source))
        val paramSymbol = TypedAst.LocalSymbol(param.name, paramType, ids.freshLocalId())
        val bodyCtx = ctx.withLocal(paramSymbol)
        infer(body)(using bodyCtx, metas, ids).map { (bodyExpr, bodyType) =>
          val fnType = TypedAst.Expr.Pi(paramType, bodyType, isImplicit = false)(expr.source)
          TypedAst.Expr.Lambda(paramSymbol, bodyExpr, fnType)(expr.source) -> fnType
        }
      case ast.Expr.LetIn(name, declaredType, value, body) =>
        val inferredValue = declaredType match
          case Some(tpe) =>
            val expected = signatureExpr(tpe, ctx.globals, Map())
            check(value, expected).map(_ -> expected)
          case None => infer(value)
        inferredValue.flatMap { (valueExpr, valueType) =>
          val symbol = TypedAst.LocalSymbol(name, valueType, ids.freshLocalId())
          val bodyCtx = ctx.withLocal(symbol, Some(valueExpr))
          infer(body)(using bodyCtx, metas, ids).map { (bodyExpr, bodyType) =>
            TypedAst.Expr.LetIn(symbol, isConstant = false, valueType, valueExpr, bodyExpr)(expr.source) -> bodyType
          }
        }
      case ast.Expr.Pi(param, body) =>
        val dom = signatureExpr(param.tpe, ctx.globals, Map())
        val cod = signatureExpr(body, ctx.globals, Map())
        val piExpr = TypedAst.Expr.Pi(dom, cod, isImplicit = false)(expr.source)
        Right(piExpr -> TypedAst.Expr.Sort()(expr.source))
      case ast.Expr.Match(scrutinee, cases) =>
        infer(scrutinee).flatMap { (scrutineeExpr, scrutineeType) =>
          val resultMeta = freshMeta(TypedAst.Expr.UnknownType()(expr.source), expr.source)
          val typedCases = cases.map { case ast.MatchCase(pattern, body) =>
            val (typedPattern, patternCtx, patternErrors) = checkPattern(pattern, scrutineeType, ctx, ids)
            val caseCtx = ctx.copy(locals = ctx.locals ++ patternCtx)
            val bodyResult = check(body, resultMeta)(using caseCtx, metas, ids)
            (typedPattern, bodyResult, patternErrors)
          }
          val errors = typedCases.collect { case (_, Left(errs), _) => errs }.flatten ++ typedCases.flatMap(_._3)
          if errors.nonEmpty then
            Left(errors)
          else
            val typedCasesExpr = typedCases.collect { case (pat, Right(bodyExpr), _) =>
              TypedAst.MatchCase(pat, bodyExpr)(bodyExpr.source)
            }
            Right(TypedAst.Expr.Match(scrutineeExpr, typedCasesExpr)(expr.source) -> resultMeta)
        }
      case ast.Expr.Hole() =>
        val meta = freshMeta(TypedAst.Expr.UnknownType()(expr.source), expr.source)
        Right(meta -> TypedAst.Expr.UnknownType()(expr.source))

  /** Checks an expression against an expected type. */
  private def check(expr: ast.Expr, expectedType: TypedAst.Expr)
                   (implicit ctx: TypeContext, metas: MetaContext, ids: IdSupply): CheckResult[TypedAst.Expr] =
    val expectedNorm = whnf(expectedType)
    (expr, expectedNorm) match
      case (ast.Expr.Lambda(param, body), TypedAst.Expr.Pi(dom, cod, false)) =>
        val paramSymbol = TypedAst.LocalSymbol(param.name, dom, ids.freshLocalId())
        val bodyCtx = ctx.withLocal(paramSymbol)
        check(body, cod)(using bodyCtx, metas, ids).map { bodyExpr =>
          TypedAst.Expr.Lambda(paramSymbol, bodyExpr, expectedNorm)(expr.source)
        }
      case (ast.Expr.Match(scrutinee, cases), _) =>
        infer(expr).flatMap { (inferredExpr, inferredType) =>
          isDefEq(inferredType, expectedNorm).map(_ => inferredExpr)
        }
      case _ =>
        infer(expr).flatMap { (inferredExpr, inferredType) =>
          isDefEq(inferredType, expectedNorm) match
            case Right(_) => Right(inferredExpr)
            case Left(errs) =>
              Left(TypeError(s"Expected ${prettyExpr(expectedNorm)} but got ${prettyExpr(inferredType)}", expr.source) :: errs)
        }

  /** Inserts implicit arguments as metas before explicit application. */
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
          val meta = freshMeta(dom, source)
          currentExpr = TypedAst.Expr.AppImplicit(currentExpr, meta, cod)(source)
          currentType = cod
        case _ => keepGoing = false
    (currentExpr, currentType)

  /** Creates a fresh meta-variable expression. */
  private def freshMeta(tpe: TypedAst.Expr, source: ast.SourceRange)(implicit ids: IdSupply): TypedAst.Expr =
    TypedAst.Expr.Meta(ids.freshMetaId(), tpe)(source)

  /** Checks whether a meta-variable can be solved with a term. */
  private def solveMeta(metaId: Int, term: TypedAst.Expr)
                       (implicit metas: MetaContext): CheckResult[Unit] =
    if occurs(metaId, term) then
      Left(List(TypeError("Occurs check failed", term.source)))
    else
      metas.assign(metaId, term)
      Right(())

  /** Performs an occurs check for a meta-variable inside a term. */
  private def occurs(metaId: Int, term: TypedAst.Expr): Boolean =
    term match
      case TypedAst.Expr.Meta(id, _) => id == metaId
      case TypedAst.Expr.App(callee, arg, _) => occurs(metaId, callee) || occurs(metaId, arg)
      case TypedAst.Expr.AppImplicit(callee, arg, _) => occurs(metaId, callee) || occurs(metaId, arg)
      case TypedAst.Expr.Lambda(_, body, _) => occurs(metaId, body)
      case TypedAst.Expr.LetIn(_, _, _, value, body) => occurs(metaId, value) || occurs(metaId, body)
      case TypedAst.Expr.Pi(dom, cod, _) => occurs(metaId, dom) || occurs(metaId, cod)
      case TypedAst.Expr.Match(scrutinee, cases) =>
        occurs(metaId, scrutinee) || cases.exists(c => occurs(metaId, c.body))
      case _ => false

  /** Combines two check results while accumulating errors. */
  private def combine[A, B](left: CheckResult[A], right: CheckResult[B]): CheckResult[Unit] =
    (left, right) match
      case (Right(_), Right(_)) => Right(())
      case _ => Left(left.left.getOrElse(Nil) ++ right.left.getOrElse(Nil))

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
        TypedAst.Expr.Pi(substitute(dom, symbol, value), substitute(cod, symbol, value), isImplicit)(term.source)
      case TypedAst.Expr.Match(scrutinee, cases) =>
        val newCases = cases.map(c => TypedAst.MatchCase(c.pattern, substitute(c.body, symbol, value))(c.source))
        TypedAst.Expr.Match(substitute(scrutinee, symbol, value), newCases)(term.source)
      case other => other

  /** Exposes substitution for tests. */
  private[typing] def substituteForTest(term: TypedAst.Expr, symbol: TypedAst.LocalSymbol, value: TypedAst.Expr): TypedAst.Expr =
    substitute(term, symbol, value)

  /** Infers an expression in a test context built from a program and exports. */
  private[typing] def inferInTestContext(
      program: ast.ProgramFile,
      expr: ast.Expr,
      exports: ExportEnv = emptyExportEnv
    ): CheckResult[(TypedAst.Expr, TypedAst.Expr)] =
    val globals = buildGlobals(program, exports)
    given ctx: TypeContext = TypeContext(globals, Map())
    given metas: MetaContext = MetaStore()
    given ids: IdSupply = IdSupply()
    infer(expr)

  /** Checks an expression in a test context against a named type. */
  private[typing] def checkInTestContext(
      program: ast.ProgramFile,
      expr: ast.Expr,
      expectedTypeName: String,
      exports: ExportEnv = emptyExportEnv
    ): CheckResult[TypedAst.Expr] =
    val globals = buildGlobals(program, exports)
    val expectedType = globals.types.get(expectedTypeName)
      .map(sym => TypedAst.Expr.Var(sym)(expr.source))
      .getOrElse(TypedAst.Expr.UnknownType()(expr.source))
    given ctx: TypeContext = TypeContext(globals, Map())
    given metas: MetaContext = MetaStore()
    given ids: IdSupply = IdSupply()
    check(expr, expectedType)

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
      case TypedAst.Expr.Pi(dom, cod, true) => s"[${prettyExpr(dom)}] -> ${prettyExpr(cod)}"
      case TypedAst.Expr.Pi(dom, cod, false) => s"${prettyExpr(dom)} -> ${prettyExpr(cod)}"
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
    ctx.globals.types.get(typeName)
      .map(sym => TypedAst.Expr.Var(sym)(value.source))
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
        ctx.globals.ctors.get(name) match
          case Some(symbol) =>
            val typed = TypedAst.Pattern.Ctor(symbol, Nil)(pattern.source)
            (typed, Map(), List())
          case None =>
            val symbol = TypedAst.LocalSymbol(name, expectedType, ids.freshLocalId())
            val typed = TypedAst.Pattern.Binder(symbol)(pattern.source)
            (typed, Map(name -> LocalBinding(symbol, None)), List())
      case ast.Pattern.Ctor(name, args) =>
        ctx.globals.ctors.get(name) match
          case Some(symbol) =>
            val fieldTypes = extractCtorFieldTypes(symbol.tpe, expectedType)
            val paddedFieldTypes = fieldTypes.padTo(args.length, TypedAst.Expr.UnknownType()(pattern.source))
            val argResults = args.zip(paddedFieldTypes).map { (arg, fieldType) =>
              checkPattern(arg, fieldType, ctx, ids)
            }
            val errors = argResults.flatMap(_._3)
            val bindings = argResults.flatMap(_._2).toMap
            val typedArgs = argResults.map(_._1)
            (TypedAst.Pattern.Ctor(symbol, typedArgs)(pattern.source), bindings, errors)
          case None =>
            val symbol = TypedAst.CtorSymbol(name, TypedAst.Expr.UnknownType()(pattern.source))
            (TypedAst.Pattern.Ctor(symbol, Nil)(pattern.source), Map(), List(TypeError(s"Unknown constructor ${name}", pattern.source)))

  // Collects explicit field types and result type from a constructor signature.
  private def decomposeCtorType(ctorType: TypedAst.Expr): (List[TypedAst.Expr], TypedAst.Expr) =
    def loop(tpe: TypedAst.Expr, fields: List[TypedAst.Expr]): (List[TypedAst.Expr], TypedAst.Expr) =
      tpe match
        case TypedAst.Expr.Pi(dom, cod, false) =>
          loop(cod, fields :+ dom)
        case TypedAst.Expr.Pi(_, cod, true) =>
          loop(cod, fields)
        case _ =>
          (fields, tpe)
    loop(ctorType, Nil)

  // Collects parameter symbol ids that appear in a type expression.
  private def collectParamIds(expr: TypedAst.Expr): Set[Int] =
    expr match
      case TypedAst.Expr.Var(param: TypedAst.ParamSymbol) => Set(param.id)
      case TypedAst.Expr.App(callee, arg, tpe) => collectParamIds(callee) ++ collectParamIds(arg) ++ collectParamIds(tpe)
      case TypedAst.Expr.AppImplicit(callee, arg, tpe) => collectParamIds(callee) ++ collectParamIds(arg) ++ collectParamIds(tpe)
      case TypedAst.Expr.Pi(dom, cod, _) => collectParamIds(dom) ++ collectParamIds(cod)
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
      case (TypedAst.Expr.Var(param: TypedAst.ParamSymbol), other) if paramIds.contains(param.id) =>
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
      case TypedAst.Expr.Var(param: TypedAst.ParamSymbol) =>
        subst.getOrElse(param.id, expr)
      case TypedAst.Expr.App(callee, arg, tpe) =>
        TypedAst.Expr.App(substituteTypeParams(callee, subst), substituteTypeParams(arg, subst), substituteTypeParams(tpe, subst))(expr.source)
      case TypedAst.Expr.AppImplicit(callee, arg, tpe) =>
        TypedAst.Expr.AppImplicit(substituteTypeParams(callee, subst), substituteTypeParams(arg, subst), substituteTypeParams(tpe, subst))(expr.source)
      case TypedAst.Expr.Pi(dom, cod, isImplicit) =>
        TypedAst.Expr.Pi(substituteTypeParams(dom, subst), substituteTypeParams(cod, subst), isImplicit)(expr.source)
      case TypedAst.Expr.Lambda(param, body, tpe) =>
        TypedAst.Expr.Lambda(param, substituteTypeParams(body, subst), substituteTypeParams(tpe, subst))(expr.source)
      case TypedAst.Expr.LetIn(symbol, isConstant, declaredType, value, body) =>
        TypedAst.Expr.LetIn(symbol, isConstant, substituteTypeParams(declaredType, subst), substituteTypeParams(value, subst), substituteTypeParams(body, subst))(expr.source)
      case TypedAst.Expr.Match(scrutinee, cases) =>
        val rewrittenCases = cases.map(c => TypedAst.MatchCase(c.pattern, substituteTypeParams(c.body, subst))(c.source))
        TypedAst.Expr.Match(substituteTypeParams(scrutinee, subst), rewrittenCases)(expr.source)
      case _ => expr

  /** Extracts constructor field types from a constructor type. */
  private def extractCtorFieldTypes(ctorType: TypedAst.Expr, expectedType: TypedAst.Expr): List[TypedAst.Expr] =
    val (fieldTypes, resultType) = decomposeCtorType(ctorType)
    val paramIds = collectParamIds(resultType)
    val subst = collectTypeParamSubst(resultType, expectedType, paramIds, Map())
    fieldTypes.map(fieldType => substituteTypeParams(fieldType, subst))

  type CheckResult[T] = Either[List[TypeError], T]

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
