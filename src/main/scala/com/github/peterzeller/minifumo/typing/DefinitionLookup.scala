package com.github.peterzeller.minifumo.typing

import com.github.peterzeller.minifumo.ast.{SourcePos, SourceRange, SourceRangeWithFile}
import com.github.peterzeller.minifumo.typing.TypedAst.Expr
import com.github.peterzeller.minifumo.typing.TypedAst.TopLevel

import scala.collection.mutable

/** Resolves go-to-definition targets for typed Minifumo programs. */
object DefinitionLookup:

  /** Finds the definition location for the symbol under the given source position. */
  def definitionAt(program: TypedAst.Program, pos: SourcePos, currentFile: String): Option[SourceRangeWithFile] =
    val localDefinitions = collectLocalDefinitions(program)
    resolveAt(program, pos, currentFile, localDefinitions)
      .orElse {
        if pos.column > 1 then resolveAt(program, SourcePos(pos.line, pos.column - 1), currentFile, localDefinitions)
        else None
      }

  /** Resolves a definition target for the AST node found at one cursor position. */
  private def resolveAt(
      program: TypedAst.Program,
      pos: SourcePos,
      currentFile: String,
      localDefinitions: Map[Int, SourceRange]
    ): Option[SourceRangeWithFile] =
    findElementAtCursor(program, pos).flatMap {
      case Expr.Var(symbol) =>
        definitionRange(symbol, currentFile, localDefinitions)
      case TypedAst.Pattern.Ctor(symbol, _) =>
        definitionRange(symbol, currentFile, localDefinitions)
      case _ =>
        None
    }

  /** Finds the innermost typed AST node that still contains the cursor position. */
  def findElementAtCursor(root: com.github.peterzeller.minifumo.typing.TypedAst, pos: SourcePos): Option[com.github.peterzeller.minifumo.typing.TypedAst] =
    if !root.source.contains(pos) then
      None
    else
      val matchingChildren = root.children
        .filter(child => child.source.contains(pos) && isStrictlyNarrower(child.source, root.source))
        .sortBy(nodeWidth)
      matchingChildren.view.flatMap(child => findElementAtCursor(child, pos)).headOption.orElse(Some(root))

  /** Computes an ordering key that prefers narrower source ranges. */
  private def nodeWidth(node: com.github.peterzeller.minifumo.typing.TypedAst): Int =
    rangeWidth(node.source)

  /** Computes an ordering key that prefers narrower source ranges. */
  private def rangeWidth(range: SourceRange): Int =
    val lineSpan = math.max(0, range.end.line - range.start.line)
    val columnSpan = math.max(0, range.end.column - range.start.column)
    lineSpan * 10000 + columnSpan

  /** Checks whether the child range is narrower than the parent range. */
  private def isStrictlyNarrower(child: SourceRange, parent: SourceRange): Boolean =
    child.start != parent.start || child.end != parent.end

  /** Collects known local-symbol definition ranges keyed by local symbol id. */
  private def collectLocalDefinitions(program: TypedAst.Program): Map[Int, SourceRange] =
    val definitions = mutable.Map[Int, SourceRange]()
    for item <- program.items do
      collectTopLevelDefinitions(item, definitions)
    definitions.toMap

  /** Collects local definitions from one top-level declaration. */
  private def collectTopLevelDefinitions(item: TopLevel, definitions: mutable.Map[Int, SourceRange]): Unit =
    item match
      case TopLevel.DataDecl(dataSymbol, typeParams, ctors) =>
        collectDatatypeTypeParamDefinitions(dataSymbol, typeParams, definitions)
        for ctor <- ctors do
          collectExprDefinitions(ctor.returnType, definitions)
          collectExprDefinitions(ctor.tpe, definitions)
          collectCtorFieldDefinitions(dataSymbol, ctor.symbol, ctor.fields, definitions)
      case TopLevel.FunDecl(sig, body) =>
        collectFunParamDefinitions(sig.symbol, sig.typeParams ++ sig.params, definitions)
        collectExprDefinitions(sig.returnType, definitions)
        collectExprDefinitions(body, definitions)

  /** Uses declaration AST data to map typed function parameters to source ranges. */
  private def collectFunParamDefinitions(
      functionSymbol: FunctionSymbol,
      params: List[LocalSymbol],
      definitions: mutable.Map[Int, SourceRange]
    ): Unit =
    val astParams = functionSymbol.continuationData.toList.flatMap { data =>
      data.declAst.sig.implicitParams ++ data.declAst.sig.params
    }
    for (typedParam, astParam) <- params.zip(astParams) do
      definitions.getOrElseUpdate(typedParam.id, astParam.source)

  /** Uses declaration AST data to map typed datatype parameters to source ranges. */
  private def collectDatatypeTypeParamDefinitions(
      datatypeSymbol: DatatypeSymbol,
      params: List[LocalSymbol],
      definitions: mutable.Map[Int, SourceRange]
    ): Unit =
    val astParams = datatypeSymbol.continuationData.toList.flatMap { data =>
      data.declAst.implicitParams ++ data.declAst.params
    }
    for (typedParam, astParam) <- params.zip(astParams) do
      definitions.getOrElseUpdate(typedParam.id, astParam.source)

  /** Uses declaration AST data to map typed constructor fields to source ranges. */
  private def collectCtorFieldDefinitions(
      datatypeSymbol: DatatypeSymbol,
      ctorSymbol: CtorSymbol,
      fields: List[LocalSymbol],
      definitions: mutable.Map[Int, SourceRange]
    ): Unit =
    val astFields = datatypeSymbol.continuationData.toList.flatMap { data =>
      data.declAst.ctors.find(_.name == ctorSymbol.name).toList.flatMap(_.fields)
    }
    for (typedField, astField) <- fields.zip(astFields) do
      definitions.getOrElseUpdate(typedField.id, astField.source)

  /** Collects local symbol definitions introduced by expression binders. */
  private def collectExprDefinitions(expr: Expr, definitions: mutable.Map[Int, SourceRange]): Unit =
    expr match
      case Expr.AppImplicit(callee, arg, tpe) =>
        collectExprDefinitions(callee, definitions)
        collectExprDefinitions(arg, definitions)
        collectExprDefinitions(tpe, definitions)
      case Expr.App(callee, arg, tpe) =>
        collectExprDefinitions(callee, definitions)
        collectExprDefinitions(arg, definitions)
        collectExprDefinitions(tpe, definitions)
      case Expr.Pi(dom, cod, _) =>
        definitions.getOrElseUpdate(dom.id, expr.source)
        collectExprDefinitions(cod, definitions)
      case Expr.Lambda(param, body, tpe) =>
        definitions.getOrElseUpdate(param.id, expr.source)
        collectExprDefinitions(body, definitions)
        collectExprDefinitions(tpe, definitions)
      case Expr.LetIn(symbol, _, declaredType, value, body) =>
        definitions.getOrElseUpdate(symbol.id, expr.source)
        collectExprDefinitions(declaredType, definitions)
        collectExprDefinitions(value, definitions)
        collectExprDefinitions(body, definitions)
      case Expr.Match(scrutinee, motive, cases) =>
        collectExprDefinitions(scrutinee, definitions)
        collectExprDefinitions(motive, definitions)
        for c <- cases do
          collectPatternDefinitions(c.pattern, definitions)
          collectExprDefinitions(c.body, definitions)
      case Expr.Lit(_) | Expr.Var(_) | Expr.Sort() | Expr.Meta(_, _) | Expr.UnknownType() | Expr.Axiom() =>
        ()

  /** Collects local symbol definitions introduced by pattern binders. */
  private def collectPatternDefinitions(pattern: TypedAst.Pattern, definitions: mutable.Map[Int, SourceRange]): Unit =
    pattern match
      case TypedAst.Pattern.Binder(symbol) =>
        definitions.getOrElseUpdate(symbol.id, pattern.source)
        ()
      case TypedAst.Pattern.Ctor(_, args) =>
        for arg <- args do
          collectPatternDefinitions(arg, definitions)
      case TypedAst.Pattern.Wildcard() | TypedAst.Pattern.Lit(_) =>
        ()

  /** Resolves a declaration location from a symbol. */
  private def definitionRange(symbol: Symbol, currentFile: String, localDefinitions: Map[Int, SourceRange]): Option[SourceRangeWithFile] =
    symbol match
      case functionSymbol: FunctionSymbol =>
        functionSymbol.continuationData.map(data => SourceRangeWithFile(normalizeFile(functionSymbol.file, currentFile), data.declAst.sig.source))
      case datatypeSymbol: DatatypeSymbol =>
        datatypeSymbol.continuationData.map(data => SourceRangeWithFile(normalizeFile(datatypeSymbol.file, currentFile), data.declAst.source))
      case ctorSymbol: CtorSymbol =>
        ctorSymbol.dt.continuationData.flatMap { data =>
          data.declAst.ctors.find(_.name == ctorSymbol.name).map(ctor => SourceRangeWithFile(normalizeFile(ctorSymbol.file, currentFile), ctor.source))
        }
      case localSymbol: LocalSymbol =>
        localDefinitions.get(localSymbol.id).orElse(Some(localSymbol.tpe.source)).map(range => SourceRangeWithFile(currentFile, range))
      case _ =>
        None

  /** Normalizes empty symbol file names to the current source file path. */
  private def normalizeFile(file: String, currentFile: String): String =
    if file == null || file.isBlank then currentFile else file
