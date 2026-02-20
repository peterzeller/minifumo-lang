package com.github.peterzeller.minifumo.backends.lean

import com.github.peterzeller.minifumo.ast.{Literal, SourceRange}
import com.github.peterzeller.minifumo.backends.lean.LeanBackend.{GeneratedLeanFile, SourceMapEntry}
import com.github.peterzeller.minifumo.typing.ProjectSymbolCache
import com.github.peterzeller.minifumo.typing.TypedAst
import com.github.peterzeller.minifumo.typing.TypedAst.{GlobalSymbolSymbol, LocalSymbol}

import java.nio.file.Path
import scala.collection.mutable

object LeanEmitter:
  // Represents one emitted declaration snippet with source span info.
  private final case class DeclChunk(lines: Vector[String], sourceFile: Path, sourceRange: SourceRange)

  // Emits one Lean module for a group of Minifumo files.
  def emitModule(moduleName: String, imports: List[String], files: List[String], cache: ProjectSymbolCache): GeneratedLeanFile =
    val mangle = new LeanNameMangler.Context()
    val chunks = mutable.ListBuffer[DeclChunk]()
    val typeEnv = mutable.Map[String, TypedAst.Expr]()

    // Collect typed declarations first so dependency sorting can include all local names.
    val declarations = files.flatMap: file =>
      val (program, _) = cache.typedAst(file)
      program.items.map(item => (file, item))

    declarations.foreach: (_, decl) =>
      decl match
        case TypedAst.TopLevel.DataDecl(symbol, _, _) =>
          typeEnv += symbol.name -> symbol.tpe
        case TypedAst.TopLevel.FunDecl(sig, _) =>
          typeEnv += sig.symbol.name -> sig.symbol.tpe

    val externalRefs = collectExternalRefs(declarations, typeEnv.toMap)

    val depGraph = buildDeclDependencyGraph(declarations)
    val orderedGroups = LeanDependencyPlanner.topologicalSccs(depGraph)
    val declarationByKey = declarations.map((f, d) => declKey(f, d) -> (f, d)).toMap

    orderedGroups.foreach: group =>
      group.toList.sorted.foreach: key =>
        declarationByKey.get(key).foreach: (file, decl) =>
          emitDecl(file, decl, mangle, typeEnv.toMap).foreach(chunks += _)

    val importLines = imports.map(i => s"import ${i}").toVector
    val header =
      if importLines.isEmpty then
        Vector(s"namespace ${moduleName}", "", "set_option autoImplicit true", "")
      else
        importLines ++ Vector("", s"namespace ${moduleName}", "", "set_option autoImplicit true", "")
    val footer = Vector("", s"end ${moduleName}")
    val axiomLines = externalRefs.toList.sortBy(_._1).map: (name, tpe) =>
      s"axiom ${emitGlobalRef(name, mangle)} : ${emitExpr(tpe, mangle, Map.empty, typeEnv.toMap)}"
    val bodyLines = (if axiomLines.isEmpty then Vector.empty else axiomLines.toVector :+ "") ++ chunks.flatMap(_.lines).toVector
    val contentLines = header ++ bodyLines ++ footer

    val lineMap = mutable.ListBuffer[SourceMapEntry]()
    var lineCursor = header.length + 1
    chunks.foreach: chunk =>
      val start = lineCursor
      val end = lineCursor + chunk.lines.length - 1
      lineMap += SourceMapEntry(start, end, chunk.sourceFile, chunk.sourceRange)
      lineCursor = end + 1

    GeneratedLeanFile(Path.of(s"${moduleName}.lean"), contentLines.mkString("\n"), lineMap.toVector)

  // Creates a stable declaration key for dependency sorting.
  private def declKey(file: String, decl: TypedAst.TopLevel): String =
    decl match
      case TypedAst.TopLevel.DataDecl(symbol, _, _) => s"${file}::${symbol.name}"
      case TypedAst.TopLevel.FunDecl(sig, _) => s"${file}::${sig.symbol.name}"

  // Builds a dependency graph between top-level declarations as dependency -> dependent edges.
  private def buildDeclDependencyGraph(declarations: List[(String, TypedAst.TopLevel)]): Map[String, Set[String]] =
    val keyByName = declarations.map((file, decl) =>
      decl match
        case TypedAst.TopLevel.DataDecl(symbol, _, _) => symbol.name -> s"${file}::${symbol.name}"
        case TypedAst.TopLevel.FunDecl(sig, _) => sig.symbol.name -> s"${file}::${sig.symbol.name}"
    ).toMap

    val outgoing = mutable.Map[String, Set[String]]().withDefaultValue(Set.empty)
    declarations.foreach((file, decl) => outgoing.update(declKey(file, decl), Set.empty))

    declarations.foreach: (file, decl) =>
      val currentKey = declKey(file, decl)
      def addExprDeps(expr: TypedAst.Expr): Unit =
        collectGlobalNames(expr).foreach: name =>
          keyByName.get(name).foreach: depKey =>
            if depKey != currentKey then
              outgoing.update(depKey, outgoing(depKey) + currentKey)
      decl match
        case TypedAst.TopLevel.DataDecl(symbol, typeParams, ctors) =>
          typeParams.foreach(param => addExprDeps(param.tpe))
          addExprDeps(symbol.tpe)
          ctors.foreach: ctor =>
            addExprDeps(ctor.symbol.tpe)
            ctor.fields.foreach(field => addExprDeps(field.tpe))
        case TypedAst.TopLevel.FunDecl(sig, body) =>
          addExprDeps(sig.symbol.tpe)
          sig.typeParams.foreach(param => addExprDeps(param.tpe))
          sig.params.foreach(param => addExprDeps(param.tpe))
          addExprDeps(sig.returnType)
          addExprDeps(body)
    outgoing.toMap

  // Emits one data or function declaration to Lean code.
  private def emitDecl(
      file: String,
      decl: TypedAst.TopLevel,
      mangle: LeanNameMangler.Context,
      typeEnv: Map[String, TypedAst.Expr]
    ): Option[DeclChunk] =
    decl match
      case TypedAst.TopLevel.DataDecl(symbol, typeParams, ctors) =>
        val dataName = mangle.mangle(LeanNameMangler.NameKind.GlobalName, symbol.name)
        val params = typeParams.map(param => s"({${mangle.mangle(LeanNameMangler.NameKind.LocalName, param.name)} : ${emitExpr(param.tpe, mangle, Map.empty, typeEnv)}})").mkString(" ")
        val ctorLines = ctors.map: ctor =>
          val ctorName = mangle.mangle(LeanNameMangler.NameKind.GlobalName, ctor.symbol.name)
          if ctor.fields.isEmpty then
            s"| ${ctorName} : ${dataName}${renderTypeParamArgs(typeParams, mangle)}"
          else
            val fields = ctor.fields.map(field => emitExpr(field.tpe, mangle, Map.empty, typeEnv)).mkString(" -> ")
            s"| ${ctorName} : ${fields} -> ${dataName}${renderTypeParamArgs(typeParams, mangle)}"
        val lines = Vector(s"inductive ${dataName} ${params} : Type where") ++ ctorLines.toVector :+ ""
        Some(DeclChunk(lines, Path.of(file), declSource(decl)))

      case TypedAst.TopLevel.FunDecl(sig, body) =>
        val funName = mangle.mangle(LeanNameMangler.NameKind.GlobalName, sig.symbol.name)
        val localScope = mutable.Map[Int, String]()
        val typeParams = sig.typeParams.map: p =>
          val n = mangle.mangle(LeanNameMangler.NameKind.LocalName, p.name)
          localScope += p.id -> n
          s"({${n} : ${emitExpr(p.tpe, mangle, localScope.toMap, typeEnv)}})"
        val params = sig.params.map: p =>
          val n = mangle.mangle(LeanNameMangler.NameKind.LocalName, p.name)
          localScope += p.id -> n
          s"(${n} : ${emitExpr(p.tpe, mangle, localScope.toMap, typeEnv)})"
        val returnType = emitExpr(sig.returnType, mangle, localScope.toMap, typeEnv)
        val bodyText = emitExpr(body, mangle, localScope.toMap, typeEnv)
        val recursion = emitRecursionClause(sig, body, mangle, localScope.toMap)
        val lines = Vector(
          s"def ${funName} ${(typeParams ++ params).mkString(" ")} : ${returnType} :=",
          s"  ${bodyText}"
        ) ++ recursion ++ Vector("")
        Some(DeclChunk(lines, Path.of(file), declSource(decl)))

  // Extracts the source range from a typed top-level declaration.
  private def declSource(decl: TypedAst.TopLevel): SourceRange =
    decl match
      case d: TypedAst.TopLevel.DataDecl => d.source
      case f: TypedAst.TopLevel.FunDecl => f.source

  // Renders implicit datatype parameter arguments at constructor result positions.
  private def renderTypeParamArgs(typeParams: List[LocalSymbol], mangle: LeanNameMangler.Context): String =
    if typeParams.isEmpty then ""
    else typeParams.map(p => s" (${mangle.mangle(LeanNameMangler.NameKind.LocalName, p.name)})").mkString

  // Emits a conservative termination clause for recursive functions.
  private def emitRecursionClause(
      sig: TypedAst.FunSig,
      body: TypedAst.Expr,
      mangle: LeanNameMangler.Context,
      localScope: Map[Int, String]
    ): Vector[String] =
    Vector.empty

  // Emits one typed expression into Lean syntax.
  private def emitExpr(
      expr: TypedAst.Expr,
      mangle: LeanNameMangler.Context,
      localScope: Map[Int, String],
      typeEnv: Map[String, TypedAst.Expr]
    ): String =
    expr match
      case TypedAst.Expr.Lit(value) =>
        value match
          case Literal.IntLit(v) => v
          case Literal.BoolLit(v) => if v then "true" else "false"
          case Literal.StringLit(v) => s"\"${escape(v)}\""
          case Literal.UnitLit() => "()"
      case TypedAst.Expr.Var(symbol) =>
        symbol match
          case local: TypedAst.LocalSymbol => localScope.getOrElse(local.id, mangle.mangle(LeanNameMangler.NameKind.LocalName, local.name))
          case global: GlobalSymbolSymbol => emitGlobalRef(global.name, mangle)
          case fun: TypedAst.FunctionSymbol => emitGlobalRef(fun.name, mangle)
          case ctor: TypedAst.CtorSymbol => emitGlobalRef(ctor.name, mangle)
          case dt: TypedAst.DatatypeSymbol => emitGlobalRef(dt.name, mangle)
          case err: TypedAst.ErrorSymbol => emitGlobalRef(err.name, mangle)
          case builtin: TypedAst.BuiltinValueSymbol => emitGlobalRef(builtin.name, mangle)
          case g: TypedAst.GlobalNameSymbol => emitGlobalRef(g.name, mangle)
      case TypedAst.Expr.App(callee, arg, _) =>
        s"(${emitExpr(callee, mangle, localScope, typeEnv)} ${emitExpr(arg, mangle, localScope, typeEnv)})"
      case TypedAst.Expr.AppImplicit(callee, arg, _) =>
        s"(${emitExpr(callee, mangle, localScope, typeEnv)} ${emitExpr(arg, mangle, localScope, typeEnv)})"
      case TypedAst.Expr.Pi(dom, cod, _) =>
        val domName = mangle.mangle(LeanNameMangler.NameKind.LocalName, dom.name)
        val scope2 = localScope + (dom.id -> domName)
        s"(${domName} : ${emitExpr(dom.tpe, mangle, localScope, typeEnv)}) -> ${emitExpr(cod, mangle, scope2, typeEnv)}"
      case TypedAst.Expr.Sort() => "Type"
      case TypedAst.Expr.Lambda(param, body, _) =>
        val paramName = mangle.mangle(LeanNameMangler.NameKind.LocalName, param.name)
        val scope2 = localScope + (param.id -> paramName)
        s"(fun (${paramName} : ${emitExpr(param.tpe, mangle, localScope, typeEnv)}) => ${emitExpr(body, mangle, scope2, typeEnv)})"
      case TypedAst.Expr.LetIn(symbol, _, declaredType, value, body) =>
        val localName = mangle.mangle(LeanNameMangler.NameKind.LocalName, symbol.name)
        val scope2 = localScope + (symbol.id -> localName)
        s"(let ${localName} : ${emitExpr(declaredType, mangle, localScope, typeEnv)} := ${emitExpr(value, mangle, localScope, typeEnv)}; ${emitExpr(body, mangle, scope2, typeEnv)})"
      case TypedAst.Expr.Meta(_, _) => "by trivial"
      case TypedAst.Expr.UnknownType() => "Type"
      case TypedAst.Expr.Match(scrutinee, _, cases) =>
        val scrut = emitExpr(scrutinee, mangle, localScope, typeEnv)
        val caseText = cases.map(c => s"| ${emitPattern(c.pattern, mangle, localScope)} => ${emitExpr(c.body, mangle, localScope, typeEnv)}").mkString(" ")
        s"(match ${scrut} with ${caseText})"

  // Emits one pattern branch into Lean syntax.
  private def emitPattern(pattern: TypedAst.Pattern, mangle: LeanNameMangler.Context, localScope: Map[Int, String]): String =
    pattern match
      case TypedAst.Pattern.Wildcard() => "_"
      case TypedAst.Pattern.Lit(value) =>
        value match
          case Literal.IntLit(v) => v
          case Literal.BoolLit(v) => if v then "true" else "false"
          case Literal.StringLit(v) => s"\"${escape(v)}\""
          case Literal.UnitLit() => "()"
      case TypedAst.Pattern.Binder(symbol) =>
        localScope.getOrElse(symbol.id, mangle.mangle(LeanNameMangler.NameKind.LocalName, symbol.name))
      case TypedAst.Pattern.Ctor(symbol, args) =>
        val ctorName = emitGlobalRef(symbol.name, mangle)
        if args.isEmpty then s".${ctorName}" else s"(.${ctorName} ${args.map(a => emitPattern(a, mangle, localScope)).mkString(" ")})"

  // Emits a global identifier while preserving Lean built-in names.
  private def emitGlobalRef(name: String, mangle: LeanNameMangler.Context): String =
    name match
      case "Int" | "Bool" | "String" | "Unit" | "Nat" => name
      case "True" => "true"
      case "False" => "false"
      case other => mangle.mangle(LeanNameMangler.NameKind.GlobalName, other)

  // Collects externally referenced globals and their types for axiom generation.
  private def collectExternalRefs(
      declarations: List[(String, TypedAst.TopLevel)],
      localTypes: Map[String, TypedAst.Expr]
    ): Map[String, TypedAst.Expr] =
    val refs = mutable.Map[String, TypedAst.Expr]()
    declarations.foreach: (_, decl) =>
      def addRefs(expr: TypedAst.Expr): Unit =
        collectGlobalRefs(expr).foreach: (name, tpe) =>
          if !localTypes.contains(name) && !isBuiltinGlobal(name) then
            { refs.getOrElseUpdate(name, tpe); () }
      decl match
        case TypedAst.TopLevel.DataDecl(symbol, typeParams, ctors) =>
          addRefs(symbol.tpe)
          typeParams.foreach(p => addRefs(p.tpe))
          ctors.foreach: ctor =>
            addRefs(ctor.symbol.tpe)
            ctor.fields.foreach(f => addRefs(f.tpe))
        case TypedAst.TopLevel.FunDecl(sig, body) =>
          addRefs(sig.symbol.tpe)
          sig.typeParams.foreach(p => addRefs(p.tpe))
          sig.params.foreach(p => addRefs(p.tpe))
          addRefs(sig.returnType)
          addRefs(body)
    refs.toMap

  // Checks whether a global name is provided by Lean directly.
  private def isBuiltinGlobal(name: String): Boolean =
    Set("Int", "Bool", "String", "Unit", "Nat", "Type", "True", "False").contains(name)

  // Collects globally referenced names and their types from an expression tree.
  private def collectGlobalRefs(expr: TypedAst.Expr): Map[String, TypedAst.Expr] =
    expr match
      case TypedAst.Expr.Lit(_) => Map.empty
      case TypedAst.Expr.Var(symbol) =>
        symbol match
          case g: GlobalSymbolSymbol => Map(g.name -> g.tpe) ++ collectGlobalRefs(g.tpe)
          case f: TypedAst.FunctionSymbol => Map(f.name -> f.tpe) ++ collectGlobalRefs(f.tpe)
          case c: TypedAst.CtorSymbol => Map(c.name -> c.tpe) ++ collectGlobalRefs(c.tpe)
          case d: TypedAst.DatatypeSymbol => Map(d.name -> d.tpe) ++ collectGlobalRefs(d.tpe)
          case g: TypedAst.GlobalNameSymbol =>
            val typeParam = TypedAst.LocalSymbol("A", TypedAst.Expr.Sort()(SourceRange.empty), -1)
            val kind = TypedAst.Expr.Pi(typeParam, TypedAst.Expr.Sort()(SourceRange.empty), isImplicit = false)(SourceRange.empty)
            Map(g.name -> kind)
          case _ => Map.empty
      case TypedAst.Expr.App(callee, arg, tpe) => collectGlobalRefs(callee) ++ collectGlobalRefs(arg) ++ collectGlobalRefs(tpe)
      case TypedAst.Expr.AppImplicit(callee, arg, tpe) => collectGlobalRefs(callee) ++ collectGlobalRefs(arg) ++ collectGlobalRefs(tpe)
      case TypedAst.Expr.Pi(dom, cod, _) => collectGlobalRefs(dom.tpe) ++ collectGlobalRefs(cod)
      case TypedAst.Expr.Sort() => Map.empty
      case TypedAst.Expr.Lambda(param, body, tpe) => collectGlobalRefs(param.tpe) ++ collectGlobalRefs(body) ++ collectGlobalRefs(tpe)
      case TypedAst.Expr.LetIn(_, _, declaredType, value, body) => collectGlobalRefs(declaredType) ++ collectGlobalRefs(value) ++ collectGlobalRefs(body)
      case TypedAst.Expr.Meta(_, tpe) => collectGlobalRefs(tpe)
      case TypedAst.Expr.UnknownType() => Map.empty
      case TypedAst.Expr.Match(scrutinee, motive, cases) =>
        collectGlobalRefs(scrutinee) ++ collectGlobalRefs(motive) ++ cases.flatMap(c => collectGlobalRefs(c.body)).toMap

  // Collects globally referenced names from an expression tree.
  private def collectGlobalNames(expr: TypedAst.Expr): Set[String] =
    expr match
      case TypedAst.Expr.Lit(_) => Set.empty
      case TypedAst.Expr.Var(symbol) =>
        symbol match
          case g: GlobalSymbolSymbol => Set(g.name)
          case f: TypedAst.FunctionSymbol => Set(f.name)
          case c: TypedAst.CtorSymbol => Set(c.name)
          case d: TypedAst.DatatypeSymbol => Set(d.name)
          case _ => Set.empty
      case TypedAst.Expr.App(callee, arg, tpe) => collectGlobalNames(callee) ++ collectGlobalNames(arg) ++ collectGlobalNames(tpe)
      case TypedAst.Expr.AppImplicit(callee, arg, tpe) => collectGlobalNames(callee) ++ collectGlobalNames(arg) ++ collectGlobalNames(tpe)
      case TypedAst.Expr.Pi(dom, cod, _) => collectGlobalNames(dom.tpe) ++ collectGlobalNames(cod)
      case TypedAst.Expr.Sort() => Set.empty
      case TypedAst.Expr.Lambda(param, body, tpe) => collectGlobalNames(param.tpe) ++ collectGlobalNames(body) ++ collectGlobalNames(tpe)
      case TypedAst.Expr.LetIn(_, _, declaredType, value, body) => collectGlobalNames(declaredType) ++ collectGlobalNames(value) ++ collectGlobalNames(body)
      case TypedAst.Expr.Meta(_, tpe) => collectGlobalNames(tpe)
      case TypedAst.Expr.UnknownType() => Set.empty
      case TypedAst.Expr.Match(scrutinee, motive, cases) =>
        collectGlobalNames(scrutinee) ++ collectGlobalNames(motive) ++ cases.flatMap(c => collectGlobalNames(c.body)).toSet

  // Escapes a string literal for Lean source output.
  private def escape(value: String): String =
    value
      .replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")

