package com.github.peterzeller.minifumo.backends.lean

import com.github.peterzeller.minifumo.ast.{Literal, SourceRange}
import com.github.peterzeller.minifumo.backends.lean.LeanBackend.{GeneratedLeanFile, SourceMapEntry}
import com.github.peterzeller.minifumo.typing.{GlobalSymbol, ProjectSymbolCache, TypedAst}
import com.github.peterzeller.minifumo.typing.TypedAst.LocalSymbol

import java.nio.file.Path
import scala.collection.mutable

object LeanEmitter:
  // Enumerates special Minifumo globals that map directly to Lean primitives.
  private enum LeanPrimitiveGlobal(val minifumoName: String, val leanName: String):
    case EqType extends LeanPrimitiveGlobal("Eq", "(fun (T : Type) (a : T) (b : T) => Eq a b)")
    case Refl extends LeanPrimitiveGlobal("refl", "(fun (T : Type) (x : T) => Eq.refl x)")
    case CongrArg extends LeanPrimitiveGlobal("congrArg", "(fun (T : Type) (U : Type) (x : T) (y : T) (f : T -> U) (h : Eq x y) => congrArg f h)")
    case IntNeg extends LeanPrimitiveGlobal("opNeg", "Int.neg")
    case IntAdd extends LeanPrimitiveGlobal("opPlus", "Int.add")
    case IntSub extends LeanPrimitiveGlobal("opMinus", "Int.sub")
    case IntMul extends LeanPrimitiveGlobal("opTimes", "Int.mul")
    case IntDiv extends LeanPrimitiveGlobal("opDiv", "Int.ediv")
    case IntMod extends LeanPrimitiveGlobal("opMod", "Int.emod")
    case IntLt extends LeanPrimitiveGlobal("opLt", "Int.lt")
    case IntLe extends LeanPrimitiveGlobal("opLe", "Int.le")
    case NatAdd extends LeanPrimitiveGlobal("natAdd", "Nat.add")
    case BoolAnd extends LeanPrimitiveGlobal("opAnd", "Bool.and")
    case BoolOr extends LeanPrimitiveGlobal("opOr", "Bool.or")
    case Println extends LeanPrimitiveGlobal("println", "(fun _ _ _ => ())")
    case ShowInt extends LeanPrimitiveGlobal("showInt", "(fun (x : Int) => toString x)")

  // Looks up direct Lean primitive mappings for selected globals.
  private def leanPrimitiveName(name: String): Option[String] =
    LeanPrimitiveGlobal.values.find(_.minifumoName == name).map(_.leanName)

  // Represents one emitted declaration snippet with source span info.
  private final case class DeclChunk(lines: Vector[String], sourceFile: Path, sourceRange: SourceRange)

  // Emits one Lean module for a group of Minifumo files.
  def emitModule(
      moduleName: String,
      imports: List[String],
      files: List[String],
      cache: ProjectSymbolCache,
      moduleNameByFile: Map[String, String]
    ): GeneratedLeanFile =
    val mangle = new LeanNameMangler.Context()
    val chunks = mutable.ListBuffer[DeclChunk]()
    val localDefinitions = collectLocalDefinitions(files, cache).map(_.name)

    // Collect typed declarations first so dependency sorting can include all local names.
    val declarations = files.flatMap: file =>
      val (program, _) = cache.typedAst(file)
      program.items.map(item => (file, item))

    val depGraph = buildDeclDependencyGraph(declarations)
    val orderedGroups = LeanDependencyPlanner.topologicalSccs(depGraph)
    val declarationByKey = declarations.map((f, d) => declKey(f, d) -> (f, d)).toMap

    orderedGroups.foreach: group =>
      group.toList.sorted.foreach: key =>
        declarationByKey.get(key).foreach: (file, decl) =>
          emitDecl(file, decl, mangle, moduleName, localDefinitions, moduleNameByFile).foreach(chunks += _)

    val importLines = imports.map(i => s"import ${i}").toVector
    val header =
      if importLines.isEmpty then
        Vector(s"namespace ${moduleName}", "", "set_option autoImplicit true", "")
      else
        importLines ++ Vector("", s"namespace ${moduleName}", "", "set_option autoImplicit true", "")
    val footer = Vector("", s"end ${moduleName}")
    val bodyLines = chunks.flatMap(_.lines).toVector
    val contentLines = header ++ bodyLines ++ footer

    val lineMap = mutable.ListBuffer[SourceMapEntry]()
    var lineCursor = header.length + 1
    chunks.foreach: chunk =>
      val start = lineCursor
      val end = lineCursor + chunk.lines.length - 1
      lineMap += SourceMapEntry(start, end, chunk.sourceFile, chunk.sourceRange)
      lineCursor = end + 1

    GeneratedLeanFile(Path.of(s"${moduleName}.lean"), contentLines.mkString("\n"), lineMap.toVector)

  // Collects all declarations defined inside this emitted module.
  private def collectLocalDefinitions(files: List[String], cache: ProjectSymbolCache): Set[GlobalSymbol] =
    files.flatMap: file =>
      val (program, _) = cache.typedAst(file)
      program.items.flatMap:
        case TypedAst.TopLevel.DataDecl(symbol, _, ctors) => symbol.sym :: ctors.map(_.symbol.sym)
        case TypedAst.TopLevel.FunDecl(sig, _) => List(sig.symbol.sym)
    .toSet

  // Creates a stable declaration key for dependency sorting.
  private def declKey(file: String, decl: TypedAst.TopLevel): String =
    decl match
      case TypedAst.TopLevel.DataDecl(symbol, _, _) => s"${file}::${symbol.name}"
      case TypedAst.TopLevel.FunDecl(sig, _) => s"${file}::${sig.symbol.name}"

  // Builds a dependency graph between top-level declarations as dependency -> dependent edges.
  private def buildDeclDependencyGraph(declarations: List[(String, TypedAst.TopLevel)]): Map[String, Set[String]] =
    val keyByName: Map[String, String] = declarations.map((file, decl) =>
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
      currentModule: String,
      localDefinitions: Set[String],
      moduleNameByFile: Map[String, String]
    ): Option[DeclChunk] =
    decl match
      case TypedAst.TopLevel.DataDecl(symbol, typeParams, ctors) =>
        val dataName = mangle.mangle(LeanNameMangler.NameKind.GlobalName, symbol.name)
        val params = typeParams
          .map(param => s"(${mangle.mangle(LeanNameMangler.NameKind.LocalName, param.name)} : ${emitExpr(param.tpe, mangle, Map.empty, currentModule, localDefinitions, moduleNameByFile)})")
          .mkString(" ")
        val ctorLines = ctors.map: ctor =>
          val ctorName = mangle.mangle(LeanNameMangler.NameKind.GlobalName, ctor.symbol.name)
          if ctor.fields.isEmpty then
            s"| ${ctorName} : ${dataName}${renderTypeParamArgs(typeParams, mangle)}"
          else
            val fields = ctor.fields
              .map(field => emitExpr(field.tpe, mangle, Map.empty, currentModule, localDefinitions, moduleNameByFile))
              .mkString(" -> ")
            s"| ${ctorName} : ${fields} -> ${dataName}${renderTypeParamArgs(typeParams, mangle)}"
        val lines = Vector(s"inductive ${dataName} ${params} : Type where") ++ ctorLines.toVector :+ ""
        Some(DeclChunk(lines, Path.of(file), declSource(decl)))

      case TypedAst.TopLevel.FunDecl(sig, body) =>
        val funName = mangle.mangle(LeanNameMangler.NameKind.GlobalName, sig.symbol.name)
        val localScope = mutable.Map[Int, String]()
        val typeParams = sig.typeParams.map: p =>
          val n = mangle.mangle(LeanNameMangler.NameKind.LocalName, p.name)
          localScope += p.id -> n
          s"(${n} : ${emitExpr(p.tpe, mangle, localScope.toMap, currentModule, localDefinitions, moduleNameByFile)})"
        val params = sig.params.map: p =>
          val n = mangle.mangle(LeanNameMangler.NameKind.LocalName, p.name)
          localScope += p.id -> n
          s"(${n} : ${emitExpr(p.tpe, mangle, localScope.toMap, currentModule, localDefinitions, moduleNameByFile)})"
        val returnType = emitExpr(sig.returnType, mangle, localScope.toMap, currentModule, localDefinitions, moduleNameByFile)
        val bodyText = emitExpr(body, mangle, localScope.toMap, currentModule, localDefinitions, moduleNameByFile)
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
      currentModule: String,
      localDefinitions: Set[String],
      moduleNameByFile: Map[String, String]
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
          case fun: TypedAst.FunctionSymbol => emitGlobalRef(fun.name, None, mangle, currentModule, localDefinitions, moduleNameByFile)
          case ctor: TypedAst.CtorSymbol => emitGlobalRef(ctor.name, None, mangle, currentModule, localDefinitions, moduleNameByFile)
          case dt: TypedAst.DatatypeSymbol => emitGlobalRef(dt.name, Some(dt.sym.file), mangle, currentModule, localDefinitions, moduleNameByFile)
          case err: TypedAst.ErrorSymbol => emitGlobalRef(err.name, None, mangle, currentModule, localDefinitions, moduleNameByFile)
          case builtin: TypedAst.BuiltinValueSymbol => emitGlobalRef(builtin.name, None, mangle, currentModule, localDefinitions, moduleNameByFile)
      case TypedAst.Expr.App(callee, arg, _) =>
        s"(${emitExpr(callee, mangle, localScope, currentModule, localDefinitions, moduleNameByFile)} ${emitExpr(arg, mangle, localScope, currentModule, localDefinitions, moduleNameByFile)})"
      case TypedAst.Expr.AppImplicit(callee, arg, _) =>
        val calleeText = emitExpr(callee, mangle, localScope, currentModule, localDefinitions, moduleNameByFile)
        val explicitCallee =
          if calleeText.startsWith("(") || calleeText.startsWith("fun ") || calleeText.startsWith("@") then calleeText
          else s"@${calleeText}"
        s"(${explicitCallee} ${emitExpr(arg, mangle, localScope, currentModule, localDefinitions, moduleNameByFile)})"
      case TypedAst.Expr.Pi(dom, cod, _) =>
        val domName = mangle.mangle(LeanNameMangler.NameKind.LocalName, dom.name)
        val scope2 = localScope + (dom.id -> domName)
        s"(${domName} : ${emitExpr(dom.tpe, mangle, localScope, currentModule, localDefinitions, moduleNameByFile)}) -> ${emitExpr(cod, mangle, scope2, currentModule, localDefinitions, moduleNameByFile)}"
      case TypedAst.Expr.Sort() => "Type"
      case TypedAst.Expr.Lambda(param, body, _) =>
        val paramName = mangle.mangle(LeanNameMangler.NameKind.LocalName, param.name)
        val scope2 = localScope + (param.id -> paramName)
        s"(fun (${paramName} : ${emitExpr(param.tpe, mangle, localScope, currentModule, localDefinitions, moduleNameByFile)}) => ${emitExpr(body, mangle, scope2, currentModule, localDefinitions, moduleNameByFile)})"
      case TypedAst.Expr.LetIn(symbol, _, declaredType, value, body) =>
        val localName = mangle.mangle(LeanNameMangler.NameKind.LocalName, symbol.name)
        val scope2 = localScope + (symbol.id -> localName)
        s"(let ${localName} : ${emitExpr(declaredType, mangle, localScope, currentModule, localDefinitions, moduleNameByFile)} := ${emitExpr(value, mangle, localScope, currentModule, localDefinitions, moduleNameByFile)}; ${emitExpr(body, mangle, scope2, currentModule, localDefinitions, moduleNameByFile)})"
      case TypedAst.Expr.Meta(_, _) => "by trivial"
      case TypedAst.Expr.UnknownType() => "Type"
      case TypedAst.Expr.Match(scrutinee, _, cases) =>
        val scrut = emitExpr(scrutinee, mangle, localScope, currentModule, localDefinitions, moduleNameByFile)
        val caseText = cases
          .map(c => s"| ${emitPattern(c.pattern, mangle, localScope, currentModule, localDefinitions, moduleNameByFile)} => ${emitExpr(c.body, mangle, localScope, currentModule, localDefinitions, moduleNameByFile)}")
          .mkString(" ")
        s"(match ${scrut} with ${caseText})"

  // Emits one pattern branch into Lean syntax.
  private def emitPattern(
      pattern: TypedAst.Pattern,
      mangle: LeanNameMangler.Context,
      localScope: Map[Int, String],
      currentModule: String,
      localDefinitions: Set[String],
      moduleNameByFile: Map[String, String]
    ): String =
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
        val ctorName = emitGlobalRef(symbol.name, None, mangle, currentModule, localDefinitions, moduleNameByFile)
        if ctorName.contains(".") then
          if args.isEmpty then ctorName else s"(${ctorName} ${args.map(a => emitPattern(a, mangle, localScope, currentModule, localDefinitions, moduleNameByFile)).mkString(" ")})"
        else if args.isEmpty then s".${ctorName}" else s"(.${ctorName} ${args.map(a => emitPattern(a, mangle, localScope, currentModule, localDefinitions, moduleNameByFile)).mkString(" ")})"

  // Emits a global identifier while preserving Lean built-in names and imported namespaces.
  private def emitGlobalRef(
      name: String,
      sourceFile: Option[String],
      mangle: LeanNameMangler.Context,
      currentModule: String,
      localDefinitions: Set[String],
      moduleNameByFile: Map[String, String]
    ): String =
    name match
      case "Int" | "Bool" | "String" | "Unit" | "Nat" => name
      case "True" => "true"
      case "False" => "false"
      case other =>
        leanPrimitiveName(other).getOrElse:
          val rendered = mangle.mangle(LeanNameMangler.NameKind.GlobalName, other)
          if localDefinitions.contains(other) then
            rendered
          else
            sourceFile
              .flatMap(path => moduleNameByFile.get(path.toString.replace('\\', '/')))
              .filter(_ != currentModule)
              .map(module => s"${module}.${rendered}")
              .getOrElse(rendered)

  // Collects globally referenced names from an expression tree.
  private def collectGlobalNames(expr: TypedAst.Expr): Set[String] =
    expr match
      case TypedAst.Expr.Lit(_) => Set.empty
      case TypedAst.Expr.Var(symbol) =>
        symbol match
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
