# Architecture and Project Map (for Codex)

## Overview
Minifumo is a language implementation in Scala 3. The core flow is:
grammar -> parser -> AST -> type checker -> interpreter.
Builtins are defined in both Scala and Minifumo source files.

## Key entry points
- Interpreter: `src/main/scala/com/github/peterzeller/minifumo/interpreter/Interpreter.scala`
- Type checking: `src/main/scala/com/github/peterzeller/minifumo/typing/TypeChecker.scala`
- Builtins (Scala side): `src/main/scala/com/github/peterzeller/minifumo/builtins/standard.scala`
- Builtins (Minifumo source): `src/main/scala/com/github/peterzeller/minifumo/builtins/standard.minifumo`

## Directory map
- AST definitions: `src/main/scala/com/github/peterzeller/minifumo/ast`
- Parser: `src/main/scala/com/github/peterzeller/minifumo/parser`
- Lexer: `src/main/scala/com/github/peterzeller/minifumo/lexer`
- Typing: `src/main/scala/com/github/peterzeller/minifumo/typing`
- Interpreter: `src/main/scala/com/github/peterzeller/minifumo/interpreter`
- Builtins: `src/main/scala/com/github/peterzeller/minifumo/builtins`

## Grammar and generated sources
- ANTLR grammar: `src/main/antlr4/Minifumo.g4`
- Generated ANTLR sources: `target/scala-*/src_managed/main/antlr4`

## Build notes
- Build config: `build.sbt`
- ANTLR generation task: `generateAntlr` in `build.sbt`

## Development setup (Codex)
Always run the full test suite (`sbt test`) before finalizing changes.
Always run `sbt scalafix` before committing changes.
Ensure `sbt` is manually added to `PATH` before running build or test commands.


## Local coding rules
- Each function should have a brief comment explaining what it does.
- Avoid hard-coded strings for value kinds; prefer enums or typed representations.


## Web frontend map
- React + TypeScript web playground: `web`
- Scala.js compiler bridge sources: `compiler-js/src/main/scala/com/github/peterzeller/minifumo/web`
- Architecture doc for browser integration: `doc/web_frontend_architecture.md`
- Generated compiler artifact consumed by frontend: `web/src/generated/minifumo-compiler.js`
