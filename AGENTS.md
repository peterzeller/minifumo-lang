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

Use the following commands to install sbt locally (per the Codex environment setup) and run tests:

```bash
curl -L -o sbt-1.10.10.zip \
  https://github.com/sbt/sbt/releases/download/v1.10.10/sbt-1.10.10.zip
unzip sbt-1.10.10.zip
export PATH="$PWD/sbt/bin:$PATH"

sbt test
```

## Local coding rules
- Each function should have a brief comment explaining what it does.
- Avoid hard-coded strings for value kinds; prefer enums or typed representations.
