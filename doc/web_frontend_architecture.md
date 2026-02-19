# Minifumo Web Frontend Architecture

## Goal
The `web` project provides a mobile-friendly browser playground for Minifumo with:
- a large code editor,
- a compile button,
- optional execution of a selected function (`main` by default),
- compile/runtime output shown directly in the page.

## Code locations
- Frontend app (React + TypeScript + Vite): `web/src`
  - Main UI: `web/src/App.tsx`
  - CodeMirror Minifumo syntax highlighting: `web/src/minifumoLanguage.ts`
  - Scala.js bridge adapter: `web/src/compiler.ts`
- Scala.js compiler bridge: `compiler-js/src/main/scala/com/github/peterzeller/minifumo/web/CompilerApi.scala`
- Shared compiler/runtime source used by both JVM and JS builds: `src/main/scala`
- Standard library resource packaged for JVM and Scala.js: `src/main/resources/com/github/peterzeller/minifumo/builtins/standard.minifumo`

## Build linkage
1. `sbt compilerJs/fastLinkJS` compiles the Scala compiler/interpreter to JavaScript.
2. `web/scripts/sync-compiler.mjs` copies the generated Scala.js `main.js` into `web/src/generated/minifumo-compiler.js`.
3. The React app imports this generated module and calls `MinifumoCompiler.compileAndRun(...)`.

`web/package.json` automates steps 1 and 2 via `predev` and `prebuild`, so `npm run dev` and `npm run build` always refresh the compiler artifact first.

## Runtime flow in the browser
1. User edits source code in CodeMirror.
2. User presses **Compile**.
3. `App.tsx` calls `MinifumoCompiler.compileAndRun(source, functionName, runFunction)`.
4. Scala.js bridge (`CompilerApi.scala`) runs parse -> typecheck -> optional interpretation.
5. The bridge returns a structured result object (`success`, `output`, `errors`, `typed`, `executed`).
6. Frontend renders the output or errors.

## Current limitations
- Import resolution is disabled in browser mode (the bridge uses an in-memory empty import cache).
- Browser execution uses the bundled standard library only.
