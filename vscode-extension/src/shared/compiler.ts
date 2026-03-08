import { MinifumoCompiler, type CompileError as CompilerError } from '../../../web/src/generated/minifumo-compiler.js'
export type { CompilerError }

/** Compiles Minifumo source and returns normalized error objects. */
export function compileMinifumoSource(source: string): CompilerError[] {
  const result = MinifumoCompiler.compileAndRun(source, 'main', false)
  return result.errors
}
