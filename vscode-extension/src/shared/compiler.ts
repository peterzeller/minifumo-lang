import { MinifumoCompiler, type CompileError as CompilerError } from '../../../web/src/generated/minifumo-compiler.js'
export type { CompilerError }

/** Represents a 1-based source range returned from go-to-definition. */
export interface CompilerDefinitionLocation {
  file: string
  line: number
  column: number
  endLine: number
  endColumn: number
}

/** Compiles Minifumo source and returns normalized error objects. */
export function compileMinifumoSource(source: string): CompilerError[] {
  const result = MinifumoCompiler.compileAndRun(source, 'main', false)
  return result.errors
}

/** Finds a definition target for the given 1-based source position. */
export function findDefinition(source: string, line: number, column: number, currentFile: string): CompilerDefinitionLocation | undefined {
  return MinifumoCompiler.definitionAt(source, line, column, currentFile)
}
