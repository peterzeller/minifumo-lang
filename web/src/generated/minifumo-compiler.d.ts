/**
 * Exposes the Scala.js compiler bridge used by the web playground.
 */
export interface CompileError {
  message: string
  line: number
  column: number
  endColumn: number
}

/**
 * Represents the structured compiler response returned by Scala.js.
 */
export interface CompileResult {
  success: boolean
  output: string
  errors: CompileError[]
  typed: boolean
  executed: boolean
}

/**
 * Represents one go-to-definition target returned by Scala.js.
 */
export interface DefinitionLocation {
  file: string
  line: number
  column: number
  endLine: number
  endColumn: number
}

export const MinifumoCompiler: {
  standardLibrarySource(): string
  compileAndRun(source: string, functionName?: string, runFunction?: boolean): CompileResult
  definitionAt(source: string, line: number, column: number, currentFile?: string): DefinitionLocation | undefined
}
