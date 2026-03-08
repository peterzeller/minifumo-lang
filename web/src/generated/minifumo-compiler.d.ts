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

export const MinifumoCompiler: {
  compileAndRun(source: string, functionName?: string, runFunction?: boolean): CompileResult
}
