import { MinifumoCompiler } from '../../../web/src/generated/minifumo-compiler.js'

/** Represents one compiler error returned by the Scala.js bridge. */
export interface CompilerError {
  message: string
  line: number
  column: number
  endColumn: number
}

/** Represents the relevant subset of the Scala.js compiler response. */
interface CompilerResult {
  errors?: unknown
}

/** Compiles Minifumo source and returns normalized error objects. */
export function compileMinifumoSource(source: string): CompilerError[] {
  const rawResult = MinifumoCompiler.compileAndRun(source, 'main', false) as CompilerResult
  return normalizeCompilerErrors(rawResult.errors)
}

/** Normalizes unknown compiler error payloads into the expected structure. */
function normalizeCompilerErrors(errors: unknown): CompilerError[] {
  if (!Array.isArray(errors)) {
    return []
  }
  return errors
    .map(normalizeCompilerError)
    .filter((error): error is CompilerError => error !== null)
}

/** Normalizes a single compiler error object if it has the expected shape. */
function normalizeCompilerError(error: unknown): CompilerError | null {
  if (error === null || typeof error !== 'object') {
    return null
  }
  const candidate = error as Record<string, unknown>
  if (
    typeof candidate.message !== 'string' ||
    typeof candidate.line !== 'number' ||
    typeof candidate.column !== 'number' ||
    typeof candidate.endColumn !== 'number'
  ) {
    return null
  }
  return {
    message: candidate.message,
    line: candidate.line,
    column: candidate.column,
    endColumn: candidate.endColumn
  }
}
