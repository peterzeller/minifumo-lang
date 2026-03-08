import * as generatedCompiler from './generated/minifumo-compiler.js'
import type { CompileResult } from './generated/minifumo-compiler.js'

interface CompilerModule {
  compileAndRun(source: string, functionName?: string, runFunction?: boolean): CompileResult
}

// Provides a typed adapter for the Scala.js-exported compiler module.
export const MinifumoCompiler = generatedCompiler.MinifumoCompiler as CompilerModule
