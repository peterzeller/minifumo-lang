import * as generatedCompiler from './generated/minifumo-compiler'

interface CompilerModule {
  compileAndRun(source: string, functionName?: string, runFunction?: boolean): unknown
}

// Provides a typed adapter for the Scala.js-exported compiler module.
export const MinifumoCompiler = generatedCompiler.MinifumoCompiler as CompilerModule
