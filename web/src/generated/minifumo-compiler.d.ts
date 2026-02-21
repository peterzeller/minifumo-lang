/**
 * Exposes the Scala.js compiler bridge used by the web playground.
 */
export const MinifumoCompiler: {
  compileAndRun(source: string, functionName?: string, runFunction?: boolean): unknown
}
