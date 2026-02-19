export const MinifumoCompiler = {
  // Placeholder compiler bridge used when Scala.js artifact is not synchronized yet.
  compileAndRun(source, functionName = 'main', runFunction = true) {
    return {
      success: false,
      output: '',
      errors: [
        {
          message:
            'Scala.js compiler artifact is not synced. Run "npm run compile:compiler" and "npm run sync:compiler".',
          line: 1,
          column: 1,
        },
      ],
      typed: false,
      executed: false,
    }
  },
}
