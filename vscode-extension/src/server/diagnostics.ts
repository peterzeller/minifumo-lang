import { Diagnostic, DiagnosticSeverity } from 'vscode-languageserver'
import { compileMinifumoSource, CompilerError } from '../shared/compiler'

/** Compiles text and converts all compiler errors into LSP diagnostics. */
export function computeDiagnostics(source: string): Diagnostic[] {
  const errors = compileMinifumoSource(source)
  return errors.map((error) => compilerErrorToDiagnostic(source, error))
}

/** Converts a Minifumo compiler error into an LSP diagnostic. */
function compilerErrorToDiagnostic(source: string, error: CompilerError): Diagnostic {
  const lineIndex = clampLine(source, error.line - 1)
  const startCharacter = clampColumn(source, lineIndex, error.column - 1)
  const endCharacter = clampColumn(source, lineIndex, Math.max(error.endColumn - 1, error.column))
  return {
    severity: DiagnosticSeverity.Error,
    source: 'minifumo',
    message: error.message,
    range: {
      start: { line: lineIndex, character: startCharacter },
      end: { line: lineIndex, character: Math.max(endCharacter, startCharacter + 1) }
    }
  }
}

/** Clamps a 0-based line index to the source bounds. */
function clampLine(source: string, lineIndex: number): number {
  const lines = source.split(/\r?\n/)
  const maxLine = Math.max(lines.length - 1, 0)
  return Math.min(Math.max(lineIndex, 0), maxLine)
}

/** Clamps a 0-based column index to the line bounds. */
function clampColumn(source: string, lineIndex: number, columnIndex: number): number {
  const lineLength = getLineLength(source, lineIndex)
  return Math.min(Math.max(columnIndex, 0), lineLength)
}

/** Returns the character length of a specific 0-based source line. */
function getLineLength(source: string, lineIndex: number): number {
  const lines = source.split(/\r?\n/)
  const line = lines[lineIndex]
  if (line === undefined) {
    return 0
  }
  return line.length
}
