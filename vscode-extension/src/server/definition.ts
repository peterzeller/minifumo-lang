import { Location, Position, Range } from 'vscode-languageserver'
import { CompilerDefinitionLocation, findDefinition } from '../shared/compiler'
import { uriToCompilerPath } from './uri'
import { definitionUri } from './definition-uri'

/** Resolves a go-to-definition location for the given document position. */
export function computeDefinition(
  source: string,
  uri: string,
  position: Position,
  resolveDefinition: typeof findDefinition = findDefinition
): Location | null {
  const currentFile = uriToCompilerPath(uri)
  const definition = resolveDefinition(source, position.line + 1, position.character + 1, currentFile)
  if (definition === undefined) {
    return null
  }
  const targetUri = definitionUri(uri, currentFile, definition.file)
  return Location.create(targetUri, toRange(definition))
}

/** Converts a compiler 1-based range into an LSP-safe 0-based range. */
function toRange(definition: CompilerDefinitionLocation): Range {
  const startLine = Math.max(definition.line - 1, 0)
  const startCharacter = Math.max(definition.column - 1, 0)
  const endLine = Math.max(definition.endLine - 1, startLine)
  const endCharacter = Math.max(definition.endColumn, 0)
  const safeEndCharacter = endLine === startLine ? Math.max(endCharacter, startCharacter + 1) : endCharacter
  return {
    start: { line: startLine, character: startCharacter },
    end: { line: endLine, character: safeEndCharacter }
  }
}
