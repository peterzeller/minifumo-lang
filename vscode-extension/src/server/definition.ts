import { Location, Position, Range } from 'vscode-languageserver'
import { CompilerDefinitionLocation, findDefinition } from '../shared/compiler'
import { uriToCompilerPath } from './uri'

/** Resolves a go-to-definition location for the given document position. */
export function computeDefinition(source: string, uri: string, position: Position): Location | null {
  const currentFile = uriToCompilerPath(uri)
  const definition = findDefinition(source, position.line + 1, position.character + 1, currentFile)
  if (definition === undefined) {
    return null
  }
  let u = definitionUri(uri, currentFile, definition.file)
  console.log(`definition uri`, u)
  return Location.create(u, toRange(definition))
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

/** Resolves the URI to open for a definition target file. */
function definitionUri(currentUri: string, currentFile: string, targetFile: string): string {
  if (targetFile === currentFile || targetFile === '') {
    return currentUri
  }
  if (!currentUri.startsWith('file://')) {
    return currentUri
  }
  if (currentFile.endsWith('standard.minifumo')) {
    // return special virtual file for standard library
    return "minifumovirtual:/standard.minifumo"
  }
  try {
    if (targetFile.startsWith('/')) {
      return new URL(`file://${targetFile}`).toString()
    }
    return new URL(targetFile, currentUri).toString()
  } catch {
    return currentUri
  }
}
