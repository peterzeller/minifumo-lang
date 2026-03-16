import { Hover, MarkupKind, Position } from 'vscode-languageserver'
import { findHover } from '../shared/compiler'
import { uriToCompilerPath } from './uri'

/** Computes hover markdown for a given source position. */
export function computeHover(source: string, uri: string, position: Position): Hover | null {
  const currentFile = uriToCompilerPath(uri)
  const info = findHover(source, position.line + 1, position.character + 1, currentFile)
  if (info === undefined) {
    return null
  }
  const lines: string[] = [`\`\`\`minifumo\n${info.typeText}\n\`\`\``]
  if (info.comment !== undefined && info.comment.trim() !== '') {
    lines.push(info.comment)
  }
  return {
    contents: {
      kind: MarkupKind.Markdown,
      value: lines.join('\n\n')
    }
  }
}
