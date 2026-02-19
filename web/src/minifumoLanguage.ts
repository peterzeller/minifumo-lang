import { StreamLanguage, type StreamParser } from '@codemirror/language'

const KEYWORDS = new Set([
  'fun',
  'data',
  'export',
  'if',
  'then',
  'else',
  'let',
  'in',
  'match',
  'case',
  'forall',
  'import',
  'from',
])

// Defines a lightweight token stream parser to syntax-highlight Minifumo source code.
const parser: StreamParser<unknown> = {
  startState: () => ({}),
  token(stream) {
    if (stream.eatSpace()) {
      return null
    }

    if (stream.match(/--.*/)) {
      return 'comment'
    }

    if (stream.match(/"([^"\\]|\\.)*"/)) {
      return 'string'
    }

    if (stream.match(/[0-9]+/)) {
      return 'number'
    }

    if (stream.match(/[A-Za-z_][A-Za-z0-9_]*/)) {
      return KEYWORDS.has(stream.current()) ? 'keyword' : 'variableName'
    }

    if (stream.match(/(->|=>|==|!=|<=|>=|&&|\|\|)/)) {
      return 'operator'
    }

    stream.next()
    return 'punctuation'
  },
}

// Creates the CodeMirror language extension used by the editor.
export const minifumoLanguage = StreamLanguage.define(parser)
