import { StreamLanguage, type StreamParser } from '@codemirror/language'

interface ParserState {
  inBlockComment: boolean
}

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
const parser: StreamParser<ParserState> = {
  // Initializes parser state used to track multi-line block comments.
  startState: (): ParserState => ({ inBlockComment: false }),
  // Produces one syntax token class for the current stream position.
  token(stream, state: ParserState) {
    if (state.inBlockComment) {
      while (!stream.eol()) {
        if (stream.match('*/')) {
          state.inBlockComment = false
          break
        }
        stream.next()
      }
      return 'comment'
    }

    if (stream.eatSpace()) {
      return null
    }

    if (stream.match(/\/\/.*/)) {
      return 'comment'
    }

    if (stream.match('/*')) {
      state.inBlockComment = true
      while (!stream.eol()) {
        if (stream.match('*/')) {
          state.inBlockComment = false
          break
        }
        stream.next()
      }
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
