import { basicSetup, EditorView } from 'codemirror'
import { useEffect, useMemo, useRef, useState } from 'react'
import { oneDark } from '@codemirror/theme-one-dark'
import { minifumoLanguage } from './minifumoLanguage'
import { MinifumoCompiler } from './compiler'

interface CompileError {
  message: string
  line: number
  column: number
  endColumn?: number
}

interface CompileResult {
  success: boolean
  output: string
  errors: CompileError[]
  typed: boolean
  executed: boolean
}

const starterProgram = `fun main(): Int
  let x = 21 in x + x`

// Renders a compiler error with source excerpt and caret underline for the output box.
function formatCompileError(source: string, error: CompileError): string {
  const sourceLines = source.split('\n')
  const lineIndex = Math.max(0, error.line - 1)
  const sourceLine = sourceLines[lineIndex]
  const location = `Line ${error.line}, Col ${error.column}`

  if (!sourceLine) {
    return `${location}: ${error.message}`
  }

  const startColumn = Math.max(1, error.column)
  const endColumn = Math.max(startColumn + 1, error.endColumn ?? startColumn + 1)
  const underline = `${' '.repeat(startColumn - 1)}${'^'.repeat(endColumn - startColumn)}`

  return `${location}\n${sourceLine}\n${underline}\n${error.message}`
}

// Renders the mobile-friendly Minifumo browser playground UI.
export function App() {
  const editorContainerRef = useRef<HTMLDivElement | null>(null)
  const editorViewRef = useRef<EditorView | null>(null)
  const [output, setOutput] = useState('')
  const [functionName, setFunctionName] = useState('main')
  const [shouldRun, setShouldRun] = useState(true)

  // Creates the CodeMirror extension list once for editor initialization.
  const editorExtensions = useMemo(
    () => [basicSetup, minifumoLanguage, oneDark, EditorView.lineWrapping],
    [],
  )

  // Initializes and tears down the CodeMirror editor instance.
  useEffect(() => {
    if (!editorContainerRef.current) {
      return
    }

    const editorView = new EditorView({
      doc: starterProgram,
      extensions: editorExtensions,
      parent: editorContainerRef.current,
    })

    editorViewRef.current = editorView

    return () => {
      editorView.destroy()
      editorViewRef.current = null
    }
  }, [editorExtensions])

  // Compiles the current editor content and optionally executes the configured function.
  const compileCode = () => {
    if (!editorViewRef.current) {
      return
    }

    const source = editorViewRef.current.state.doc.toString()
    try {
      const result = MinifumoCompiler.compileAndRun(source, functionName || 'main', shouldRun) as CompileResult

      if (result.success) {
        setOutput(result.output)
        return
      }

      const formattedErrors = result.errors.map((error) => formatCompileError(source, error)).join('\n\n')
      setOutput(formattedErrors)
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : String(error)
      setOutput(`Unexpected frontend error while compiling:\n${errorMessage}`)
    }
  }

  return (
    <main className="page">
      <h1>Minifumo Web Playground</h1>
      <p className="subtitle">Compile and run Minifumo programs directly in your browser.</p>

      <div className="controls">
        <label>
          Function name
          <input
            value={functionName}
            onChange={(event) => setFunctionName(event.target.value)}
            placeholder="main"
          />
        </label>
        <label className="checkbox">
          <input
            type="checkbox"
            checked={shouldRun}
            onChange={(event) => setShouldRun(event.target.checked)}
          />
          Run after compile
        </label>
        <button onClick={compileCode}>Compile</button>
      </div>

      <section className="editorPanel">
        <div ref={editorContainerRef} className="editor" />
      </section>

      <section>
        <h2>Output</h2>
        <textarea readOnly value={output} className="output" />
      </section>
    </main>
  )
}
