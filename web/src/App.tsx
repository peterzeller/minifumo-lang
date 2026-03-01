import { basicSetup, EditorView } from 'codemirror'
import { useEffect, useMemo, useRef, useState } from 'react'
import { oneDark } from '@codemirror/theme-one-dark'
import { minifumoLanguage } from './minifumoLanguage'
import { MinifumoCompiler } from './compiler'
import { tutorialSections, type TutorialSection } from './tutorial'

type Tab = 'playground' | 'tutorial'

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

// Runs the compiler call while capturing browser console output produced by Scala.js println helpers.
function runCompilerWithCapturedConsole(source: string, functionName: string, shouldRun: boolean): {
  result: CompileResult
  consoleLines: string[]
} {
  const consoleLines: string[] = []
  const originalLog = console.log

  console.log = (...args: unknown[]) => {
    const renderedLine = args.map((arg) => (typeof arg === 'string' ? arg : String(arg))).join(' ')
    consoleLines.push(renderedLine)
    originalLog(...args)
  }

  try {
    const result = MinifumoCompiler.compileAndRun(source, functionName || 'main', shouldRun) as CompileResult
    return { result, consoleLines }
  } finally {
    console.log = originalLog
  }
}

// Compiles Minifumo source code and returns combined output text for display.
async function compileSource(source: string, functionName: string, shouldRun: boolean): Promise<string> {
  const { result, consoleLines } = runCompilerWithCapturedConsole(source, functionName, shouldRun)

  if (result.success) {
    return [...consoleLines, result.output].filter((section) => section.length > 0).join('\n')
  }

  const formattedErrors = result.errors.map((error) => formatCompileError(source, error)).join('\n\n')
  return [...consoleLines, formattedErrors].filter((section) => section.length > 0).join('\n\n')
}

// Renders one runnable tutorial code sample with compile and run output.
function TutorialExampleCard({ section }: { section: TutorialSection }) {
  const [outputs, setOutputs] = useState<Record<string, string>>({})

  // Compiles a tutorial snippet and stores the output in local component state.
  const runExample = async (exampleId: string, source: string, functionName: string, shouldRun: boolean) => {
    setOutputs((previous) => ({ ...previous, [exampleId]: 'Compiling...' }))
    await new Promise((resolve) => window.setTimeout(resolve, 0))
    try {
      const output = await compileSource(source, functionName, shouldRun)
      setOutputs((previous) => ({ ...previous, [exampleId]: output }))
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : String(error)
      setOutputs((previous) => ({ ...previous, [exampleId]: `Unexpected frontend error while compiling:\n${errorMessage}` }))
    }
  }

  return (
    <article className="tutorialSection" key={section.id}>
      <h2>{section.title}</h2>
      <p className="subtitle">{section.content}</p>
      {section.examples.map((example) => (
        <div className="tutorialExample" key={example.id}>
          <h3>{example.title}</h3>
          <pre className="tutorialCode">{example.source}</pre>
          <button
            onClick={() => runExample(example.id, example.source, example.functionName, example.shouldRun)}
            className="tutorialRunButton"
          >
            Run example
          </button>
          <textarea
            readOnly
            value={outputs[example.id] ?? ''}
            className="output"
            aria-label={`Output for ${example.title}`}
          />
        </div>
      ))}
    </article>
  )
}

// Renders the mobile-friendly Minifumo browser playground UI.
export function App() {
  const editorContainerRef = useRef<HTMLDivElement | null>(null)
  const editorViewRef = useRef<EditorView | null>(null)
  const [output, setOutput] = useState('')
  const [functionName, setFunctionName] = useState('main')
  const [shouldRun, setShouldRun] = useState(true)
  const [activeTab, setActiveTab] = useState<Tab>('playground')

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
  const compileCode = async () => {
    if (!editorViewRef.current) {
      return
    }

    const source = editorViewRef.current.state.doc.toString()
    setOutput('Compiling...')

    // Yields to the browser so the compile status text renders before heavy work starts.
    await new Promise((resolve) => window.setTimeout(resolve, 0))

    try {
      setOutput(await compileSource(source, functionName, shouldRun))
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : String(error)
      setOutput(`Unexpected frontend error while compiling:\n${errorMessage}`)
    }
  }

  return (
    <main className="page">
      <h1>Minifumo Web Playground</h1>
      <p className="subtitle">Compile and run Minifumo programs directly in your browser.</p>

      <div className="tabs" role="tablist" aria-label="Minifumo website sections">
        <button onClick={() => setActiveTab('playground')} className={activeTab === 'playground' ? 'tab activeTab' : 'tab'}>
          Playground
        </button>
        <button onClick={() => setActiveTab('tutorial')} className={activeTab === 'tutorial' ? 'tab activeTab' : 'tab'}>
          Tutorial
        </button>
      </div>

      {activeTab === 'playground' ? (
        <>
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
        </>
      ) : (
        <section>
          <h2>Tutorial</h2>
          <p className="subtitle">Draft tutorial infrastructure for GitHub Pages with runnable examples.</p>
          {tutorialSections.map((section) => (
            <TutorialExampleCard key={section.id} section={section} />
          ))}
        </section>
      )}
    </main>
  )
}
