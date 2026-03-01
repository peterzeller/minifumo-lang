import { basicSetup, EditorView } from 'codemirror'
import { useEffect, useMemo, useRef, useState } from 'react'
import { oneDark } from '@codemirror/theme-one-dark'
import { minifumoLanguage } from './minifumoLanguage'
import { MinifumoCompiler } from './compiler'
import { tutorialPages, tutorialPagesById, type TutorialBlock, type TutorialCodeInclude, type TutorialPage } from './tutorial'

type Tab = 'playground' | 'tutorial'
type Theme = 'light' | 'dark'

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

// Renders one runnable tutorial code sample with editable source and compile output.
function TutorialCodeEditor({ include }: { include: TutorialCodeInclude }) {
  const [source, setSource] = useState(include.source)
  const [output, setOutput] = useState('')
  const [hasRun, setHasRun] = useState(false)

  // Compiles a tutorial snippet and stores the output in local component state.
  const runExample = async () => {
    setHasRun(true)
    setOutput('Compiling...')
    await new Promise((resolve) => window.setTimeout(resolve, 0))
    try {
      setOutput(await compileSource(source, include.functionName, include.shouldRun))
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : String(error)
      setOutput(`Unexpected frontend error while compiling:\n${errorMessage}`)
    }
  }

  return (
    <div className="tutorialExample" key={include.id}>
      <h3>{include.title}</h3>
      <textarea
        value={source}
        onChange={(event) => setSource(event.target.value)}
        className="tutorialCodeEditor"
        aria-label={`Editable source for ${include.title}`}
      />
      <button onClick={runExample} className="tutorialRunButton">
        Run example
      </button>
      {hasRun ? <textarea readOnly value={output} className="output" aria-label={`Output for ${include.title}`} /> : null}
    </div>
  )
}

// Renders one parsed markdown block for the currently active tutorial page.
function TutorialBlockView({ block, onNavigate }: { block: TutorialBlock; onNavigate: (pageId: string) => void }) {
  if (block.kind === 'heading') {
    if (block.level === 1) {
      return <h2>{block.text}</h2>
    }
    if (block.level === 2) {
      return <h3>{block.text}</h3>
    }
    return <h4>{block.text}</h4>
  }

  if (block.kind === 'codeInclude') {
    return <TutorialCodeEditor include={block} />
  }

  return (
    <p className="tutorialParagraph">
      {block.parts.map((part, index) => {
        const link = part.link
        if (link) {
          return (
            <button key={`${link.targetId}-${index}`} className="tutorialLink" onClick={() => onNavigate(link.targetId)}>
              {part.text}
            </button>
          )
        }
        return <span key={`${part.text}-${index}`}>{part.text}</span>
      })}
    </p>
  )
}

// Renders the markdown-driven tutorial with page navigation and editable code includes.
function TutorialView() {
  const [currentPageId, setCurrentPageId] = useState(tutorialPages[0]?.id ?? '')
  const [isSidebarOpen, setIsSidebarOpen] = useState(false)
  const currentPage: TutorialPage | undefined = tutorialPagesById[currentPageId]
  const currentIndex = tutorialPages.findIndex((page) => page.id === currentPageId)
  const previousPage = currentIndex > 0 ? tutorialPages[currentIndex - 1] : undefined
  const nextPage = currentIndex >= 0 && currentIndex < tutorialPages.length - 1 ? tutorialPages[currentIndex + 1] : undefined

  const navigateToPage = (pageId: string) => {
    if (tutorialPagesById[pageId]) {
      setCurrentPageId(pageId)
      setIsSidebarOpen(false)
    }
  }

  return (
    <section className="tutorialLayout">
      <button
        className="tutorialMenuButton"
        onClick={() => setIsSidebarOpen((current) => !current)}
        aria-expanded={isSidebarOpen}
        aria-controls="tutorial-sidebar"
      >
        ☰ Tutorial pages
      </button>
      <aside id="tutorial-sidebar" className={isSidebarOpen ? 'tutorialSidebar open' : 'tutorialSidebar'}>
        <h2>Tutorial</h2>
        <div className="tutorialPageList" role="tablist" aria-label="Tutorial pages">
          {tutorialPages.map((page) => (
            <button
              key={page.id}
              role="tab"
              className={page.id === currentPageId ? 'tab activeTab' : 'tab'}
              onClick={() => navigateToPage(page.id)}
            >
              {page.title}
            </button>
          ))}
        </div>
      </aside>
      <div className="tutorialContent">
        <h2>{currentPage?.title ?? 'Tutorial'}</h2>
        {currentPage ? (
          <article className="tutorialSection" key={currentPage.id}>
            {currentPage.blocks.map((block, index) => (
              <TutorialBlockView key={`${currentPage.id}-${index}`} block={block} onNavigate={navigateToPage} />
            ))}
          </article>
        ) : (
          <p className="subtitle">No tutorial pages were found.</p>
        )}

        <nav className="tutorialPager" aria-label="Tutorial page navigation">
          <button onClick={() => previousPage && navigateToPage(previousPage.id)} disabled={!previousPage}>
            ← Previous
          </button>
          <button onClick={() => nextPage && navigateToPage(nextPage.id)} disabled={!nextPage}>
            Next →
          </button>
        </nav>
      </div>
    </section>
  )
}

// Reads a saved theme value and falls back to browser preference or light mode.
function getInitialTheme(): Theme {
  const savedTheme = window.localStorage.getItem('minifumo-theme')
  if (savedTheme === 'light' || savedTheme === 'dark') {
    return savedTheme
  }

  return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light'
}

// Renders the mobile-friendly Minifumo browser playground UI.
export function App() {
  const editorContainerRef = useRef<HTMLDivElement | null>(null)
  const editorViewRef = useRef<EditorView | null>(null)
  const [output, setOutput] = useState('')
  const [functionName, setFunctionName] = useState('main')
  const [shouldRun, setShouldRun] = useState(true)
  const [activeTab, setActiveTab] = useState<Tab>('playground')
  const [theme, setTheme] = useState<Theme>(() => getInitialTheme())

  // Creates the CodeMirror extension list once for editor initialization.
  const editorExtensions = useMemo(
    () => [basicSetup, minifumoLanguage, oneDark, EditorView.lineWrapping],
    [],
  )

  // Initializes and tears down the CodeMirror editor instance.
  useEffect(() => {
    document.documentElement.setAttribute('data-theme', theme)
    window.localStorage.setItem('minifumo-theme', theme)
  }, [theme])

  // Initializes and tears down the CodeMirror editor instance.
  useEffect(() => {
    if (!editorContainerRef.current) {
      return
    }

    const editorView = new EditorView({
      doc: starterProgram,
      extensions: theme === 'dark' ? editorExtensions : [basicSetup, minifumoLanguage, EditorView.lineWrapping],
      parent: editorContainerRef.current,
    })

    editorViewRef.current = editorView

    return () => {
      editorView.destroy()
      editorViewRef.current = null
    }
  }, [editorExtensions, theme])

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
      <button onClick={() => setTheme((current) => (current === 'dark' ? 'light' : 'dark'))} className="themeToggleButton">
        {theme === 'dark' ? 'Switch to light mode' : 'Switch to dark mode'}
      </button>

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
        <TutorialView />
      )}
    </main>
  )
}
