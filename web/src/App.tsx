import { basicSetup, EditorView } from 'codemirror'
import { useEffect, useMemo, useRef, useState } from 'react'
import { oneDark } from '@codemirror/theme-one-dark'
import { minifumoLanguage } from './minifumoLanguage'
import { MinifumoCompiler } from './compiler'
import { tutorialPages, tutorialPagesById, type TutorialBlock, type TutorialCodeInclude, type TutorialPage } from './tutorial'

type Theme = 'light' | 'dark'

type NavigationTarget =
  | { kind: 'playground' }
  | { kind: 'tutorial'; pageId: string }

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

// Renders the markdown-driven tutorial with previous and next page navigation.
function TutorialView({ currentPageId, onNavigate }: { currentPageId: string; onNavigate: (pageId: string) => void }) {
  const currentPage: TutorialPage | undefined = tutorialPagesById[currentPageId]
  const currentIndex = tutorialPages.findIndex((page) => page.id === currentPageId)
  const previousPage = currentIndex > 0 ? tutorialPages[currentIndex - 1] : undefined
  const nextPage = currentIndex >= 0 && currentIndex < tutorialPages.length - 1 ? tutorialPages[currentIndex + 1] : undefined

  return (
    <section className="tutorialContent">
      <h2>{currentPage?.title ?? 'Tutorial'}</h2>
      {currentPage ? (
        <article className="tutorialSection" key={currentPage.id}>
          {currentPage.blocks.map((block, index) => (
            <TutorialBlockView key={`${currentPage.id}-${index}`} block={block} onNavigate={onNavigate} />
          ))}
        </article>
      ) : (
        <p className="subtitle">No tutorial pages were found.</p>
      )}

      <nav className="tutorialPager" aria-label="Tutorial page navigation">
        <button onClick={() => previousPage && onNavigate(previousPage.id)} disabled={!previousPage}>
          ← Previous
        </button>
        <button onClick={() => nextPage && onNavigate(nextPage.id)} disabled={!nextPage}>
          Next →
        </button>
      </nav>
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

// Returns the initial navigation target based on available tutorial pages.
function getInitialNavigationTarget(): NavigationTarget {
  const firstTutorialPage = tutorialPages[0]
  return firstTutorialPage ? { kind: 'tutorial', pageId: firstTutorialPage.id } : { kind: 'playground' }
}

// Renders the mobile-friendly Minifumo browser playground UI.
export function App() {
  const editorContainerRef = useRef<HTMLDivElement | null>(null)
  const editorViewRef = useRef<EditorView | null>(null)
  const [output, setOutput] = useState('')
  const [functionName, setFunctionName] = useState('main')
  const [shouldRun, setShouldRun] = useState(true)
  const [theme, setTheme] = useState<Theme>(() => getInitialTheme())
  const [isNavOpen, setIsNavOpen] = useState(false)
  const [navigationTarget, setNavigationTarget] = useState<NavigationTarget>(() => getInitialNavigationTarget())

  // Creates the CodeMirror extension list once for editor initialization.
  const editorExtensions = useMemo(
    () => [basicSetup, minifumoLanguage, oneDark, EditorView.lineWrapping],
    [],
  )

  // Applies and persists the active light or dark theme.
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

  // Switches the main view to the playground section.
  const openPlayground = () => {
    setNavigationTarget({ kind: 'playground' })
    setIsNavOpen(false)
  }

  // Switches the main view to a selected tutorial page.
  const openTutorialPage = (pageId: string) => {
    if (!tutorialPagesById[pageId]) {
      return
    }

    setNavigationTarget({ kind: 'tutorial', pageId })
    setIsNavOpen(false)
  }

  const isPlaygroundActive = navigationTarget.kind === 'playground'

  return (
    <main className="page">
      <header className="topBar">
        <button
          className="navToggleButton"
          onClick={() => setIsNavOpen((current) => !current)}
          aria-expanded={isNavOpen}
          aria-controls="site-navigation"
        >
          ☰
        </button>
        <h1 className="topBarTitle">Minifumo Web Playground</h1>
        <button onClick={() => setTheme((current) => (current === 'dark' ? 'light' : 'dark'))} className="themeToggleButton">
          {theme === 'dark' ? 'Light' : 'Dark'}
        </button>
      </header>

      <aside id="site-navigation" className={isNavOpen ? 'tutorialSidebar open' : 'tutorialSidebar'}>
        <h2>Navigation</h2>
        <button className={isPlaygroundActive ? 'tab activeTab' : 'tab'} onClick={openPlayground}>
          Playground
        </button>
        <h3 className="navigationSectionTitle">Tutorial</h3>
        <div className="tutorialPageList" role="tablist" aria-label="Tutorial pages">
          {tutorialPages.map((page) => {
            const isActive = navigationTarget.kind === 'tutorial' && navigationTarget.pageId === page.id
            return (
              <button key={page.id} role="tab" className={isActive ? 'tab activeTab' : 'tab'} onClick={() => openTutorialPage(page.id)}>
                {page.title}
              </button>
            )
          })}
        </div>
      </aside>

      <section className="mainContent">
        {isPlaygroundActive ? (
          <>
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
          </>
        ) : (
          <TutorialView currentPageId={navigationTarget.pageId} onNavigate={openTutorialPage} />
        )}
      </section>
    </main>
  )
}
