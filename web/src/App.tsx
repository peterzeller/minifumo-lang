import { basicSetup, EditorView } from 'codemirror'
import { useEffect, useMemo, useRef, useState } from 'react'
import { oneDark } from '@codemirror/theme-one-dark'
import { minifumoLanguage } from './minifumoLanguage'
import { MinifumoCompiler } from './compiler'
import { tutorialPagesById, type TutorialBlock, type TutorialCodeInclude, type TutorialPage } from './tutorial'
import { siteNavigationModel, tutorialOrderedPages, tutorialPageTitlesById, type TutorialNavigationNode } from './navigation'

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

// Selects the shared Minifumo editor extensions for a given theme mode.
function getEditorExtensions(theme: Theme, baseExtensions: readonly unknown[]) {
  if (theme === 'dark') {
    return baseExtensions
  }

  return [basicSetup, minifumoLanguage, EditorView.lineWrapping]
}

// Renders one runnable tutorial code sample with editable source and compile output.
function TutorialCodeEditor({ include, theme, editorExtensions }: { include: TutorialCodeInclude; theme: Theme; editorExtensions: readonly unknown[] }) {
  const editorContainerRef = useRef<HTMLDivElement | null>(null)
  const editorViewRef = useRef<EditorView | null>(null)
  const [output, setOutput] = useState('')
  const [hasRun, setHasRun] = useState(false)

  // Initializes and tears down the tutorial CodeMirror instance.
  useEffect(() => {
    if (!editorContainerRef.current) {
      return
    }

    const editorView = new EditorView({
      doc: include.source,
      extensions: [
        ...getEditorExtensions(theme, editorExtensions),
        EditorView.contentAttributes.of({ 'aria-label': `Editable source for ${include.title}` }),
      ],
      parent: editorContainerRef.current,
    })

    editorViewRef.current = editorView

    return () => {
      editorView.destroy()
      editorViewRef.current = null
    }
  }, [editorExtensions, include.source, include.title, theme])

  // Compiles a tutorial snippet and stores the output in local component state.
  const runExample = async () => {
    if (!editorViewRef.current) {
      return
    }

    const source = editorViewRef.current.state.doc.toString()
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
      <div ref={editorContainerRef} className="editor tutorialCodeEditor" />
      <button onClick={runExample} className="tutorialRunButton">
        Run example
      </button>
      {hasRun ? <textarea readOnly value={output} className="output" aria-label={`Output for ${include.title}`} /> : null}
    </div>
  )
}

// Renders one parsed markdown block for the currently active tutorial page.
function TutorialBlockView({
  block,
  onNavigate,
  theme,
  editorExtensions,
}: {
  block: TutorialBlock
  onNavigate: (pageId: string) => void
  theme: Theme
  editorExtensions: readonly unknown[]
}) {
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
    return <TutorialCodeEditor include={block} theme={theme} editorExtensions={editorExtensions} />
  }

  return (
    <p className="tutorialParagraph">
      {block.parts.map((part, index) => {
        const link = part.link
        if (link) {
          return (
            <a
              key={`${link.targetId}-${index}`}
              className="tutorialLink"
              href="#"
              onClick={(event) => {
                event.preventDefault()
                onNavigate(link.targetId)
              }}
            >
              {part.text}
            </a>
          )
        }
        return <span key={`${part.text}-${index}`}>{part.text}</span>
      })}
    </p>
  )
}

// Returns previous and next tutorial pages for a given current page id.
function getNeighborPages(currentPageId: string): {
  previousPage?: { pageId: string; title: string }
  nextPage?: { pageId: string; title: string }
} {
  const currentIndex = tutorialOrderedPages.findIndex((page) => page.pageId === currentPageId)
  const previousPage = currentIndex > 0 ? tutorialOrderedPages[currentIndex - 1] : undefined
  const nextPage = currentIndex >= 0 && currentIndex < tutorialOrderedPages.length - 1 ? tutorialOrderedPages[currentIndex + 1] : undefined
  return { previousPage, nextPage }
}

// Renders one recursive tutorial navigation branch inside the sidebar table of contents.
function TutorialNavigationTree({
  nodes,
  currentPageId,
  onOpenPage,
}: {
  nodes: TutorialNavigationNode[]
  currentPageId: string
  onOpenPage: (pageId: string) => void
}) {
  return (
    <ul className="tocList">
      {nodes.map((node) => {
        if (node.kind === 'page') {
          const isActive = currentPageId === node.pageId
          return (
            <li key={node.pageId}>
              <a
                href="#"
                className={isActive ? 'tocLink activeTocLink' : 'tocLink'}
                onClick={(event) => {
                  event.preventDefault()
                  onOpenPage(node.pageId)
                }}
              >
                {node.title}
              </a>
            </li>
          )
        }

        return (
          <li key={node.title}>
            <details className="tocGroup" open>
              <summary>{node.title}</summary>
              <TutorialNavigationTree nodes={node.children} currentPageId={currentPageId} onOpenPage={onOpenPage} />
            </details>
          </li>
        )
      })}
    </ul>
  )
}

// Renders the markdown-driven tutorial with previous and next page navigation.
function TutorialView({
  currentPageId,
  onNavigate,
  theme,
  editorExtensions,
}: {
  currentPageId: string
  onNavigate: (pageId: string) => void
  theme: Theme
  editorExtensions: readonly unknown[]
}) {
  const currentPage: TutorialPage | undefined = tutorialPagesById[currentPageId]
  const { previousPage, nextPage } = getNeighborPages(currentPageId)

  return (
    <section className="tutorialContent">
      <h2>{currentPage?.title ?? tutorialPageTitlesById[currentPageId] ?? 'Tutorial'}</h2>
      {currentPage ? (
        <article className="tutorialSection" key={currentPage.id}>
          {currentPage.blocks.map((block, index) => (
            <TutorialBlockView
              key={`${currentPage.id}-${index}`}
              block={block}
              onNavigate={onNavigate}
              theme={theme}
              editorExtensions={editorExtensions}
            />
          ))}
        </article>
      ) : (
        <p className="subtitle">No tutorial page was found for this navigation entry.</p>
      )}

      <nav className="tutorialPager" aria-label="Tutorial page navigation">
        <button onClick={() => previousPage && onNavigate(previousPage.pageId)} disabled={!previousPage}>
          {previousPage ? `← ${previousPage.title}` : '← Previous'}
        </button>
        <button onClick={() => nextPage && onNavigate(nextPage.pageId)} disabled={!nextPage}>
          {nextPage ? `${nextPage.title} →` : 'Next →'}
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

// Returns the initial navigation target based on the declared tutorial table of contents.
function getInitialNavigationTarget(): NavigationTarget {
  const firstTutorialPage = tutorialOrderedPages[0]
  return firstTutorialPage ? { kind: 'tutorial', pageId: firstTutorialPage.pageId } : { kind: 'playground' }
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
  const isPlaygroundActive = navigationTarget.kind === 'playground'

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

  // Initializes and tears down the CodeMirror editor instance when the playground view is active.
  useEffect(() => {
    if (!isPlaygroundActive || !editorContainerRef.current) {
      return
    }

    const editorView = new EditorView({
      doc: starterProgram,
      extensions: getEditorExtensions(theme, editorExtensions),
      parent: editorContainerRef.current,
    })

    editorViewRef.current = editorView

    return () => {
      editorView.destroy()
      editorViewRef.current = null
    }
  }, [editorExtensions, isPlaygroundActive, theme])

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

  // Switches the main view to a selected tutorial page when it exists in parsed content.
  const openTutorialPage = (pageId: string) => {
    if (!tutorialPagesById[pageId]) {
      return
    }

    setNavigationTarget({ kind: 'tutorial', pageId })
    setIsNavOpen(false)
  }

  const activeTutorialPageId = navigationTarget.kind === 'tutorial' ? navigationTarget.pageId : ''

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
        <nav aria-label="Main navigation">
          <h2>Contents</h2>
          <a
            href="#"
            className={isPlaygroundActive ? 'tocLink activeTocLink' : 'tocLink'}
            onClick={(event) => {
              event.preventDefault()
              openPlayground()
            }}
          >
            Playground
          </a>
          <details className="tocGroup" open>
            <summary>Tutorial</summary>
            <TutorialNavigationTree
              nodes={siteNavigationModel.tutorialTree}
              currentPageId={activeTutorialPageId}
              onOpenPage={openTutorialPage}
            />
          </details>
        </nav>
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
          <TutorialView
            currentPageId={navigationTarget.pageId}
            onNavigate={openTutorialPage}
            theme={theme}
            editorExtensions={editorExtensions}
          />
        )}
      </section>
    </main>
  )
}
