import { mkdir, readdir, readFile, writeFile } from 'node:fs/promises'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const scriptDirectory = path.dirname(fileURLToPath(import.meta.url))
const webRoot = path.resolve(scriptDirectory, '..')
const distDirectory = path.join(webRoot, 'dist')
const tutorialPagesDirectory = path.resolve(webRoot, '../doc/tutorial/pages')

// Discovers tutorial page ids from markdown filenames.
async function loadTutorialPageIds() {
  const tutorialFiles = await readdir(tutorialPagesDirectory)
  return tutorialFiles.filter((fileName) => fileName.endsWith('.md')).map((fileName) => fileName.replace(/\.md$/u, ''))
}

// Returns all route paths that should exist as concrete static HTML exports.
async function collectExportRoutes() {
  const tutorialPageIds = await loadTutorialPageIds()
  const tutorialPageRoutes = tutorialPageIds.map((pageId) => `/tutorial/${encodeURIComponent(pageId)}/`)
  return ['/playground/', '/tutorial/', ...tutorialPageRoutes]
}

// Writes index.html copies so static hosts can serve deep links without rewrites.
async function writeStaticExportFiles() {
  const indexHtmlPath = path.join(distDirectory, 'index.html')
  const indexHtmlContent = await readFile(indexHtmlPath, 'utf8')
  const exportRoutes = await collectExportRoutes()

  for (const routePath of exportRoutes) {
    const routeDirectory = path.join(distDirectory, routePath)
    await mkdir(routeDirectory, { recursive: true })
    await writeFile(path.join(routeDirectory, 'index.html'), indexHtmlContent)
  }
}

await writeStaticExportFiles()
