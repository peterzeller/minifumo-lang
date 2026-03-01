interface TutorialLink {
  label: string
  targetId: string
}

interface TutorialParagraphPart {
  text: string
  link?: TutorialLink
}

export interface TutorialParagraph {
  kind: 'paragraph'
  parts: TutorialParagraphPart[]
}

export interface TutorialHeading {
  kind: 'heading'
  level: 1 | 2 | 3
  text: string
}

export interface TutorialCodeInclude {
  kind: 'codeInclude'
  id: string
  title: string
  source: string
  functionName: string
  shouldRun: boolean
}

export type TutorialBlock = TutorialParagraph | TutorialHeading | TutorialCodeInclude

export interface TutorialPage {
  id: string
  title: string
  blocks: TutorialBlock[]
}

const markdownPages = import.meta.glob('../../doc/tutorial/pages/*.md', {
  eager: true,
  query: '?raw',
  import: 'default',
}) as Record<string, string>

const minifumoSources = import.meta.glob('../../doc/tutorial/examples/*.minifumo', {
  eager: true,
  query: '?raw',
  import: 'default',
}) as Record<string, string>

// Normalizes a project-relative path and resolves dot segments.
function normalizeProjectPath(path: string): string {
  const cleaned = path.replace(/\\/g, '/')
  const segments = cleaned.split('/')
  const normalized: string[] = []

  for (const segment of segments) {
    if (segment.length === 0 || segment === '.') {
      continue
    }
    if (segment === '..') {
      normalized.pop()
      continue
    }
    normalized.push(segment)
  }

  return normalized.join('/')
}

// Returns the directory part of a project-relative path.
function getDirectoryPath(path: string): string {
  const slashIndex = path.lastIndexOf('/')
  return slashIndex === -1 ? '' : path.slice(0, slashIndex)
}

// Resolves a project-relative target path against a base markdown file path.
function resolveProjectPath(basePath: string, targetPath: string): string {
  if (targetPath.startsWith('/')) {
    return normalizeProjectPath(targetPath)
  }

  const baseDirectory = getDirectoryPath(basePath)
  return normalizeProjectPath(`${baseDirectory}/${targetPath}`)
}

// Converts a markdown file path into a stable tutorial page id.
function pagePathToId(pagePath: string): string {
  const fileName = pagePath.slice(pagePath.lastIndexOf('/') + 1)
  return fileName.replace(/\.md$/u, '')
}

// Extracts inline markdown links from one paragraph while keeping plain text chunks.
function parseParagraphParts(text: string, pagePath: string): TutorialParagraphPart[] {
  const linkPattern = /\[([^\]]+)\]\(([^)]+)\)/gu
  const parts: TutorialParagraphPart[] = []
  let currentIndex = 0

  for (const match of text.matchAll(linkPattern)) {
    const fullMatch = match[0]
    const label = match[1]
    const rawTarget = match[2]
    const matchIndex = match.index ?? 0

    if (matchIndex > currentIndex) {
      parts.push({ text: text.slice(currentIndex, matchIndex) })
    }

    if (rawTarget.endsWith('.md')) {
      const targetPagePath = resolveProjectPath(pagePath, rawTarget)
      parts.push({
        text: label,
        link: {
          label,
          targetId: pagePathToId(targetPagePath),
        },
      })
    } else {
      parts.push({ text: fullMatch })
    }

    currentIndex = matchIndex + fullMatch.length
  }

  if (currentIndex < text.length) {
    parts.push({ text: text.slice(currentIndex) })
  }

  return parts.length > 0 ? parts : [{ text }]
}

// Builds one parsed tutorial page from markdown text and known source files.
function parseTutorialPage(
  pagePath: string,
  markdown: string,
  minifumoSourceByPath: Record<string, string>,
): TutorialPage {
  const pageId = pagePathToId(pagePath)
  const lines = markdown.split('\n')
  const blocks: TutorialBlock[] = []
  let paragraphLines: string[] = []

  // Flushes pending paragraph text into a structured paragraph block.
  const flushParagraph = () => {
    if (paragraphLines.length === 0) {
      return
    }

    blocks.push({
      kind: 'paragraph',
      parts: parseParagraphParts(paragraphLines.join(' '), pagePath),
    })
    paragraphLines = []
  }

  for (const line of lines) {
    const trimmed = line.trim()

    if (trimmed.length === 0) {
      flushParagraph()
      continue
    }

    const includeMatch = trimmed.match(/^\[([^\]]+)\]\(([^)]+\.minifumo)\)$/u)
    if (includeMatch) {
      flushParagraph()
      const title = includeMatch[1]
      const includeTarget = includeMatch[2]
      const resolvedPath = resolveProjectPath(pagePath, includeTarget)
      const source = minifumoSourceByPath[resolvedPath]

      if (source) {
        blocks.push({
          kind: 'codeInclude',
          id: `${pageId}-${blocks.length + 1}`,
          title,
          source,
          functionName: 'main',
          shouldRun: true,
        })
      } else {
        blocks.push({
          kind: 'paragraph',
          parts: [{ text: `Missing tutorial source file: ${includeTarget}` }],
        })
      }
      continue
    }

    const headingMatch = trimmed.match(/^(#{1,3})\s+(.*)$/u)
    if (headingMatch) {
      flushParagraph()
      blocks.push({
        kind: 'heading',
        level: headingMatch[1].length as 1 | 2 | 3,
        text: headingMatch[2],
      })
      continue
    }

    paragraphLines.push(trimmed)
  }

  flushParagraph()

  const titleHeading = blocks.find((block): block is TutorialHeading => block.kind === 'heading' && block.level === 1)

  return {
    id: pageId,
    title: titleHeading?.text ?? pageId,
    blocks,
  }
}

const markdownPagesByPath = Object.fromEntries(
  Object.entries(markdownPages).map(([rawPath, content]) => [normalizeProjectPath(rawPath.replace('../../', '')), content]),
)

const minifumoSourcesByPath = Object.fromEntries(
  Object.entries(minifumoSources).map(([rawPath, content]) => [normalizeProjectPath(rawPath.replace('../../', '')), content]),
)

export const tutorialPages: TutorialPage[] = Object.entries(markdownPagesByPath)
  .sort(([left], [right]) => left.localeCompare(right))
  .map(([path, markdown]) => parseTutorialPage(path, markdown, minifumoSourcesByPath))

export const tutorialPagesById = Object.fromEntries(tutorialPages.map((page) => [page.id, page]))
