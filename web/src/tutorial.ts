import { Lexer, Tokens } from 'marked'

interface TutorialLink {
  label: string
  targetId: string
}

interface TutorialParagraphPart {
  text: string
  link?: TutorialLink
  isInlineCode?: boolean
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

const minifumoSources = import.meta.glob('../../doc/tutorial/examples/**/*.minifumo', {
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

// Converts one markdown link token into either a tutorial page link or plain text.
function linkTokenToParagraphPart(token: Tokens.Link, pagePath: string): TutorialParagraphPart {
  if (token.href.endsWith('.md')) {
    const targetPagePath = resolveProjectPath(pagePath, token.href)
    return {
      text: token.text,
      link: {
        label: token.text,
        targetId: pagePathToId(targetPagePath),
      },
    }
  }

  return { text: token.raw }
}

// Flattens inline markdown tokens into paragraph parts used by the tutorial renderer.
function parseParagraphParts(tokens: Tokens.Generic[] | undefined, pagePath: string): TutorialParagraphPart[] {
  if (!tokens || tokens.length === 0) {
    return []
  }

  const parts: TutorialParagraphPart[] = []

  for (const token of tokens) {
    if (token.type === 'codespan') {
      parts.push({ text: token.text, isInlineCode: true })
      continue
    }

    if (token.type === 'link') {
      parts.push(linkTokenToParagraphPart(token as Tokens.Link, pagePath))
      continue
    }

    if ('tokens' in token && Array.isArray(token.tokens)) {
      parts.push(...parseParagraphParts(token.tokens as Tokens.Generic[], pagePath))
      continue
    }

    if ('raw' in token && typeof token.raw === 'string') {
      parts.push({ text: token.raw })
      continue
    }

    if ('text' in token && typeof token.text === 'string') {
      parts.push({ text: token.text })
    }
  }

  return parts
}

// Detects a standalone minifumo include link paragraph and returns parsed capture groups.
function parseIncludeParagraph(token: Tokens.Paragraph): { title: string; includeTarget: string } | undefined {
  const includeMatch = token.raw.trim().match(/^\[([^\]]+)\]\(([^)]+\.minifumo)\)$/u)
  if (!includeMatch) {
    return undefined
  }

  return {
    title: includeMatch[1],
    includeTarget: includeMatch[2],
  }
}

// Parses markdown block tokens into tutorial blocks.
function parseTutorialBlocks(
  pagePath: string,
  markdown: string,
  minifumoSourceByPath: Record<string, string>,
): TutorialBlock[] {
  const tokens = Lexer.lex(markdown)
  const blocks: TutorialBlock[] = []
  const pageId = pagePathToId(pagePath)

  for (const token of tokens) {
    if (token.type === 'heading' && token.depth <= 3) {
      blocks.push({
        kind: 'heading',
        level: token.depth as 1 | 2 | 3,
        text: token.text,
      })
      continue
    }

    if (token.type === 'paragraph') {
      const include = parseIncludeParagraph(token as Tokens.Paragraph)
      if (include) {
        const resolvedPath = resolveProjectPath(pagePath, include.includeTarget)
        const source = minifumoSourceByPath[resolvedPath]

        if (source) {
          blocks.push({
            kind: 'codeInclude',
            id: `${pageId}-${blocks.length + 1}`,
            title: include.title,
            source,
            functionName: 'main',
            shouldRun: true,
          })
        } else {
          blocks.push({
            kind: 'paragraph',
            parts: [{ text: `Missing tutorial source file: ${include.includeTarget}` }],
          })
        }
        continue
      }

      const paragraphToken = token as Tokens.Paragraph
      const parts = parseParagraphParts(paragraphToken.tokens as Tokens.Generic[] | undefined, pagePath)
      blocks.push({
        kind: 'paragraph',
        parts: parts.length > 0 ? parts : [{ text: paragraphToken.text }],
      })
    }
  }

  return blocks
}

// Builds one parsed tutorial page from markdown text and known source files.
function parseTutorialPage(
  pagePath: string,
  markdown: string,
  minifumoSourceByPath: Record<string, string>,
): TutorialPage {
  const pageId = pagePathToId(pagePath)
  const blocks = parseTutorialBlocks(pagePath, markdown, minifumoSourceByPath)
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
