import { mkdir, readFile, writeFile } from 'node:fs/promises'
import { dirname, join } from 'node:path'

const compilerApiPath = join(
  '..',
  'compiler-js',
  'src',
  'main',
  'scala',
  'com',
  'github',
  'peterzeller',
  'minifumo',
  'web',
  'CompilerApi.scala',
)
const outputPath = join('src', 'generated', 'minifumo-compiler.d.ts')

/**
 * Splits a comma-separated Scala parameter list while keeping nested generic types intact.
 */
function splitTopLevelCommaSeparated(text) {
  const result = []
  let current = ''
  let depth = 0

  for (const char of text) {
    if (char === ',' && depth === 0) {
      result.push(current.trim())
      current = ''
      continue
    }
    if (char === '[' || char === '(') {
      depth += 1
    } else if (char === ']' || char === ')') {
      depth -= 1
    }
    current += char
  }

  if (current.trim().length > 0) {
    result.push(current.trim())
  }
  return result
}

/**
 * Normalizes a Scala doc comment into a TypeScript doc comment body.
 */
function normalizeDocComment(comment) {
  return comment
    .split('\n')
    .map((line) => line.replace(/^\s*\/?\**\s?/, '').replace(/\*\/$/, '').trimEnd())
    .filter((line, index, lines) => !(line === '' && (index === 0 || index === lines.length - 1)))
}

/**
 * Converts the subset of Scala.js-exposed Scala types used by CompilerApi into TypeScript.
 */
function scalaTypeToTypescript(scalaType) {
  const normalizedType = scalaType.replace(/\s+/g, ' ').trim()

  if (normalizedType === 'String') {
    return 'string'
  }
  if (normalizedType === 'Int' || normalizedType === 'Double') {
    return 'number'
  }
  if (normalizedType === 'Boolean') {
    return 'boolean'
  }
  if (normalizedType.startsWith('js.Array[') && normalizedType.endsWith(']')) {
    const innerType = normalizedType.slice('js.Array['.length, -1)
    return `${scalaTypeToTypescript(innerType)}[]`
  }
  if (normalizedType.startsWith('js.UndefOr[') && normalizedType.endsWith(']')) {
    const innerType = normalizedType.slice('js.UndefOr['.length, -1)
    return `${scalaTypeToTypescript(innerType)} | undefined`
  }

  return normalizedType
}

/**
 * Parses one Scala parameter declaration into a TypeScript-compatible shape.
 */
function parseParameter(parameterText) {
  const normalizedParameter = parameterText.replace(/^val\s+/, '')
  const match = normalizedParameter.match(/^(\w+):\s*(.+?)(?:\s*=\s*(.+))?$/)
  if (!match) {
    throw new Error(`Unsupported parameter declaration: ${parameterText}`)
  }

  const [, name, scalaType, defaultValue] = match
  return {
    name,
    optional: defaultValue !== undefined,
    type: scalaTypeToTypescript(scalaType),
  }
}

/**
 * Reads a Scala doc comment starting at the current line index.
 */
function readDocComment(lines, startIndex) {
  const commentLines = []
  let index = startIndex

  while (index < lines.length) {
    commentLines.push(lines[index])
    if (lines[index].includes('*/')) {
      return {
        comment: normalizeDocComment(commentLines.join('\n')),
        nextIndex: index + 1,
      }
    }
    index += 1
  }

  throw new Error(`Unterminated doc comment at line ${startIndex + 1}`)
}

/**
 * Reads a possibly multi-line class declaration until its js.Object extension line.
 */
function readClassDeclaration(lines, startIndex) {
  let declaration = lines[startIndex].trim()
  let index = startIndex + 1

  while (!declaration.includes('extends js.Object')) {
    declaration += ` ${lines[index].trim()}`
    index += 1
  }

  return {
    declaration,
    nextIndex: index,
  }
}

/**
 * Parses Scala.js classes and exported methods from CompilerApi with their adjacent comments.
 */
function parseCompilerApi(sourceText) {
  const lines = sourceText.split('\n')
  const classes = []
  const methods = []
  let pendingComment = []
  let exportPending = false

  for (let index = 0; index < lines.length;) {
    const trimmedLine = lines[index].trim()

    if (trimmedLine.startsWith('/**')) {
      const result = readDocComment(lines, index)
      pendingComment = result.comment
      index = result.nextIndex
      continue
    }

    if (trimmedLine === '@JSExport') {
      exportPending = true
      index += 1
      continue
    }

    if (trimmedLine.startsWith('class ')) {
      const result = readClassDeclaration(lines, index)
      const classMatch = result.declaration.match(/^class\s+(\w+)\(([\s\S]*?)\)\s+extends\s+js\.Object$/)
      if (!classMatch) {
        throw new Error(`Unsupported class declaration: ${result.declaration}`)
      }

      const [, name, rawParameters] = classMatch
      const properties = splitTopLevelCommaSeparated(rawParameters).map(parseParameter)
      classes.push({
        comment: pendingComment,
        name,
        properties,
      })
      pendingComment = []
      exportPending = false
      index = result.nextIndex
      continue
    }

    if (exportPending && trimmedLine.startsWith('def ')) {
      const methodMatch = trimmedLine.match(/^def\s+(\w+)\(([\s\S]*?)\):\s*([^\n=]+?)\s*=$/)
      if (!methodMatch) {
        throw new Error(`Unsupported exported method declaration: ${trimmedLine}`)
      }

      const [, name, rawParameters, rawReturnType] = methodMatch
      const parameters = rawParameters.trim().length === 0
        ? []
        : splitTopLevelCommaSeparated(rawParameters).map(parseParameter)
      methods.push({
        comment: pendingComment,
        name,
        parameters,
        returnType: scalaTypeToTypescript(rawReturnType),
      })
      pendingComment = []
      exportPending = false
      index += 1
      continue
    }

    if (trimmedLine !== '') {
      pendingComment = []
      exportPending = false
    }
    index += 1
  }

  return { classes, methods }
}

/**
 * Renders a TypeScript doc comment when one is present.
 */
function renderDocComment(lines) {
  if (lines.length === 0) {
    return []
  }

  return ['/**', ...lines.map((line) => ` * ${line}`), ' */']
}

/**
 * Renders the generated TypeScript declaration file content.
 */
function renderDeclarationFile(classes, methods) {
  const outputLines = [
    '// Generated from compiler-js/src/main/scala/com/github/peterzeller/minifumo/web/CompilerApi.scala.',
    '// Do not edit this file manually.',
    '',
  ]

  for (const exportedClass of classes) {
    outputLines.push(...renderDocComment(exportedClass.comment))
    outputLines.push(`export interface ${exportedClass.name} {`)
    for (const property of exportedClass.properties) {
      outputLines.push(`  ${property.name}: ${property.type}`)
    }
    outputLines.push('}')
    outputLines.push('')
  }

  outputLines.push('export const MinifumoCompiler: {')
  for (const method of methods) {
    outputLines.push(...renderDocComment(method.comment).map((line) => `  ${line}`))
    const renderedParameters = method.parameters
      .map((parameter) => `${parameter.name}${parameter.optional ? '?' : ''}: ${parameter.type}`)
      .join(', ')
    outputLines.push(`  ${method.name}(${renderedParameters}): ${method.returnType}`)
  }
  outputLines.push('}')
  outputLines.push('')

  return `${outputLines.join('\n')}`
}

/**
 * Generates the frontend declaration file from the Scala.js compiler API source.
 */
async function generateCompilerDeclarations() {
  const sourceText = await readFile(compilerApiPath, 'utf8')
  const { classes, methods } = parseCompilerApi(sourceText)
  const declarationText = renderDeclarationFile(classes, methods)

  await mkdir(dirname(outputPath), { recursive: true })
  await writeFile(outputPath, declarationText, 'utf8')
  console.log(`Generated ${outputPath} from ${compilerApiPath}`)
}

await generateCompilerDeclarations()
