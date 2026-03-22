/** Resolves the URI to open for a definition target file. */
export function definitionUri(currentUri: string, currentFile: string, targetFile: string): string {
  if (targetFile === currentFile || targetFile === '') {
    return currentUri
  }
  if (!currentUri.startsWith('file://')) {
    return currentUri
  }
  try {
    if (targetFile.startsWith('/')) {
      return new URL(`file://${targetFile}`).toString()
    }
    return new URL(targetFile, currentUri).toString()
  } catch {
    return currentUri
  }
}
