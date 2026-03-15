/** Converts an LSP document URI into a compiler file path key. */
export function uriToCompilerPath(uri: string): string {
  if (!uri.startsWith('file://')) {
    return uri
  }
  try {
    return decodeURIComponent(new URL(uri).pathname)
  } catch {
    return uri
  }
}
