import {
  Connection,
  InitializeParams,
  InitializeResult,
  TextDocumentSyncKind,
  TextDocuments
} from 'vscode-languageserver'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { computeDiagnostics } from './diagnostics'
import { computeDefinition } from './definition'

/** Starts the Minifumo language server using a provided LSP connection. */
export function startMinifumoLanguageServer(connection: Connection): void {
  const documents = new TextDocuments(TextDocument)

  connection.onInitialize((params) => handleInitialize(params))
  documents.onDidOpen((event) => void validateDocument(connection, event.document))
  documents.onDidChangeContent((event) => void validateDocument(connection, event.document))
  connection.onDefinition((params) => {
    const document = documents.get(params.textDocument.uri)
    if (document === undefined) {
      return null
    }
    return computeDefinition(document.getText(), document.uri, params.position)
  })
  documents.onDidClose((event) =>
    connection.sendDiagnostics({
      uri: event.document.uri,
      diagnostics: []
    })
  )

  documents.listen(connection)
  connection.listen()
}

/** Declares server capabilities for text document synchronization. */
function handleInitialize(_params: InitializeParams): InitializeResult {
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      definitionProvider: true
    }
  }
}

/** Validates a document and publishes diagnostics back to the client. */
async function validateDocument(connection: Connection, document: TextDocument): Promise<void> {
  const diagnostics = computeDiagnostics(document.getText())
  connection.sendDiagnostics({
    uri: document.uri,
    diagnostics
  })
}
