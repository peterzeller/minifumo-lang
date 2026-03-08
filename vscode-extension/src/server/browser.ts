import { BrowserMessageReader, BrowserMessageWriter, createConnection } from 'vscode-languageserver/browser'
import { startMinifumoLanguageServer } from './server-core'

declare const self: DedicatedWorkerGlobalScope

/** Creates a browser worker LSP connection and starts the Minifumo server. */
function main(): void {
  const messageReader = new BrowserMessageReader(self)
  const messageWriter = new BrowserMessageWriter(self)
  const connection = createConnection(messageReader, messageWriter)
  startMinifumoLanguageServer(connection)
}

main()
