import { createConnection, ProposedFeatures } from 'vscode-languageserver/node'
import { startMinifumoLanguageServer } from './server-core'

/** Creates a Node LSP connection and starts the Minifumo server. */
function main(): void {
  const connection = createConnection(ProposedFeatures.all)
  startMinifumoLanguageServer(connection)
}

main()
