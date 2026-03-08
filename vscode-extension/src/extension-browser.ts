import * as vscode from 'vscode'
import { LanguageClient, LanguageClientOptions } from 'vscode-languageclient/browser'

let client: LanguageClient | undefined

/** Activates the web extension host and starts the Minifumo browser LSP client. */
export function activate(context: vscode.ExtensionContext): void {
  const workerPath = new URL('dist/server/browser.js', `${context.extensionUri.toString(true)}/`)
  const workerUri = vscode.Uri.parse(workerPath.toString())
  const worker = new Worker(workerUri.toString(true), { type: 'module' })
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: 'minifumo' }]
  }
  client = new LanguageClient('minifumoLanguageServer', 'Minifumo Language Server', clientOptions, worker)
  context.subscriptions.push(client)
  void client.start()
}

/** Deactivates the web extension host and stops the LSP client. */
export async function deactivate(): Promise<void> {
  if (client === undefined) {
    return
  }
  await client.stop()
  client = undefined
}
