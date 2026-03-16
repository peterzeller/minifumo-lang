import * as vscode from 'vscode'
import { LanguageClient, LanguageClientOptions } from 'vscode-languageclient/browser'
import { MinifumoCompiler } from '../../web/src/generated/minifumo-compiler'

let client: LanguageClient | undefined

/** Activates the web extension host and starts the Minifumo browser LSP client. */
export function activate(context: vscode.ExtensionContext): void {
  const workerPath = new URL('dist/server/browser.js', `${context.extensionUri.toString(true)}/`)
  const workerUri = vscode.Uri.parse(workerPath.toString())
  const worker = new Worker(workerUri.toString(true), { type: 'module' })
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: 'minifumo' }]
  }

  const provider: vscode.TextDocumentContentProvider = {
    provideTextDocumentContent(uri: vscode.Uri): string {
      return MinifumoCompiler.standardLibrarySource()
    },
  };

  context.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider("minifumovirtual", provider)
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("minifumo.openVirtualFile", async () => {
      const uri = vscode.Uri.parse("minifumovirtual:/example.txt");
      const doc = await vscode.workspace.openTextDocument(uri);
      await vscode.window.showTextDocument(doc);
    })
  );

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
