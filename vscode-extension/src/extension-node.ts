import * as path from 'node:path'
import * as vscode from 'vscode'
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node'
import { MinifumoCompiler } from '../../web/src/generated/minifumo-compiler'

let client: LanguageClient | undefined

/** Activates the desktop extension host and starts the Minifumo LSP client. */
export function activate(context: vscode.ExtensionContext): void {
  const serverModule = context.asAbsolutePath(path.join('dist', 'server', 'node.js'))
  const serverOptions: ServerOptions = {
    run: { module: serverModule, transport: TransportKind.ipc },
    debug: { module: serverModule, transport: TransportKind.ipc }
  }
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: 'minifumo' }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.minifumo')
    }
  }

const provider: vscode.TextDocumentContentProvider = {
    provideTextDocumentContent(uri: vscode.Uri): string {
      return MinifumoCompiler.standardLibrarySource()
    },
  };

  context.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider("minifumovirtual", provider)
  );

  client = new LanguageClient('minifumoLanguageServer', 'Minifumo Language Server', serverOptions, clientOptions)
  context.subscriptions.push(client)
  void client.start()
}

/** Deactivates the desktop extension host and stops the LSP client. */
export async function deactivate(): Promise<void> {
  if (client === undefined) {
    return
  }
  await client.stop()
  client = undefined
}
