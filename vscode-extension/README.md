# Minifumo VS Code Extension

This extension provides syntax highlighting and compiler diagnostics for `.minifumo` files.

## Features

- Syntax highlighting for Minifumo.
- LSP-based diagnostics (Problems panel + editor squiggles).
- Works in desktop VS Code and browser-based VS Code clients.

## How diagnostics work

The extension runs a language server in both environments:

- Desktop: Node language server.
- Browser: Web Worker language server.

Both server variants use the same Scala.js-compiled Minifumo compiler bridge and publish diagnostics through the Language Server Protocol.

## Development

```sh
npm install
npm run compile
```
