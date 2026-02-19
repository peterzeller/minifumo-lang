import { access, cp, mkdir, readdir } from 'node:fs/promises'
import { constants } from 'node:fs'
import { join } from 'node:path'

// Finds the Scala.js fastLinkJS artifact and copies it into the web source tree when available.
async function syncCompilerArtifact() {
  const scalaVersion = 'scala-3.7.4'
  const targetRoot = join('..', 'compiler-js', 'target', scalaVersion)
  const destinationDir = join('src', 'generated')
  const destinationFile = join(destinationDir, 'minifumo-compiler.js')

  let entries
  try {
    entries = await readdir(targetRoot, { withFileTypes: true })
  } catch {
    console.warn(`No Scala.js target folder found at ${targetRoot}; keeping existing ${destinationFile}.`)
    return
  }

  const fastOptDir = entries.find((entry) => entry.isDirectory() && entry.name.endsWith('-fastopt'))
  if (!fastOptDir) {
    console.warn(`No fastopt folder found in ${targetRoot}; keeping existing ${destinationFile}.`)
    return
  }

  const sourceFile = join(targetRoot, fastOptDir.name, 'main.js')
  try {
    await access(sourceFile, constants.R_OK)
  } catch {
    console.warn(`No Scala.js main.js at ${sourceFile}; keeping existing ${destinationFile}.`)
    return
  }

  await mkdir(destinationDir, { recursive: true })
  await cp(sourceFile, destinationFile)
  console.log(`Copied ${sourceFile} -> ${destinationFile}`)
}

await syncCompilerArtifact()
