import assert from 'node:assert/strict'
import { definitionUri } from './definition-uri'

/** Verifies definitionUri keeps same-file targets in the current editor document. */
function testSameFileDefinitionUri(): void {
  const uri = 'file:///workspace/minifumo-lang/doc/examples/test.minifumo'
  const resolved = definitionUri(uri, '/workspace/minifumo-lang/doc/examples/test.minifumo', '/workspace/minifumo-lang/doc/examples/test.minifumo')
  assert.equal(resolved, uri)
}

/** Verifies definitionUri preserves standard library virtual-document targets. */
function testStandardLibraryDefinitionUri(): void {
  const uri = 'file:///workspace/minifumo-lang/doc/examples/test.minifumo'
  const resolved = definitionUri(uri, '/workspace/minifumo-lang/doc/examples/test.minifumo', 'minifumovirtual:/standard.minifumo')
  assert.equal(resolved, 'minifumovirtual:/standard.minifumo')
}

testSameFileDefinitionUri()
testStandardLibraryDefinitionUri()
