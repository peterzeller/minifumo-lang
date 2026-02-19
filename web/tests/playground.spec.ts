import { expect, test, type Page } from '@playwright/test'

// Replaces the editor program with the given source code.
async function replaceProgram(page: Page, source: string): Promise<void> {
  await page.locator('.cm-content').click()
  await page.keyboard.press('ControlOrMeta+a')
  await page.keyboard.type(source)
}

test('type errors are rendered in the output box', async ({ page }) => {
  await page.goto('/')

  await replaceProgram(
    page,
    `fun main(): Int
  unknownName`,
  )

  await page.getByRole('button', { name: 'Compile' }).click()

  const output = page.locator('textarea.output')
  await expect(output).toContainText('unknownName')
  await expect(output).toContainText('Line')
  await expect(output).toContainText('^')
})

test('successful execution shows println output and final return value', async ({ page }) => {
  await page.goto('/')

  await replaceProgram(
    page,
    `fun printlnString(value: String): Unit
  MakeUnit

fun main(): Int
  let _ = printlnString("hello from browser test") in 42`,
  )

  await page.getByRole('button', { name: 'Compile' }).click()

  const output = page.locator('textarea.output')
  await expect(output).toContainText('hello from browser test')
  await expect(output).toContainText('42')
})
