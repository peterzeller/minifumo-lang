import { expect, test, type Page } from '@playwright/test'

// Opens the sidebar and keeps it open for selecting a navigation entry.
async function openSidebar(page: Page): Promise<void> {
  await page.getByRole('button', { name: '☰' }).click()
  await expect(page.locator('#site-navigation')).toHaveClass(/open/)
}

// Navigates to a top-level destination from the sidebar table of contents.
async function navigateToTopLevel(page: Page, title: string): Promise<void> {
  await openSidebar(page)
  await page.locator('#site-navigation').getByRole('link', { name: title, exact: true }).click()
}

// Navigates to a tutorial page from the sidebar table of contents.
async function navigateToTutorialPage(page: Page, title: string): Promise<void> {
  await openSidebar(page)
  await page.locator('#site-navigation').getByRole('link', { name: title, exact: true }).click()
}

// Replaces the editor program with the given source code.
async function replaceProgram(page: Page, source: string): Promise<void> {
  await navigateToTopLevel(page, 'Playground')
  await expect(page.locator('.cm-content')).toBeVisible()
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

test('tutorial example runner shows output only after execution', async ({ page }) => {
  await page.goto('/')

  await navigateToTutorialPage(page, 'First program')

  await expect(page.locator('textarea.output')).toHaveCount(0)
  await page.getByRole('button', { name: 'Run example' }).first().click()

  await expect(page.locator('textarea.output').first()).toContainText('42')
})

test('tutorial page links navigate and included code is editable', async ({ page }) => {
  await page.goto('/')

  await navigateToTutorialPage(page, 'Working with lists')

  const editableSource = page.getByRole('textbox', { name: 'Editable source for Computing list length' })
  await editableSource.fill(`fun main(): Int
  7`)

  await page.getByRole('button', { name: 'Run example' }).first().click()

  const tutorialOutput = page.getByRole('textbox', { name: 'Output for Computing list length' })
  await expect(tutorialOutput).toContainText('7')
})
