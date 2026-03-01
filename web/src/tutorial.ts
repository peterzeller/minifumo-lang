import introProgramSource from '../../doc/tutorial/examples/intro-main.minifumo?raw'
import listProgramSource from '../../doc/tutorial/examples/list-length.minifumo?raw'

export interface TutorialExample {
  id: string
  title: string
  source: string
  functionName: string
  shouldRun: boolean
}

export interface TutorialSection {
  id: string
  title: string
  content: string
  examples: TutorialExample[]
}

export const tutorialSections: TutorialSection[] = [
  {
    id: 'first-program',
    title: 'Your first Minifumo program',
    content:
      'This example defines a main function and evaluates a small expression. Use the Run button to compile and execute it in the browser.',
    examples: [
      {
        id: 'intro-main',
        title: 'Basic expression evaluation',
        source: introProgramSource,
        functionName: 'main',
        shouldRun: true,
      },
    ],
  },
  {
    id: 'data-types',
    title: 'Working with lists',
    content:
      'This example introduces algebraic data types and pattern matching. The program is the same one that is type-checked in CI.',
    examples: [
      {
        id: 'list-length',
        title: 'Computing list length',
        source: listProgramSource,
        functionName: 'main',
        shouldRun: true,
      },
    ],
  },
]
