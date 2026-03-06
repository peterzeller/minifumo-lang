export interface TutorialNavigationLeaf {
  kind: 'page'
  pageId: string
  title: string
}

export interface TutorialNavigationGroup {
  kind: 'group'
  title: string
  children: TutorialNavigationNode[]
}

export type TutorialNavigationNode = TutorialNavigationLeaf | TutorialNavigationGroup

export interface SiteNavigationModel {
  tutorialTree: TutorialNavigationGroup[]
}

// Declares the complete sidebar tutorial table of contents and hierarchy.
export const siteNavigationModel: SiteNavigationModel = {
  tutorialTree: [
    {
      kind: 'group',
      title: 'Getting started',
      children: [
        {
          kind: 'page',
          pageId: 'first-program',
          title: 'First program',
        },
      ],
    },
    {
      kind: 'group',
      title: 'Language tour',
      children: [
        {
          kind: 'page',
          pageId: 'language-tour',
          title: 'Overview',
        },
        {
          kind: 'page',
          pageId: '01-hello-world',
          title: '1. Hello world',
        },
        {
          kind: 'page',
          pageId: '02-standard-library-data-types',
          title: '2. Standard library data types',
        },
        {
          kind: 'page',
          pageId: '03-functions',
          title: '3. Functions',
        },
        {
          kind: 'page',
          pageId: '04-simple-expressions',
          title: '4. Simple expressions',
        },
        {
          kind: 'page',
          pageId: '05-custom-data-types',
          title: '5. Custom data types',
        },
        {
          kind: 'page',
          pageId: '06-module-system',
          title: '6. Module system',
        },
        {
          kind: 'page',
          pageId: '08-type-parameters',
          title: '8. Type parameters',
        },
        {
          kind: 'page',
          pageId: '09-dependent-types',
          title: '9. Dependent types',
        },
        {
          kind: 'page',
          pageId: '10-equality-rewriting',
          title: '10. Equality and rewriting',
        },
        {
          kind: 'page',
          pageId: '11-proofs-by-induction',
          title: '11. Proofs by induction',
        },
      ],
    },
    {
      kind: 'group',
      title: 'Collections',
      children: [
        {
          kind: 'page',
          pageId: 'working-with-lists',
          title: 'Working with lists',
        },
      ],
    },
  ],
}

// Flattens tutorial navigation tree nodes into linear leaf entries for previous/next navigation.
function flattenTutorialNodes(nodes: TutorialNavigationNode[]): TutorialNavigationLeaf[] {
  const leaves: TutorialNavigationLeaf[] = []
  for (const node of nodes) {
    if (node.kind === 'page') {
      leaves.push(node)
      continue
    }
    leaves.push(...flattenTutorialNodes(node.children))
  }
  return leaves
}

export const tutorialOrderedPages: TutorialNavigationLeaf[] = flattenTutorialNodes(siteNavigationModel.tutorialTree)

export const tutorialPageTitlesById: Record<string, string> = Object.fromEntries(
  tutorialOrderedPages.map((page) => [page.pageId, page.title]),
)
