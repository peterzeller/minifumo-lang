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
