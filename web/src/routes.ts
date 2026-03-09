import { tutorialPagesById } from './tutorial'

export type NavigationTarget =
  | { kind: 'playground' }
  | { kind: 'tutorial'; pageId: string }

const playgroundPath = '/playground'
const tutorialPathPrefix = '/tutorial/'

// Returns the application base path configured for static deployment environments.
function getBasePath(): string {
  return import.meta.env.BASE_URL.replace(/\/$/u, '') || ''
}

// Removes the deployment base path from a browser pathname.
function stripBasePath(pathname: string): string {
  const basePath = getBasePath()
  if (basePath && pathname.startsWith(basePath)) {
    const strippedPath = pathname.slice(basePath.length)
    return strippedPath.length > 0 ? strippedPath : '/'
  }

  return pathname
}

// Converts a browser pathname into a navigation target used by the React app state.
export function pathToNavigationTarget(pathname: string, fallbackTarget: NavigationTarget): NavigationTarget {
  const normalizedPath = stripBasePath(pathname).replace(/\/$/u, '') || '/'

  if (normalizedPath === '/' || normalizedPath === playgroundPath) {
    return { kind: 'playground' }
  }

  if (normalizedPath.startsWith(tutorialPathPrefix)) {
    const pageId = decodeURIComponent(normalizedPath.slice(tutorialPathPrefix.length))
    if (tutorialPagesById[pageId]) {
      return { kind: 'tutorial', pageId }
    }
  }

  return fallbackTarget
}

// Converts a navigation target into a deployment-aware browser path.
export function navigationTargetToPath(target: NavigationTarget): string {
  const relativePath = target.kind === 'playground' ? playgroundPath : `${tutorialPathPrefix}${encodeURIComponent(target.pageId)}`
  return `${getBasePath()}${relativePath}`
}

// Navigates to a target and updates browser history unless the URL already matches.
export function syncBrowserPath(target: NavigationTarget): void {
  const nextPath = navigationTargetToPath(target)
  if (window.location.pathname !== nextPath) {
    window.history.pushState(null, '', nextPath)
  }
}
