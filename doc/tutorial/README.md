# Tutorial content

This directory contains the source material for the GitHub Pages tutorial.

- Author tutorial prose and structure in `web/src/tutorial.ts`.
- Keep executable code snippets in `doc/tutorial/examples/*.minifumo`.
- Every snippet in `doc/tutorial/examples` is type-checked by CI through `TutorialExamplesSuite`.
- The web tutorial imports these files directly so examples stay in sync.
