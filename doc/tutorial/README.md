# Tutorial content

This directory contains the source material for the GitHub Pages tutorial.

- Author tutorial prose and structure in `doc/tutorial/pages/*.md`.
- Keep executable code snippets in `doc/tutorial/examples/*.minifumo`.
- Include executable snippets by adding a standalone markdown link line such as `[Example title](../examples/name.minifumo)`.
- Links between tutorial pages are regular markdown links to other `.md` files.
- Every snippet in `doc/tutorial/examples` is type-checked by CI through `TutorialExamplesSuite`.
- The web tutorial imports these files directly so examples stay in sync.
