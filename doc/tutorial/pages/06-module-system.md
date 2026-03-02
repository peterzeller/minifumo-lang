# 6. Module system

Minifumo modules let you split code across files and control visibility.

Key ideas:

- Use `export` to make definitions available to other files.
- Use `import ... from "..."` to consume exported names.
- Project files (for example `minifumo.yml`) can describe project-level settings and structure.

The two snippets below show a minimal exported helper and a consumer module that imports and uses it.

[Module file with exports](../examples/modules/math-utils.minifumo)

[Module file importing exports](../examples/modules/module-main.minifumo)

Previous: [5. Custom data types](./05-custom-data-types.md)

Next: [8. Type parameters](./08-type-parameters.md)
