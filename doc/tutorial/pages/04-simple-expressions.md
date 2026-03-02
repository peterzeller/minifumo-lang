# 4. Simple expressions

Minifumo expressions combine functional constructs with indentation-based blocks.

This page shows common everyday syntax:

- `let` bindings,
- statement-style expression lists,
- `match` / `case` branching,
- `if ... then/else` style branching,
- function calls.

A subtle parser detail is that the lexer injects newline-sensitive structure, so indentation and line breaks matter for how blocks are grouped. The example is intentionally formatted to make that structure easy to read.

[Simple expressions and statement lists](../examples/simple-expressions.minifumo)
