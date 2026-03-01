# Language tour

This extended tour introduces the core features of Minifumo through executable snippets.

## 1. Hello world

This example shows a simple `main` function and line comments (`// ...`).

[Hello world with comments and functions](../examples/hello-world.minifumo)

## 2. Standard library data types

Minifumo ships with useful data types such as `Nat`, `Int`, `Bool`, and `List`.

[Standard library data types](../examples/standard-library-data-types.minifumo)

## 3. Functions

This section demonstrates top-level function definitions, function types, passing functions as values, currying, anonymous functions, and a reusable `repeatNat` helper. It then uses `repeatNat` to define `plus`, `mult`, and `pow`.

[Functions, currying, and repeatNat](../examples/functions-repeat.minifumo)

## 4. Simple expressions

This snippet covers `let`, statement lists, `match`, `if/then/else`, function calls, and indentation-based syntax. The parser relies on lexer newline injection for statement boundaries.

[Simple expressions and statement lists](../examples/simple-expressions.minifumo)

## 5. Custom data types

Define your own algebraic data types and consume them with recursive functions.

[Custom data types](../examples/custom-data-types.minifumo)

## 6. Module system

Use `export` to publish definitions and `import` to consume them across files. Real projects can also use `minifumo.yml` to describe project-level module settings.

[Module file with exports](../examples/modules/math-utils.minifumo)

[Module file importing exports](../examples/modules/module-main.minifumo)

## 8. Type parameters

Generic functions and generic data types let you write reusable code. This example includes a generic list plus `map` and `foldl`.

[Type parameters, generic list, map, and foldl](../examples/type-parameters.minifumo)

## 9. Dependent types

A `Vector[T, N]` can carry its length in the type. This snippet shows dependent pattern matching and definitional equality in action.

[Dependent types with vectors](../examples/dependent-types-vector.minifumo)

## 10. Equality and rewriting

Minifumo provides `Eq` proofs and rewriting helpers like `subst`, `congrArg`, and `congr`.

[Equality and rewriting examples](../examples/equality-rewriting.minifumo)

## 11. Proofs by induction

This section proves the append/sum property by structural recursion, using recursive calls as induction hypotheses. The snippet uses a recursively normalized right-hand side (`sumAppendRhs`) that corresponds to `sum(listA) + sum(listB)`.

[Proof by induction over list append and sum](../examples/proofs-induction.minifumo)
