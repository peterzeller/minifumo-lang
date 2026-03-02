# 3. Functions

Functions are first-class in Minifumo: you can define top-level functions, pass functions as arguments, and return functions from other functions.

This section covers:

- named top-level functions,
- function types (for example `Nat -> Nat`),
- higher-order functions (functions that take functions),
- currying,
- anonymous functions (`(x) => ...`).

The included example defines `repeatNat`, which applies a `Nat -> Nat` function `n` times to a base case. It then uses this building block to define arithmetic-style operations (`plus`, `mult`, `pow`) in a compositional way.

[Functions, currying, and repeatNat](../examples/functions-repeat.minifumo)

Previous: [2. Standard library data types](./02-standard-library-data-types.md)

Next: [4. Simple expressions](./04-simple-expressions.md)
