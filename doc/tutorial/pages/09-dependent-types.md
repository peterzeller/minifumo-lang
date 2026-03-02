# 9. Dependent types

Dependent types let types mention values, which allows stronger compile-time guarantees.

The classic example is a vector indexed by length: `Vector[T, N]`. With this representation, operations can encode shape constraints directly in their types.

The example includes:

- a length-indexed vector definition,
- dependent pattern matching to refine indices,
- a tail function whose type guarantees non-empty input.

[Dependent types with vectors](../examples/dependent-types-vector.minifumo)

Previous: [8. Type parameters](./08-type-parameters.md)

Next: [10. Equality and rewriting](./10-equality-rewriting.md)
