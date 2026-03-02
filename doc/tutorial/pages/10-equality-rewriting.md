# 10. Equality and rewriting

Minifumo represents propositional equality with `Eq`.

Once you have an equality proof, you can transport and rewrite expressions using helper functions such as:

- `subst` (substitute equals in a dependent motive),
- `congrArg` (rewrite function arguments),
- `congr` (rewrite both functions and arguments).

These are foundational tools for proof-oriented programming and for making type-level reasoning explicit.

[Equality and rewriting examples](../examples/equality-rewriting.minifumo)

Previous: [9. Dependent types](./09-dependent-types.md)

Next: [11. Proofs by induction](./11-proofs-by-induction.md)
