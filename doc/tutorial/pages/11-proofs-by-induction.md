# 11. Proofs by induction

Induction proofs in Minifumo are written as recursive functions that return equality evidence.

The recursive call provides the induction hypothesis, and proof combinators like `congrArg` lift that hypothesis into the goal for the successor case.

The included snippet proves an associativity-shaped theorem for natural-number addition in this style. This is the same core technique used in larger proofs (including list properties like `sum(append(a, b))`).

[Proof by induction example](../examples/proofs-induction.minifumo)
