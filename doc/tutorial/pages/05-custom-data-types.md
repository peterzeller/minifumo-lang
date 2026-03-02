# 5. Custom data types

Beyond built-ins, you can define your own algebraic data types with `data`.

In practice, this is how you model domain data in Minifumo: choose constructors for your cases, then consume values using pattern matching. Recursive data and recursive functions naturally go together.

The example defines a small binary tree of integers and computes the sum of all values.

[Custom data types](../examples/custom-data-types.minifumo)
