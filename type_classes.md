# Minifumo Type Class Specification

## 1. Syntax and Structure

### Type Class Declaration
A type class defines a contract (interface) for a set of types. It introduces a new type constructor (the class name) and a set of method signatures.

```minifumo
typeclass Show[T]
    fun show(value T) String
```

*   **Scope**: The class name `Show` and its methods (e.g., `show`) become visible in the file scope where declared.

### Instance Declaration
An instance provides a concrete implementation of a type class for a specific type.

```minifumo
instance showInt for Show[Int]
    fun show(value Int) String
        "41"
```

*   **Naming**: Instances are named (e.g., `showInt`). This name is used for explicit dictionary passing via `using`.
*   **Scope**: The instance name is a value visible in the file scope. It is also registered in the global/module implicit scope for resolution.
*   **Constraints**: Instances can have their own `given` clauses (conditional instances), e.g., `instance listShow[T] for Show[List[T]] given (s Show[T])`.

### Functions with Context (`given`)
Functions can declare dependencies on type classes using the `given` clause.

```minifumo
fun printValue[T](value T) given (s Show[T])
    println(show(value))
```

*   **Semantics**: The `given` clause adds implicit parameters to the function. `s` becomes a value of type `Show[T]` available in the function body.
*   **Method Resolution**: Inside the body, calling `show(value)` is possible, since the given type class can be used.

### Call Site Resolution
When calling a function with a `given` clause, the compiler attempts to supply the arguments automatically.

*   **Implicit Resolution**: `printValue("Hello")`. The compiler looks for a `Show[String]` instance.
    First it looks in the `given` clauses of the current function.
    If not found, it looks in the file scope and imported instances.

*   **Explicit Resolution**: `printValue(42) using (showInt2)`. The `using` clause manually supplies the dictionary, bypassing implicit search.

---

## 2. Resolution Rules

### Visibility and Scope
1.  **Local Scope**: "Given" parameters of the current function are highest priority.
2.  **File/Import Scope**: Instances defined at the top level of the file or imported are checked next.

### Ambiguity
*   **Rule**: For a required constraint `C`, if multiple candidates match and none is strictly more specific or locally scoped, it is an **Ambiguity Error**.
*   **Resolution**: The user must resolve ambiguity by providing an explicit instance via `using`.


### Interaction with generics matching

When typing a function call for a function `f[T1, ..., TN]`
* First, the arguments and return type are matched.
* Then, the `given` parameters are matched from left to right.
* For the first given parameter, use what is currently known about the type parameters, and match against all candidates.
    If no candidate matches, error.
    If exactly one candidate matches, use that candidate and continue with the next given parameter.
    If more than one candidate matches, error.
* Continue this process until all given parameters have been matched.

For example, the following would work since there is only one Converter with `Int` as the first type parameter.


```minifumo
typeclass Coverter[T1, T2]
    fun convert(value T1) T2

fun convert[A, B, C](value A) C given (a Converter[A, B], b Converter[B, C])
    b.convert(a.convert(value))

instance converterInt for Converter[Int, String]
    fun convert(value Int) String
        value.toString()

instance converterString for Converter[String, List[Char]]
    fun convert(value String) Int
        value.chars()
```



---

# Implementation Approach

## 1. Runtime implementation (Dictionary Passing)

The standard technique for implementing type classes is **Dictionary Passing**.

*   **Classes to Records**: The `typeclass Show[T]` is compiled to a struct (ADT or Record) holding the function pointers.
*   **Instances to Values**: `instance showInt` becomes a global value of that struct type.
*   **Functions to Explicit Arguments**: `fun printValueT given (s Show[T])` is compiled to `fun printValue[T](..., s Show_Dict[T])`.

## 2. Type Checking and Resolution Algorithm

The core complexity lies in the **Type Checker**. When a function call `f(x)` is encountered where `f` has a `given` clause, the checker must synthesize the implicit arguments.

### A. Constraint Solving
Let's say we need to resolve `Show[Int]`.

1.  **Collect Candidates**:
    *   Gather all `given` parameters from the current scope.
    *   Gather all visible `instance` declarations from the file/imports.

2.  **Unification & Matching**:
    *   For each candidate, check if its head type matches the required constraint.
    *   **Rigid vs. Flexible Types (Skolems)**:
        *   When checking the body of `fun <T> foo(...)`, `T` is a **Skolem** (a rigid constant). It only unifies with itself.
        *   When using a generic instance `instance <A> listShow for Show[List[A]]`, `A` is a **Meta-variable** (flexible).
        *   To match `Show[List[T]]` against `Show[List[A]]`, we unify `List[T]` with `List[A]`, resulting in `A = T`.

3.  **Search Strategy**:
    *   **Step 1**: Filter candidates that unify with the goal.
    *   **Step 2**:
        *   **0 matches**: Error ("No instance found for Show[Int]").
        *   **1 match**: Success. If the candidate has its own `given` clause (e.g., `Show[List[A]]` requires `Show[A]`), recursively resolve those sub-goals.
        *   **>1 matches**: Error ("Ambiguous implicit values").

### B. Handling `using` Clauses
If a `using` clause is present:
1.  Skip the implicit search.
2.  Type-check the provided expression (e.g., `showInt2`).
3.  Ensure the type of the provided expression matches the required constraint.

### C. Infinite Loop Prevention
Recursive instances can lead to infinite searches (e.g., `Eq[T]` requires `Eq[T]`).

*   **Termination Check**: Maintain a stack of "currently resolving constraints".
*   **Rule**: When resolving a sub-goal, ensure the type is **structurally smaller** than the parent goal, or strictly check that we haven't seen the exact same goal in the current branch of the search tree.
