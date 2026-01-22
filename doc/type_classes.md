This document specifies Minifumo type classes as used by the grammar in
`src/main/antlr4/Minifumo.g4` and the example in `doc/examples/typeclasses.minifumo`.
The design follows Scala 3 with two explicit differences:
1) only `instance` blocks define typeclass instances
2) the type system is intentionally simpler

Overview
--------
Type classes provide ad-hoc polymorphism. A type class declares a set of
functions; an instance provides implementations for a concrete type. Functions
can require instances via a `given` clause, and the compiler resolves those
instances from the available `instance` blocks.

Syntax
------
Type class declaration:
```
typeclass Show[T]
    fun show(value T) String
```

Instance declaration:
```
instance showString for Show[String]
    fun show(value String) String
        value
```

Type class constraints on functions:
```
fun printValue[T](value T) given (s Show[T])
    println(show(value))
```

Notes based on the grammar:
- `typeclass` introduces a type constructor (e.g. `Show[T]`) and a set of
  function signatures.
- `instance` defines a named instance for a specific type application.
- `given (x C[T])` attaches implicit parameters (constraints) to a function or
  instance.
- Calls may optionally include `using (...)` to disambiguate instance
  resolution.

Semantics
---------
Symbol tables
- The global symbol table contains all type class names and the names of all
  instances. Instance names are available for reference, but instance selection
  is based on type class matching, not on names.

Type class declaration
- A `typeclass` defines a new type constructor `C[T1, ..., Tn]` and a set of
  member function signatures. The signatures are abstract and must be
  implemented by every instance.
- Member signatures are in the same syntax as `fun` signatures. Type parameters
  on the type class are in scope in member signatures.

Instance declaration
- An `instance` introduces a concrete implementation for a type class
  application, optionally polymorphic:
  - `instance name for C[Args]` defines an instance head.
  - Optional type parameters on the instance are universally quantified.
  - An optional `given` clause declares additional required instances.
- Each instance must provide a `fun` implementation for every member declared
  by the type class. The implementation types must match the declared member
  types after substituting the instance head type arguments.
- Instances are values only for resolution; there are no user-defined "given"
  values outside of `instance` blocks.

Given clause
- A `given` clause lists implicit parameters (constraints) that the compiler
  must resolve automatically at call sites.
- Parameters in the `given` clause are in scope in the body as normal
  parameters. They may be referenced by name, but usually member functions can
  be called directly (see "Member resolution" below).

Member resolution
- A call to a type class member `m` is resolved by finding a suitable instance
  for the member's type class, then calling that member on the instance.
- Informally, `show(value)` in a scope with `given (s Show[T])` is treated as
  `s.show(value)`. If no matching given parameter exists, instance resolution
  is performed.

Using clause
- A call may include `using (T1, ..., Tn)` to disambiguate instance selection.
  Each type in the list corresponds to a required given parameter. The types
  are used to filter candidates to those with matching instance heads.

Differences from Scala 3
------------------------
- The only way to define an instance is with an `instance` block.
- There are no local `given` definitions, no implicit conversions, and no
  priority rules beyond the simple resolution described below.
- The type system does not include higher-kinded types, implicit function
  types, or overloading based on extension methods. Resolution is based on
  simple first-order type unification.

Type Checking and Constraint Resolution
---------------------------------------
The type checker operates in two phases: (1) build global declarations and
instance tables, and (2) type check terms while generating and solving type
class constraints.

Phase 1: Collect declarations
1) Collect all type class declarations into a map:
   - `TypeClassEnv[C] = (typeParams, members)`
2) Collect all instance declarations into a global instance table:
   - `InstanceEnv` contains entries `(name, tparams, head, givens, members)`,
     where `head` is `C[Args]`.
3) Populate the global symbol table:
   - Type class names and instance names are registered in the global table.
3) Validate each instance:
   - The referenced type class exists.
   - The instance head has the correct arity.
   - Every type class member has a matching `fun` in the instance.
   - The implementation signature matches the declared member type after
     substituting the instance head type arguments.

Phase 2: Type checking with constraints
Type inference produces a type for each expression and a set of required
constraints of the form `C[Args]`.

Rules:
- Function signatures with `given` clauses introduce implicit parameters
  that are added to the local "given environment".
- When a type class member is called, the type checker introduces a constraint
  requiring a matching instance (unless a specific given parameter is in
  scope, in which case that parameter is used directly).
- Constraints are accumulated as part of type inference and are solved once
  their type arguments are fixed by unification.

Constraint solving algorithm
----------------------------
The resolution procedure is similar to Scala 3 implicit search but simplified.

Inputs:
- `Goal`: a required constraint `C[Args]`
- `GivenEnv`: the list of in-scope given parameters from function/instance
  signatures
- `LocalInstanceEnv`: instances defined in the current scope (if supported by
  the surrounding module or block structure)
- `ImportedInstanceEnv`: instances brought in from imports

Resolution steps:
1) Try local givens first:
   - Find given parameters in `GivenEnv` with a type that unifies with `Goal`.
   - If exactly one candidate matches, use it.
   - If multiple match, report an ambiguity error.
2) Try local instances:
   - Search `LocalInstanceEnv` for instances whose head unifies with `Goal`.
   - If exactly one candidate succeeds (including recursive resolution of its
     own `given` clause), select it and stop. Do not search outer scopes.
   - If more than one succeeds, report an ambiguity error.
3) Try imported instances:
   - Search `ImportedInstanceEnv` for instances whose head unifies with `Goal`.
   - Apply the same unification and recursive resolution as for local instances.
   - If exactly one candidate succeeds, select it.
   - If none succeed, report a "no instance found" error.
   - If more than one succeeds, report an ambiguity error.
Selection and shadowing:
- If the inner scope has a unique instance, that instance is chosen and the
  outer scope is not consulted. No conflict error is reported in this case.

Unification and recursion
- Unification is first-order: it matches type constructors and their arguments.
- Occurs checks are required to avoid infinite types.
- The resolver maintains a stack of goals; if the same goal is already on the
  stack, report a cycle error.

Interaction with type inference
- If a goal contains unresolved type variables, it is deferred until those
  variables are fixed by unification from other typing constraints.
- After inference, all remaining goals must be resolvable; otherwise, the type
  checker reports an error.

Examples and expected behavior
------------------------------
- If two instances match the same type class application (e.g. `Show[Int]`),
  any use that requires `Show[Int]` is ambiguous and results in a type error.
- In the example, `printValue("Hello World")` resolves `Show[String]` to
  `showString`, enabling `show(value)` to call the instance implementation.
