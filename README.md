# Minifumo Lang

Minifumo is a programming language with the following ideas:

- Minifumo builds on minimal functional modules instead of big frameworks on libraries
- The language supports open source collaboration on a shared global module database.
- Minifumo can be compiled to many languages.
    As such, it can be used to define functions that need to be used in the context of different languages.
- About the language itself:
    - Syntax is inspired by Scala, F#, Python, Go, and Math
    - Functional style by default (with mutability as opt-in)
    - Statically typed, no sub-typing, with simple generics
    - Type classes via (implicit) parameters (as in Scala or Lean)
    - Algebraic data types (like enums in Scala, Swift, Rust)
    - Errors as return values (panics only for rare cases, as in Go)
    - No runtime reflection

## The idea of the shared global module database

Everyone can add new modules to the database.
Each function and each datatype is its own module.

Everyone can improve existing functions by:
- Adding documentation
- Adding examples
- Adding tests
- Defining properties for the function

Everyone can improve properties by:
- Adding a counter example for wrong properties
- Adding a proof for correct properties (theorems)

Everyone can improve data types by:
- Implementing type class instances
- Implementing conversion functions

And finally, everyone can add challenges for implementing a function.
Other humans and machines can then search for correct implementations of those functions.

Of course, code is written in files as in most programming languages.
And it is possible to work without the global module database and import modules from (private) Git repos.

## The idea of minimal functional modules

Big libraries do not work.
It is not possible to build a stable library if the library is too big.
It is much easier to build one function and make it work well.

So, why don't existing languages use minimal functional modules?

- **Interoperability**:

    It is hard to make different functions work well together if they are not designed together.

    Minifumo solves this by providing a rich set of standard data types that unify different places of usage.

    Also, Minifumo tools can (in the future) automatically find conversion functions between user defined data types to transform a function working with type `A` to a function working with type `B`.


- **Discoverability**:

    When having a lot of small modules it is important to be able to find the right module quickly.

    This is a problem that can be fixed by having a good database and search functionality.

    In the age of AI, an AI might simply rewrite an existing function from scratch,
    but good tools can detect that duplication and use a canonical implementation.

- **Dependency Hell**:
    Importing one function might import two more functions, which again import more functions.
    This can lead to a lot of imports.

    However, this is still better than with big libraries:
    We only import what we really need and nothing else.

    The real problem with dependency hell in other languages are stability and security, which we discuss below.


- **Stability**:
    If a program can have thousands of dependencies, other languages can make a developers' life very hard when updating.
    Minifumo solves this problem by making libraries immutable.

    It is possible to link a new version of a function in the global database, but there is no need to update unless the used functions are broken.

    In the approach with big libraries, one needs to update a lot of functions that are bundled together, which can lead to a lot of works when migrating.

    With the use of properties, the community can precisely describe and verify the backwards-compatibility promises of updated versions.


- **Security**:
    A big problem with small libraries in other languages is trust:
    How can we trust the authors of the thousands of libraries that we import?

    In Minifumo, this problem is solved on a language level by making the language secure by default.
    A function can only use what is passed to it via function arguments.
    There is no global state and no dangerous built-in functions.

    So, a function can never access files on your computer, unless you pass it a file system.
    A function can never access the internet, unless you pass the internet as a parameter.

    Minifumo is secure by default.

- **Trust on correctness**
    When using a small library from a single author, how can we trust that author to implement the function correctly?

    Minifumo addresses this challenge by involving the community.
    Since everyone can add tests and properties to a function, over time the trust in the correctness of functions increases.

- **Abstract data types**
    Abstract data types are the only use-case in Minifumo that sometimes requires a bigger module.
    An abstract data type typically maintains an invariant that is more that the structure of the representation type.

    There are two ways to define an abstract data type in Minifumo:
    1. Define a data type and an invariant on the data type.
        The invariant is a function that takes an instance of the data type and returns a Boolean.
        When construction an instance of the data type, it is checked that the invariant is true (either with a runtime check or proven via a property).
    2. An encapsulated data type that can only be constructed via a small set of function and does not expose its internal representation.

## Development setup (Codex)

See `AGENTS.md` for the Codex setup and test commands.
