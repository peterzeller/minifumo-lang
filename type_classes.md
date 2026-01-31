# Minifumo Type Class Replacement

Minifumo no longer supports `typeclass`, `instance`, `given`, or `using`. Instead, use plain data types with function fields and pass them explicitly as parameters.

## 1. Define a data type

```minifumo
data Show[T] = Show(show T -> String)
```

## 2. Provide instances as functions

```minifumo
// Converts an Int to a String using the standard formatter.
fun showIntFn(value Int) String
    intToString(value)

fun showInt() Show[Int]
    Show(showIntFn)
```

## 3. Use explicit parameters

```minifumo
fun printValue[T](value T, s Show[T]) unit
    println(value, s)
```

This mirrors dictionary passing directly in user code and avoids implicit resolution.
