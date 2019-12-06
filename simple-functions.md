# Simple Functions

Monomorphic functions are functions for which each parameter has a single, predetermined type, and as result, the function also evaluates to a single, predetermined type.

For example, the following function has a single parameter of type `int64`, and returns the same type, such that the following code compiles successfully:

```prolog
f(X) := X+2.

assert f(3) == 5.
```

In the following, we show that the function's type here is `int64`.

```prolog
f(X) := X+2.

assert f(3).
```

```error
Type mismatch. Expression f(3) expected to be bool, inferred: int64.
```

An here we show that the parameter type is expected to be `int64`.

```prolog
f(X) := X+2.

assert f("hello") == 5.
```

```error
Type mismatch. Expression hello expected to be int64, inferred: string.
```

### Implicit and Explicit Type Signatures

In the above example, the compiler managed to infer the function's signature, because we added an integer literal to `X`, inferring that its type (and the type of the result) is `int64`. However, in the following example the compiler cannot infer the type of `X`.

```prolog
f(X) := X+X.
```

```error
Cannot infer the type of argument X. Please provide an explicit declaration.
```

Providing an explicit type declaration causes the code to compile successfully:

```prolog
declare f(float64) -> float64.
f(X) := X+X.

assert f(3.0) == 6.0.
```

Of course, if the function's definition does not match its declaration, a compilation error is produced.

```prolog
declare f(float64) -> float64.
f(X) := X+2.
```

```error
Type mismatch. Expression X::int64 expected to be float64, inferred: int64.
```
