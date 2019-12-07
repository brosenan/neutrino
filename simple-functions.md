# Simple Functions

Simple, or monomorphic functions are functions for which each parameter has a single, predetermined type, and as result, the function also evaluates to a single, predetermined type.

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

The header of a function must only contain l-values, which in the case of simple types is limited to variables.

```prolog
f(2) := 3.
```

```error
Expected l-value of type int64. Found 2.
```

## Implicit and Explicit Type Signatures

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

## Linear Typing

[Linear type systems](https://en.wikipedia.org/wiki/Substructural_type_system#Linear_type_systems) are type systems that make sure objects are being used only once. Being linearly typed gives Neutrino much of its power in terms of performance.

As in probably every other functional programming language, a variable (of any type) must be used at most once in the function's header.

```prolog
declare f(int64, int64) -> int64.
f(A, A) := A.
```

```error
Variable A has already been introduced in this context.
```

Also like other languages, variables used in the body of a function must be first introduced in its head (or in a [case expression](unions.md#case-expressions).

```prolog
declare f(int64) -> int64.
f(A) := B.
```

```error
Variable B has not been introduced in this context.
```

What makes linear typing unique is the restriction on non-basic types (types other than numbers, Booleans or references) to be used only once. The following function is illegal, because the string `S` is being used twice in its body.

```prolog
declare f(string) -> bool.
f(S) := S == S.
```

```error
Variable S of non-basic type string is used more than once.
```

In linear typing, non-basic types can be used _exactly_ once in the body of a function. For example, the following function will not compile because variable `X` is not used:

```prolog
declare f(string) -> bool.
f(X) := true.
```

```error
Variable X of non-basic type string is not used in this context.
```

This means that when a parameter is not used by a function, it must not be bound to a variable in the first place. Neutrino uses the `_` singleton variable for that.

The following code compiles successfully:

```prolog
declare f(string) -> bool.
f(_) := true.
```
