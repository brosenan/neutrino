# Struct Types

Similar to [union types](unions.md), struct types are user-defined compound types. Unlike union types, a struct type has only one constructor.

In the following example we define a simple struct type named `foo`, which contains two fields, an `int64` and a `string`. It compiles successfully:

```prolog
struct foo = foo(int64, string).
```

## Destructing and Constructing Structs

Unlike most imperative languages, where struct types give names to their fields and code can reference them by name, in Neutrino to access the fields of a struct it needs to be _destructed_. As the name implies, destructing a struct means it no longer exists as a struct.

They are constructed using their constructor.

The following compiles successfully:

```prolog
struct foo = foo(int64, string).

the_number(foo(N, _)) := N.
the_string(foo(_, S)) := S.

assert the_number(foo(3, "hello")) == 3.
assert the_string(foo(3, "hello")) == "hello".
```

## Parametric Struct Types

Similar to [union types](unions.md), struct types can also be parametric. Parametric types take type parameters, which then can be used in the constructor. For example, the following snippet defines a parametric type `pair`, which has two elements of the same type `T`. It compiles successfully:

```prolog
struct pair(T) = pair(T, T).
```

Parameters in a struct type definition must always be free variables. Anything else will cause a compilation error.

```prolog
struct pair(int64) = pair(int64, int64).
```

```error
Expected variable, found int64.
```

Only variables that are introduced in the left-hand-side are allowed in the right-hand side.

```prolog
struct pair(T) = pair(X, Y).
```

```error
Variable X is not introduced in the definition of pair.
```
