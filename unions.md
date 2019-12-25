# Union Types and Case Expressions

Union types are types that hold different data alternatives. For example, the following definition compiles successfully:

```prolog
union num_or_str = num(int64) + str(string).
```

This definition defines a new type, `num_or_str`, which represents either an `int64` number (`num(int64)`), or a string (`str(string)`).

The different options may have zero or more parameters, which need to all be types. For example, the following will fail:

```prolog
union num_or_str = num(3) + str("foo").
```

```error
Expected type, found 3.
```

## Parametric Union Types

Union types can be parametric, and depend on other types. A simple example is a binary tree, where the type of the value held in the leafs is given as parameter.

The following compiles successfully:

```prolog
union tree(T) = leaf(T) + node(tree(T), tree(T)).
```

Type parameters must be free variables.

```prolog
union foo_type(X, 42) = foo + bar.
```

```error
Expected variable, found 42.
```

As in the `tree` example, variables are allowed in the right-hand-side of the definition. However, only variables that are introduced in the left-hand-side are allowed.

```prolog
union foo_type(X) = foo(Y) + bar(Z).
```

```error
Variable Y is not introduced in the definition of foo_type.
```

## Case Expressions

Case expressions provide a way to test a union-type value against its different options. For example, the following code defines a union type `foobar` that can either be `foo` or `bar`. Then a `case` expression over `foo` maps `foo` to `true` and `bar` to `false`.

The following compiles successfully:

```prolog
union foobar = foo + bar.
assert case foo of {
    foo => true;
    bar => false
}.
```

Case expressions only work for union types. Using them on other types (e.g., numbers) will fail to compile.

```prolog
assert case 2 of {
    1 => false;
    2 => true
}.
```

```error
Expected union type expression. Found 1 of non-union type int64.
```

The mapping enclosed in the braces should reference all the options defined for the type.

```prolog
union foobar = foo + bar.
assert case foo of {
    foo => true
}.
```

```error
Incomplete case expression for type foobar. Missing cases: bar.
```

Additionally, they have to appear in the order in which they were defined.

```prolog
union foobar = foo + bar.
assert case foo of {
    bar => false;
    foo => true
}.
```

```error
Expected case foo, found bar.
```

### Case Expressions over Union Types with Data

When working on a union type with data elements, the case expression binds these data elements to variables.

The following compiles successfully:

```prolog
union num_or_str = num(int64) + str(string).
assert case num(3) of {
    num(N) => N == 3;
    str(S) => S == "foo"
}.
```

In the above example, the value `num(3)` is matched against either the pattern `num(N)` (which it matches), or the pattern `str(S)`, which it doesn't. For `num(N)` we check that the value bound by `N` is indeed 3, and for `str(S)` we simply return `false`.

In such `case` expressions, the arity (number of arguments) of each pattern must match its definition in the union type.

```prolog
union num_or_str = num(int64) + str(string).
assert case num(3) of {
    num(N1, N2) => N1 == 3;
    str(S) => S == "foo"
}.
```

```error
Undefined expression num/2.
```

The pattern arguments themselves must be _l-value_, which in the case of simple types such as `int64` and `string` (as in the case of union types), consists only of variables.

```prolog
union num_or_str = num(int64) + str(string).
assert case num(3) of {
    num(N) => N == 3;
    str("hello") => false
}.
```

```error
Expected l-value of type string. Found hello.
```

A `case` expression evaluates to the value at the right-hand side of the `=>` operator on the branch that matches the pattern. Its branches need to have the same type.

```prolog
union num_or_str = num(int64) + str(string).
assert case num(3) of {
    num(N) => N == 3;
    str(S) => 42
}.
```

```error
Type mismatch. Expression 42 expected to be bool, inferred: int64.
```

The variables bound by the patterns are typed according to the union type definition.

```prolog
union num_or_str = num(int64) + str(string).
assert case num(3) of {
    num(N) => N == 3;
    str(S) => S == 4
}.
```

```error
Type mismatch. Expression 4 expected to be string, inferred: int64.
```

Ultimately, the type of all branches must match the type expected for the whole expression.

```prolog
union num_or_str = num(int64) + str(string).
assert case num(3) of {
    num(N) => N;
    str(S) => 42
}.
```

```error
Type mismatch. Expression case num(3)of{num(N::int64)=>N::int64;str(S::string)=>42} expected to be bool, inferred: int64.
```

### Case Expressions and Linear Typing

As discussed for [simple functions](simple-functions.md), Neutrino is linearly-typed, meaning that variables holding non-basic types must be consumed exactly once. In case expressions, this is done on a per-branch basis.

In the following example the string variable `S` is introduced in the second branch and is used twice. This causes a compilation error.

```prolog
union foobar = foo(int64) + bar(string).

assert case foo(3) of {
    foo(N) => N == N;
    bar(S) => S == S
}.
```

```error
Variable S of non-basic type string is used more than once.
```

Note that the error refers to variable `S` defined in the second branch and not variable `N` defined in the first branch, because the latter is of type `int64` which is a basic type.

Because branches represent alternative execution, they should all agree on which variables are introduced or consumed. For example, the second branch in the following code _leaks_ variable `S` (but the first branch does not).

```prolog
union foobar = foo(string) + bar(string).

assert case foo("bar") of {
    foo(S) => S == "foo";
    bar(S) => true
}.
```

```error
Variable S of non-basic type string is not used in this context.
```

To fix this, use `_` to replace unused variables. The following compiles successfully:

```prolog
union foobar = foo(string) + bar(string).

assert case foo("bar") of {
    foo(S) => S == "bar";
    bar(_) => false
}.
```

