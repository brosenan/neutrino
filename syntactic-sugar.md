# Syntactic Sugar

At its core, Neutrino is a minimalistic language. Expressions in core Neutrino consist of only the following:

1. Literals.
2. Variables.
3. References.
4. Function invocations.
5. Case expressions.

Riding on Prolog's parser, infix operators provide the illusion of a richer set of expressions, but these are interpreted as function calls.

Neutrino's minimalism reduces the number of edge cases the compiler has to cope with, thus allowing powerful features such as partial evaluation and linear typing. However, minimalism comes at a cost in term of usability.

To bridge this gap, Neutrino comes with limited syntactic sugar, intended to improve developer productivity. This syntactic sugar is consumed by the compiler in the early stages of compilation, thus not complicating the core language.

The most important and most complicated piece of syntactic sugar is [lambda support, which has its own doc](lambdas.md). Here we focus on the assortment of simpler features.

## If Expressions

As discussed in [the unions doc](unions.md#built-in-union-types), the `if` expression is syntactic sugar, transforming itself into case expressions over a `bool`. The following compiles successfully:

```prolog
delta(N) := if(N == 0, 1, 0).

assert delta(0) == 1.
assert delta(42) == 0.
```

## Expression Pipeline

It is common that we wish to apply a sequence of functions to a value, where each function takes as parameter the output from the previous function. This is common, e.g., when we perform multiple updates to a value, where each update is performed by taking that value (and possibly some other values) as parameter(s), and returning a new value.

Consider for example the following [lens](https://medium.com/@dtipson/functional-lenses-d1aba9e52254) functions `update_first` and `update_second`. These functions take a pair (`(X,Y)`) and a lambda, and return a new pair, applying the lambda on either the first or second element of the given pair.

The following compiles successfully:

```prolog
T1 : any, F : (T1 -> T1), T2 : any =>
declare update_first((T1, T2), F) -> (T1, T2).

update_first((X, Y), F) := (F!X, Y).

T1 : any, T2 : any, F : (T2 -> T2) =>
declare update_second((T1, T2), F) -> (T1, T2).

update_second((X, Y), F) := (X, F!Y).

assert update_second(update_first((2, 3), (N -> N - 1)), (N -> N + 1)) == (1, 4).
```

As can be seen in the last line, it is very hard to reason upon such changes with Neutrino's core syntax. Pipeline expressions, introduced through the `>>` operator are intended to fix that. The same example is given below, this time using the `>>` operator on the insertion. It compiles successfully:

```prolog
T1 : any, F : (T1 -> T1), T2 : any =>
declare update_first((T1, T2), F) -> (T1, T2).

update_first((X, Y), F) := (F!X, Y).

T1 : any, T2 : any, F : (T2 -> T2) =>
declare update_second((T1, T2), F) -> (T1, T2).

update_second((X, Y), F) := (X, F!Y).

assert (2, 3) >> update_first(N -> N - 1) >> update_second(N -> N + 1) == (1, 4).
```

Indeed, the assertion on the last line is much more readable with the `>>` operator.
