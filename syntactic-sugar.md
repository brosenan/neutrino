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
F : (T1 -> T1) =>
declare update_first((T1, T2), F) -> (T1, T2).

update_first((X, Y), F) := (F!X, Y).

F : (T2 -> T2) =>
declare update_second((T1, T2), F) -> (T1, T2).

update_second((X, Y), F) := (X, F!Y).

assert update_second(update_first((2, 3), (N -> N - 1)), (N -> N + 1)) == (1, 4).
```

As can be seen in the last line, it is very hard to reason upon such changes with Neutrino's core syntax. Pipeline expressions, introduced through the `>>` operator are intended to fix that. The same example is given below, this time using the `>>` operator on the insertion. It compiles successfully:

```prolog
F : (T1 -> T1) =>
declare update_first((T1, T2), F) -> (T1, T2).

update_first((X, Y), F) := (F!X, Y).

F : (T2 -> T2) =>
declare update_second((T1, T2), F) -> (T1, T2).

update_second((X, Y), F) := (X, F!Y).

assert (2, 3) >> update_first(N -> N - 1) >> update_second(N -> N + 1) == (1, 4).
```

Indeed, the assertion on the last line is much more readable with the `>>` operator.

## Monad-Like Things

In Haskell, [monads](https://www.haskell.org/tutorial/monads.html) play a crucial part in allowing programmers to express program flow. While it is hard to get a simple explanation of what monads are (simple != [a monoid in the category of endofunctros](http://james-iry.blogspot.com/2009/05/brief-incomplete-and-mostly-wrong.html)), it is clear from the evidence that they convey the notion of _do this, then do that with the result_.

In Haskell, monad is a higher-order type-class, defined as follows:

```haskell
infixl 1  >>, >>=
class  Monad m  where
    (>>=)            :: m a -> (a -> m b) -> m b
    (>>)             :: m a -> m b -> m b
    return           :: a -> m a
    fail             :: String -> m a

    m >> k           =  m >>= \_ -> k
```

The most important part of this definition is the bind operator (`>>=`):

```haskell
    (>>=)            :: m a -> (a -> m b) -> m b
```

It is a function that takes two parameters, a monadic value of type `m a` (the current monad `m` with payload type `a`), and a function from the payload type `a` to a monadic value with some payload type `b`, and returns a monadic value with payload type `b`.

Different monads implement the bind operator differently, but most of them follow the same pattern. They take the original monadic value, extract the payload, and then call the given function to get the next monadic value.

For example, it is possible to define [a monad to abstract over `Maybe` values](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/Maybe):

```haskell
return :: a -> Maybe a
    return x  = Just x

    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    (>>=) m g = case m of
                   Nothing -> Nothing
                   Just x  -> g x
```

In this example the bind operator applies the given function if the value is `Just x` (i.e., if there is a value to read), and in the case of `Nothing` it propagates the `Nothing`ness.

Neutrino's concept of type classes is a bit different from Haskell's, so higher-order classes cannot be defined. For this reason we cannot define a type class named `monad` and give it a bind method, like they did in Haskell. However, monad-like code can still be written in Neutrino. Consider the Maybe monad described above. In Neutrino, the bind operator can be defined as follows. The following compiles successfully:

```prolog
F : (T1 -> maybe(T2)), F : delete =>
declare bind_maybe(maybe(T1), F) -> maybe(T2).

bind_maybe(X, Fn) := case X of {
    just(V) => Fn!V;
    none => none del Fn
}.

assert just(1) >> bind_maybe(N -> just(N+1)) >> bind_maybe(N -> just(N + 2)) == just(4).
assert just(1) >> bind_maybe(_ -> none) >> bind_maybe(N -> just(N + 2)) == none.
```

With the use of the `>>` operator, this code is relatively readable. However, we can do better. Neutrino does provide syntactic sugar similar to Haskell's [do notation](https://en.wikibooks.org/wiki/Haskell/do_notation), to help serialize monad-like code. The idea is to make the code more similar to procedural code, where operations are performed step-by-step. In each step the result is bound to one or more variables, which are then available down the road.

Because there is no single class named `monad`, we do not have a single keyword similar to Haskell's `do` keyword. Instead, the `<< {}` operator takes on its left hand the name of the bind function to be used. The following is the above example converted to this notation. It compiles successfully:

```prolog
F : (T1 -> maybe(T2)), F : delete =>
declare bind_maybe(maybe(T1), F) -> maybe(T2).

bind_maybe(X, Fn) := case X of {
    just(V) => Fn!V;
    none => none del Fn
}.

assert bind_maybe << {
    One := just(1);
    Two := just(One+1);
    just(Two + 2)
} == just(4).

assert bind_maybe << {
    _ := just(1);
    Two := none;
    just(Two + 2)
} == none.
```
