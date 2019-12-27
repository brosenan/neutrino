# Lambdas

Lambdas are an important part of any functional programming language. While lambdas today are important features in almost any programming language, in functional programming they are simply essential.

In Neutrino, lambdas are handled differently than in most functional programming languages, due to the lack of garbage collection. The concept of lambdas in Neutrino is therefore similar to that in other "unmanaged" languages such as C++, and is most similar to that in Rust.

Here are the two main differences between lambdas in Neutrino and those in other functional programming languages such as Haskell:

1. In Haskell regular functions and lambdas are interchangeable. You can take a regular function, place it in a variable and call it the same way you would a lambda. In Neutrino they are two different things. Functions are functions, lambdas are lambdas.
2. In Haskell, all functions (and lambdas) with the same domain and range types are objects of the same type, and therefore can be stored together in a list. In Neutrino the signature of a lambda is a [type class](type-classes.md) and different lambdas are of different types in that class.

Another difference that was borrowed from Rust stems from the nature of linear types. In Neutrino there are two type classes for lambdas: `->` and `&->`. The former represents a lambda that can be used only once (it is consumed by the `!` operator that is used to invoke it), similar to Rust's [FnOnce](https://doc.rust-lang.org/std/ops/trait.FnOnce.html) trait. In contrast, `&->` only needs a reference to be invoked (by the `&!` operator) to that a single instance can be invoked multiple times, similar to Rust's [Fn](https://doc.rust-lang.org/std/ops/trait.Fn.html) trait. However, it can only use its closure parameters by reference.

Note that Rust has a third lambda trait, [FnMut](https://doc.rust-lang.org/std/ops/trait.FnMut.html), but it is irrelevant for Neutrino because it allows the lambda to mutate its closure parameters. Data in Neutrino is immutable.

## Invoke-Once Lambdas

The type class `->` represents lambdas that can be used once. If they have closure parameters (parameters that are carried as part of the state of the lambda and not coming from the invocation), these parameters are owned by the lambda object and are then consumed by the invocation.

In the following example, function `f` takes a value and a lambda, and applies the lambda to the value. It compiles successfully:

```prolog
F : (int64 -> int64) =>
declare f(F, int64) -> int64.

f(F, N) := F!N.

assert f((N -> N+2), 3) == 5.
```

Lambdas be based on closure parameters. In the following example we create a function that returns a lambda for greeting things. It takes the greeting (e.g., "hello") as parameter and creates a single-use lambda that takes something to greet (e.g., "world") and returns the full greeting. It compiles successfully:

```prolog
greeting(Greeting) := (Thing -> Greeting + ", " + Thing).

assert greeting("hello")!"world" == "hello, world".
```

As mentioned above, closure variables are _owned_ by the closure. This is true even if the lambda just uses the object by reference.

```prolog
assert (S -> strlen(&S))!"hello" == 5.
```

```error
Variable S of non-basic type string is not used in this context.
```

To solve this problem, use the explicit `del` operator within the lambda's body. This compiles successfully:

```prolog
assert (S -> strlen(&S) del S)!"hello" == 5.
```

### Currying

[Currying](https://en.wikipedia.org/wiki/Currying) is a style that allows functions in the lambda calculus, where a function application takes only one parameter, to take several parameters. It is done in steps, where in each step the lambda expression returns a lambda expression that accepts the rest of the parameters.

In Neutrino, there are two standard ways to provide multiple parameters to a lambda. One is by the use of tuples, and the other is through the use of currying. The following example demonstrates these two options for a function that sums three integers. It compiles successfully:

```prolog
using_a_tuple := X, Y, Z -> X+Y+Z+0.

assert using_a_tuple!(1, 2, 3) == 6.

by_currying := X -> Y -> Z -> X+Y+Z+0.

assert by_currying!1!2!3 == 6.
```

Note that in both examples we add a `+0` to the sum to hint that the value is of type `int64`.

Because Neutrino uses partial evaluation as part of the compilation process, both methods should be equivalent from a performance standpoint. Currying provides more flexibility in terms of partial application.

## Lambdas for Multiple Invocations

Often we need to invoke a lambda multiple times. One example is applying a function to every element on a list (think map, filter or reduce operations). Neutrino supports lambdas for multiple invocations through the `@>` type class.

The `@>` class defines one method, the `@` operator, which instead of consuming the lambda it takes a reference to it. This allows the lambda to be used multiple times. On the down-side, the lambda only gets reference to the colsure parameters.

In the following example we define a polymorphic `map_list` function, which applies the given lambda on every element of a list. It consumes one list and generates another in return. It compiles successfully:

```prolog
T1 : any, T1 : any, F : (T1 @> T2), F : delete =>
declare map_list(list(T1), F) -> list(T2).

map_list(L, F) := case L of {
    [] => [] del F;
    [X | Xs] => [&F@X | map_list(Xs, F)]
}.
```
