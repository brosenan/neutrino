# Type Classes and Polymorphism

[Type classes](https://en.wikipedia.org/wiki/Type_class) is a programming-language feature that exists in many programming languages (sometimes under the name _traits_, as in [Rust](https://doc.rust-lang.org/1.8.0/book/traits.html) or [Scala](https://docs.scala-lang.org/tour/traits.html)), and provides [ad-hoc polymorphism](https://en.wikipedia.org/wiki/Ad_hoc_polymorphism).

A type-class represents a contract, defined as a set of functions named _methods_. A type can be defined as an _instance_ of a type-class by defining these methods for this type.

In the following example, we define a type-class named `named_type`. Its contract consists of a single method, `type_name`, which takes an object of the instance type and returns a `string`, representing the name of the type.

The following compiles successfully:

```prolog
class T : named_type where {
    type_name(T) -> string
}.
```

With such a definition in place, concrete types can be assigned as _instances_ of the type class. For example, the type `int64` can be made an instance of `named_type` by implementing `type_name` to simply return the string `"int64"`. For `float64`, the method will return the string `"float64"`, etc.

The following compiles successfully:

```prolog
class T : named_type where {
    type_name(T) -> string
}.

instance int64 : named_type where {
    type_name(_) := "int64"
}.

instance float64 : named_type where {
    type_name(_) := "float64"
}.

assert type_name(3) == "int64".
assert type_name(3.0) == "float64".
```

## Class Declarations

A type-class declaration of the form `class T:C where { Decls }` consists of three elements:

1. The type-class name (`C`).
2. A free variable representing an instance of this type (`T`).
3. One or more function declarations (`Decls`), separated by semicolons (`;`), representing the methods.

The instance type must be represented by a free variable, as it abstracts over all possible instances. Placing a concrete type in its place will trigger a compilation error.

```prolog
class int64 : named_type where {
    type_name(int64) -> string
}.
```

```error
Expected instance type to be a free variable in the declaration of class named_type. Found: int64.
```

The declaration of each method must depend on `T`. `T` must appear in at least one of the parameters, and can possibly appear in the return type. If `T` does not appear in the argument types of a method, when this method is called Neutrino cannot determine which implementation it needs to use.

```prolog
class F : foo where {
    method1(F, int64) -> int64;
    method2(string) -> int64
}.
```

```error
Type variable F cannot be inferred in this context.
```

## Simple Instance Definitions

Simple, or monomorphic instance definitions are instance definitions that reference a single concrete type. The `named_type` example at the beginning of this doc defines such instances for `int64` and `float64`.

Obviously, for an instance definition to be valid, the class must exist.

```prolog
instance int64 : foo where {
    method1(A, B) := A+B
}.
```

```error
Type class foo does not exist.
```

An instance of a type-class must implement all methods declared by the type-class. In the following example the instance is missing a method.

```prolog
class F : foo where {
    method1(F, int64) -> int64;
    method2(F, string) -> int64
}.

instance int64 : foo where {
    method1(A, B) := A+B
}.
```

```error
Instance int64 of class foo defines less methods than required by the class.
```

The methods needs to be defined in the same order they were defined in the class.

```prolog
class F : foo where {
    method1(F, int64) -> int64;
    method2(F, string) -> string
}.

instance int64 : foo where {
    method2(_, S) := S;
    method1(A, B) := A+B
}.
```

```error
Expected method1/2 in instance int64 of class foo. Found method2/2.
```

Note that the notation `name/number` used by this error message is borrowed from Prolog and represents the name and number of arguments to a method, which are both checked.

Obviously, it is also illegal to provide too many methods.

```prolog
class F : foo where {
    method1(F, int64) -> int64;
    method2(F, string) -> string
}.

instance int64 : foo where {
    method1(A, B) := A+B;
    method2(_, S) := S;
    method3(N) := N
}.
```

```error
Instance int64 of class foo defines more methods than required by the class.
```

The methods defined in an instance must conform to the type signature they are declared with.

```prolog
class F : foo where {
    method1(F, int64) -> int64;
    method2(F, string) -> string
}.

instance float64 : foo where {
    method1(A, B) := A+B;
    method2(_, S) := S
}.
```

```error
Type mismatch. Expression B::float64 expected to be int64, inferred: float64.
```

## Polymorphic Functions

Type classes allow us to define polymorphic functions. These functions, given a single definition, actually define a "family" of simple functions, which differ only in their argument types.

Consider the `named_type` example given above. Using this type-class we can define a polymorphic function `greet_type` which takes a `string` greeting and an object of any `named_type` and returns a string greeting the type. The following compiles successfully:

```prolog
class T : named_type where {
    type_name(T) -> string
}.

T : named_type =>
declare greet_type(string, T) -> string.

greet_type(Greeting, Obj) := Greeting + ", " + type_name(Obj).

instance int64 : named_type where {
    type_name(_) := "int64"
}.

instance float64 : named_type where {
    type_name(_) := "float64"
}.

assert greet_type("hello", 3) == "hello, int64".
assert greet_type("hola", 3.0) == "hola, float64".
```

A polymorphic function cannot assume anything about the types it receives as parameters, except that they conform to the classes they are declared to conform to. For example, we cannot assume `Obj` is of type `string`.

```prolog
class T : named_type where {
    type_name(T) -> string
}.

T : named_type =>
declare greet_type(string, T) -> string.

greet_type(Greeting, Obj) := Greeting + ", " + Obj.
```

```error
Type mismatch.
```

When calling a polymorphic function, the compiler checks the assumptions hold for the given types. For example, if we call `greet_type` on a `float64` without first defining `float64` as an instance of `named_type`, we get a compilation error.

```prolog
class T : named_type where {
    type_name(T) -> string
}.

T : named_type =>
declare greet_type(string, T) -> string.

greet_type(Greeting, Obj) := Greeting + ", " + type_name(Obj).

assert greet_type("hola", 3.0) == "hola, float64".
```

```error
Type float64 is not an instance of class named_type.
```

A polymorphic function is not restricted to assigning a type to only one type class. The following compiles successfully:

```prolog
class F:foo where {
    foo(F) -> F
}.

class B:bar where {
    bar(B) -> B
}.

T:foo, T:bar =>
declare foobar(T) -> T.

foobar(X) := foo(bar(X)).
```

### Polymorphic Methods

Like regular functions, we sometimes wish type-class methods to be polymorphic as well, with regard to types other than the type-class instance.

Consider a class `foo` with method `foo` that takes an instance of the class and some other object (of `any` type), and returns a tuple, consisting of an instance of the class and the other type. The object of the other type must be the same as the object we received (we do not know anything on that type and therefore cannot apply anything on its value).

The following compiles successfully:

```prolog
class F : foo where {
    foo(F, T) -> F, T
}.

instance int64 : foo  where {
    foo(N, X) := N+1, X
}.
```

## Type Inference

The declaration of a polymorphic function may include type variables to describe both the types of the arguments and the return type. However, the return type must be _inferable_ from the argument types.

The following compiles successfully:

```prolog
declare identity(T) -> T.
```

But the following does not.

```prolog
declare foo(T1) -> T2.
```

```error
Type variable T2 cannot be inferred in this context.
```

### Parametric Classes as Type Relations

The inference of types in a declaration can either be direct (i.e., the type variables in the return type also appear in the argument types), or indirect through _parametric classes_.

If we consider a regular (non-parametric) type-class to be a _set_ of types, a parametric class can be seen as a [relation](https://en.wikipedia.org/wiki/Finitary_relation) between types.

Consider class `c` which takes a type parameter. The term `T1 : c(T2)` defines a relationship between types `T1` and `T2`. This relationship can be stated as _for any type `T1` there exists type `T2` such that `T1` is an instance of `c(T2)`_. Instance definitions for class `c` must then make sure this assertion holds. For example, the following instance definition does not compile because it does not specify `T2`.

```prolog
class T1 : c(T2) where {
    foo(T1) -> bool
}.

instance int64 : c(T2) where {
    foo(N) := N == 42
}.
```

```error
Type variable T2 cannot be inferred in this context.
```

But this compiles successfully:

```prolog
class T1 : c(T2) where {
    foo(T1) -> bool
}.

instance int64 : c(float64) where {
    foo(N) := N == 42
}.

instance list(T) : c(T) where {
    foo(_) := false
}.
```

Because instances specify their corresponding classes completely, function declarations can rely on them to infer their return types. Inference works from instance to class. For example, the following compiles successfully:

```prolog
class T1 : c(T2) where {
    foo(T1) -> T2
}.

T1 : c(T2) =>
declare bar(T1) -> T2.

bar(X) := foo(X).
```

In the above example type `T2` for function `bar` (and in fact for method `foo` as well) does not appear in the argument types, but can be inferred from `T1` through class `c`.

For an assumption of the form `T : C` to be valid (and infer the type variables in `C`), all variables in `T` must be known before this assumption is being evaluated. For example, the following code does not compile because `X` cannot be inferred from the argument types.

```prolog
class T1 : c(T2) where {
    foo(T1) -> T2
}.

X : c(T2) =>
declare bar(T1) -> T2.
```

```error
Type variable X cannot be inferred in this context.
```

Inference is done in a chain, from left to right. The following compiles successfully:

```prolog
class X : c(Y) where {
    foo(X) -> Y
}.

X : c(Y), Y : c(Z) =>
declare bar(X) -> Z.

bar(X) := foo(foo(X)).
```

But this does not:

```prolog
class X : c(Y) where {
    foo(X) -> Y
}.

% We switched the order.
Y : c(Z), X : c(Y) =>
declare bar(X) -> Z.

bar(X) := foo(foo(X)).
```

```error
Type variable Y cannot be inferred in this context.
```

### Type Inference in Function Bodies

The body of a polymorphic function must be general enough so that it would work correctly regardless of the actual type with which it is invoked. Consider a change to the `identity` function defined in one of the previous examples. Consider we would like to add 1 to the result. Such a definition will fail to compile because adding 1 assumes the argument type is `int64` (the type of the value 1), while the function is supposed to work with any input type.

```prolog
declare identity_plus_1(T) -> T.

identity_plus_1(X) := X + 1.
```

```error
Type mismatch.
```

Similarly, when a type is inferred via a class it is also generalized.

```prolog
class X : c(Y) where {
    foo(X) -> Y
}.

X : c(Y), Y : c(Z) =>
declare foo_foo_plus_1(X) -> Z.

foo_foo_plus_1(X) := foo(foo(X)) + 1.
```

```error
Type mismatch.
```
## Polymorphic Instance Definitions

Unlike a [monomorphic instance definition](#simple-instance-definitions), which define a single concrete type as an instance, a polymorphic instance definition defines a family of types as instances of a class. For example, the instance definition for `list(T)` in our [sequence example](sequence-example.md) is a polymorphic instance definition, because it abstracts over all element types (the different values `T` can take) and provides method definitions as polymorphic functions.

In the sequence example, `T` was just a type. We did not make any assumption on what it was. A list of things became a sequence of things. However, sometimes we need to make such assumptions.

Consider the `named_type` example from the beginning of this doc. So far we have seen how we could name concrete types. But how do we name parametric types such as `list(T)`? Obviously, we cannot do this for just any `T`. We need to assume `T` is by itself a `named_type`. If it is, naming `list(T)` is as simple as wrapping `T`'s name with `list(` and `)`.

The following compiles successfully:

```prolog
class T : named_type where {
    type_name(T) -> string
}.

T : named_type =>
instance list(T) : named_type where {
    type_name(L) := case L of {
        [] => "nil";
        [Elem | _] => "list(" + type_name(Elem) + ")"
    }
}.

instance int64 : named_type where {
    type_name(_) := "int64"
}.

assert type_name([1, 2, 3]) == "list(int64)".
assert type_name([]) == "nil".
```

## Built-In Type Classes

Neutrino defines a few type classes by itself, and assigns types to them automatically.

### The `delete` Class

Recall that Neutrino is [linearly-typed](simple-functions.md#linear-typing). This means that by default, every value must be used _exactly once_ in a function. But what do we do when we are done with an object? As discussed in the [simple functions](simple_functions.md) doc, we use underscore (`_`) to tell Neutrino we are not interested in it. Under the hood, Neutrino makes a call to the `del` operator to delete these values and reclaim their space. The `del` operator (which can also be called explicitly when needed), is a method of the `delete` class.

Neutrino automatically defines every struct and union type as instances of the `delete` class.

For example, the following compiles successfully:

```prolog
struct foo(T) = foo(T, T).
union bar(T) = bar1(T, int64) + bar2(string, T) + bar3.

assert (2 del foo("hello", "world")) == 2.
assert (3.14 del bar1("hello", 42)) == 3.14.
```

### The `basic_type` Class

Basic types are types that do not need to adhere to the rules of linear typing. Instances of this class can be used zero or more times in a function, with no restrictions. Neutrino automatically assigns eligible types to this class. Programmers _cannot_ do so themselves.


```prolog
struct foo = foo(int64).

instance foo : basic_type where {
    something(X) := Y
}.
```

```error
Type class basic_type does not exist.
```

Numeric types such as `int64` and `float64` are defined as instances of `basic_type`. All references are instances too.

The following compiles successfully:

```prolog
T : basic_type =>
declare check(T) -> bool.

check(X) := true.

assert check(3).
assert check(3.14).

declare check_ref(string) -> bool.
check_ref(S) := check(&S) del S.
```

In the above example, the `check` function is legal because it requires that its argument type is of a `basic_type`. This allows it to ignore it.

Union types that do not have any data fields (e.g., `bool`), are instances of `basic_type` as well. The rationale for this is that such union types do not need allocation or de-allocation. Every option that does not have fields does not require an allocation and can be represented with a sentinel. If all options can be represented with a sentinel, there is no need to allocate anything.

The following compiles successfully:

```prolog
T : basic_type =>
declare check(T) -> bool.

check(X) := true.

union foobar = foo + bar + baz.

assert check(foo).
assert check(bar).
assert check(baz).
```
