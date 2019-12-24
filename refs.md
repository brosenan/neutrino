# References

Neutrino's linear type system restricts functions to only use objects once. However, sometimes this could be too restrictive. Sometimes we only need to take a little pick at an object, without changing anything (that is, without destroying it and creating related objects), and we may want to do this multiple times.

For this, Neutrino has _references_. Every non-basic type (e.g., strings, union types and structs) can have references made to it. Some operations are defined on references. For example, checking the length of a string does not require destroying the string so it makes sense for this function to only take a reference to a string. In contrast, any function making changes to a string (e.g., changing a character), requires consuming the string, and therefore cannot be defined to work on a reference.

In return to the limited functionality, references are not exclusive and can be used multiple times within a function.

In the following example we define a function that takes a reference to a string and uses the built-in function `strlen` twice to return the double of the string's length. It compiles successfully:

```prolog
declare double_len(&string) -> int64.

double_len(S) := strlen(S) + strlen(S).
```

Note that if this were a regular string (not a reference), we wouldn't have been able to use it twice.

## Matching References

Destructing of union and struct types can work on references. In the following example, a function takes a reference to a list. It uses a case expression to match the two options for a list (an empty and a non-empty list). We add a `&` between the `of` and the `{` to note that this case expression works on a reference, in which case the component values exposed by destructing the list are references as well.

The following compiles successfully:

```prolog
declare sum_lengths(&list(string)) -> int64.

sum_lengths(L) := case L of & {
    [] => 0;
    [S | L1] => strlen(S) + sum_lengths(L1)
}.
```

Destructing a reference to a struct is done by adding `&` before the struct's constructor.

The following compiles successfully:

```prolog
struct foo = foo(int64, string).

declare bar(&foo) -> int64.

bar(&foo(N, S)) := strlen(S) + N.
```

## Referencing Owned Objects

When a function owns an object (i.e., receives it as a non-reference), it may _lend_ a reference to the object to other functions. There is no restriction on the number of times an object can be lent. However, an object cannot be consumed while being lent.

In the following example we define functions `foo` and `bar`. `foo` takes ownership of a string. It calls `strlen` twice, each time with a reference of that string, and then calls `bar` giving it the two length values and the string itself (passing ownership). It compiles successfully:

```prolog
declare bar(int64, string, int64) -> int64.
bar(N1, _, N2) := N1 + N2.

foo(S) := bar(strlen(&S), S, strlen(&S)).
```

The above example compiles because the two borrows happen _before_ `S` is being consumed. The two calls to `strlen` happen first, and then the results are forwarded to `bar`. However, if we wish to send `S` to a function both as an owned object and as a reference, we would get a compilation error.

```prolog
declare bar(&string, string) -> int64.
bar(_, _) := 4.

foo(S) := bar(&S, S).
```

```error
Attempt to both consume and lend variable S in a single step.
```

The order of parameters does not matter.

```prolog
declare bar(string, &string) -> int64.
bar(_, _) := 4.

foo(S) := bar(S, &S).
```

```error
Attempt to both consume and lend variable S in a single step.
```
