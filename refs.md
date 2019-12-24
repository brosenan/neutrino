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

