# Simple Expressions

Following is the simplest, no-op Neutrino program, which should compile successfully:

```prolog
assert true.
```

The `assert` statement takes a Boolean expression and tests that it evaluates to `true`. In this case, this holds trivially. If it doesn't, we get a compilation (or test-time) error:

```prolog
assert false.
```

```error
Assertion has failed. Expected true, got false.
```

Obviously, using an expression that is not Boolean will result in a compilation error.

```prolog
assert 42.
```

```error
Type mismatch. Expression 42 expected to be bool, inferred: int64.
```

The `==` operator takes two expressions of the same type, and returns whether or not they evaluate to the same value.

The following compiles successfully:
```prolog
assert 42 == 42.
```

But this should fail:
```prolog
assert 42 == "the answer".
```

```error
Type mismatch. Expression the answer expected to be int64, inferred: string.
```

The `+` operator takes two numeric values of the same type and returns a numeric value of that type, so should compile successfully:

```prolog
assert 1 + 1 == 2.
```

But this should fail.

```prolog
assert 1.0 + 1.0 == 2.
```

```error
Type mismatch. Expression 2 expected to be float64, inferred: int64.
```

