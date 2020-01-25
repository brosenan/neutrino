# Input and Output

So far we have defined Neutrino as a purely-functional programming language. This purity makes Neutrino very powerful. For example, the lack of side-effects makes it safe for the compiler to perform partial evaluation on code, without having to worry about missing a desired effect at runtime.

But sometimes we want programs to actually do something. Actually, there is not much point in a program if it cannot _do_ anything. So to allows Neutrino programs to do things, Neutrino defines the `io` type.

`io` is a built-in type which represents the state of the world. A Neutrino program has exactly one `io` value, which is given to its `main` function as its sole parameter, and `main` is expected to return it. `io` is not an instance of `delete`, so it cannot be destroyed and functions receiving it are obliged to pass it along like a hot potato.

Impure functions, i.e., functions that interact with the outside world, always take and return `io`. Built-in functions such as `print`, which prints out a string, are defined in such a way that their first argument is always of type `io`, and their return type is a pair (`IO,X`), of which the first element (`IO`) is of type `io`.

The following is Neutrino's "hello, world" example, which compiles successfully:

```prolog
main(IO) := let << {
    IO1, _ := print(IO, "Hello, World!");
    IO1
}, void.
```

Here is a slightly more elaborate example that also involves receiving input from the user. The following compiles successfully:

```prolog
main(IO) := let << {
    IO1, _ := print(IO, "Who are you?");
    IO2, Who := input(IO1);
    IO3, _ := print(IO2, "Hello, " + Who + "!");
    IO3
}, void.
```

## The `io` Class and the `let_io` Binder

As one can see in the above example, passing the `io` value by hand is not very convenient, and clutters the program. To impure operations easier to perform, Neutrino defines for each impure built-in function a struct of the same name, with one parameter less (omitting the `io` value). For example, the following compiles successfully:

```prolog
declare foo -> print.
foo := print("hello").
declare bar -> input.
bar := input.
```

These structs are automatically defined to be instances of the `io` class (not to be confused with the `io` _type_ discussed earlier). This class defines the `do` method which takes an `io` object and an instance (the struct representing the operation), and returns an `io` value and the return value from the operation.

The following is the I/O example from before, this time using the `do` method instead of directly calling `print` and `input`. It compiles successfully:

```prolog
main(IO) := let << {
    IO1, _ := do(IO, print("Who are you?"));
    IO2, Who := do(IO1, input);
    IO3, _ := do(IO2, print("Hello, " + Who + "!"));
    IO3
}, void.
```

Obviously, using the `do` notation did not make the code less awkward. However, by isolating `io` as a separate input and output parameter we allow for the `io` value to be threaded outside our code. This is what the `let_io` function does.

The following compiles successfully:

```prolog
main(IO) := (let_io << {
    _ := print("Who are you?");
    Who := input;
    _ := print("Hello, " + Who + "!");
    return(void)
})!IO.
```

### Impure Functions

The `let_io` binder allows us to define impure functions, and use them from within one another. For example, let us consider the `prompt` function, which prints a prompt and then reads input from the user. The following compiles successfully:

```prolog
prompt(Prompt) := let_io << {
    _ := print(Prompt);
    Ret := input;
    return(Ret)
}.

main(IO) := (let_io << {
    Who := prompt("Who are you?");
    _ := print("Hello, " + Who);
    return(void)
})!IO.
```
