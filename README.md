# Neutrino

Important note: In the rare case that you are reading these lines with the intention of actually using Neutrino, you're probably doing it wrong. Use [Rust](https://www.rust-lang.org/) instead.

Neutrino is a toy language I've been wanting to implement for quite some time now. It is a purely-functional programming language with type classes, a linear type system and a partial evaluator built into the compiler. To understand why I chose this particular combination of features, we need to understand DSLs.

## Domain-Specific Languages

[A lot has been said](https://martinfowler.com/books/dsl.html) about Domain-Specific Languages (DSLs), so I'm not going to expand on what they are or what they're good for. But I am going to go a little into the different ways they are built in, and their trade-offs.

At the highest level, DSLs can be separated to [external and internal](https://subscription.packtpub.com/book/application_development/9781782166504/1/ch01lvl1sec09/internal-versus-external-dsls). External DSLs are implemented through a compiler or an interpreter, written in a different language, while internal DSLs are libraries in a host language, extending the language to have the constructs of the DSL.

In functional programming, internal DSLs (which are also called _embedded DSLs_) are very common. To my knowledge, they come in three "flavors":

1. Deep embeddings.
2. Shallow embeddings.
3. Macros.

[Deep and shallow embeddings](https://alessandrovermeulen.me/2013/07/13/the-difference-between-shallow-and-deep-embedding/) are common in strongly-typed functional programming languages such as Haskell and Scala. With deep embedding, the DSL is represented as a _recursive data structure_, and the function implementing the DSL pattern-matches over that data structure, performing what it needs to perform. With shallow embedding, the DSL consists of _functions_, which when combined together perform the required operation.

Macros are used mostly in dynamically-typed functional programming languages, such as the Lisps. These are functions that are invoked at compile-time to convert instances of the macro into "simpler" code in the same language, code that (typically) does not make use of that macro.

There are clear trade-offs between embeddings (deep and shallow) and macros. For the sake of this discussion, let us only refer to deep embeddings, as [the two are closely related](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/embedding.pdf).

Deep embeddings are elegant. They consist of a clear syntactic definition, often given as an [algebraic data-type (ADT)](https://en.wikipedia.org/wiki/Algebraic_data_type), and a clear semantic definition, often given as a function that matches over that data type. This makes them easy to reason upon -- test, debug and fix when needed. Because they make use of simple functional constructs, they are no different than any other code written in the same functional language. For example, compilation errors will point you to the right place in your code.

The disadvantage of deep embedding is in the fact that the matching process takes place at runtime. This means that in addition to actually doing what it needs to be doing, a function implementing deep embedding will need to constantly interpret the DSL code, at the cost of run-time performance.

Macros, on the other hand, are completely evaluated at compile-time. They simply do not exist at run-time. This way, they do not add anything to the cost in terms of performance. However, they are much harder to reason upon.

Macros are prevalent in dynamically-typed languages. This means that many errors that, in a statically-typed language, would have been caught at compile-time, linger to be found only at run-time. But then, the macros do not exist anymore. Error messages then relate to the generated code. Similarly, debugging is also much harder in this situation, as the constructs written by the programmer do not exist anymore.

## The Futamura Projections

In the 1970's, [Yoshihiko Futamura](https://fi.ftmr.info/) envisioned [three theoretical _projections_](http://blog.sigfpe.com/2009/05/three-projections-of-doctor-futamura.html) in which a new process he called _specialization_ (also known as _partial evaluation_)could be used to turn an interpreter for a language into a compiler. With his first projection, specializing an interpreter with a program as input will result in a compiled version of the program. Taking this one step further, his second projection states that if we specialize a specializer taking an interpreter as input, we get a compiler for the same language. Furthermore, his third projection states that if we specialize a specializer with a specializer as input, we get a big headache... but also a piece of software that can compile interpreters into compilers.

Futamura himself has never (to my knowledge) implemented such a specializer, but he his papers describe what such a specializer entails. They operate on software that takes two pieces of input, such as interpreters, which take both the source code to interpret and the input from the user. A specializer is then given the program (e.g., the interpreter in the first projection), and its partial input (the source code, in the first projection), and returns a _specialized_ version of the program, with all decisions that are based on the partial input removed. This program is, as result, very different than the original one. Instead of a generic interpreter we get a program that only takes the user's input, and does what the source-code tells it to do. In other words, it emits a compiled program.

To date, Futamura's first projection has been more-or-less implemented by two software projects that I know of. One is [the Grall VM](https://github.com/oracle/graal) with its [truffle](https://github.com/oracle/graal/tree/master/truffle) framework, which allows you to write an interpreter and it compiles your program to byte-code by running partial evaluation; and the [RPython](https://rpython.readthedocs.io/en/latest/) language which has been developed as part of the [PyPy](https://pypy.org/) project (PyPy is a Python interpreter written in RPython, which the RPython compiler specializes to create a Python compiler).

The key to a successful implementation of Futamura's projections is the choice of the language the specializer targets (which also needs to be the language the specializer is written in, for the second and third projections). On the one hand, higher-level languages, such as purely-functional or purely-logic languages are easier to specialize relative to low-level imperative languages. A higher-level language also makes implementing interpreters easier. However, even if we perform perfect specialization, the performance we can expect from an executable that was compiled using Futamura's projections will never exceed that of the language we use for specialization. Therefore, if our language is too high-level (e.g., requires a garbage collector and persistent data structures), the compiled executable will have the same restrictions.

## Neutrino's Mission

What I want to achieve with Neutrino is the ability to:

1. Define DSLs using deep embeddings.
2. Compile DSL programs to native code using partial evaluation.
3. Allow the native code to be as low-level as possible, by using linear types.

## Status

Neutrino is work in progress. Even when it will be code-complete, I don't expect it to be useful for anything other than language experimentation.

## Documentation

* [Simple Expressions](simple-expressions.md)
* [Simple Functions](simple-functions.md)
* [Union Types](unions.md)
* [Type Classes](type-classes.md)
