# Comprehensive Sequence Example

This example should actually be part of Neutrino's standard library, once we build one...

It defines the `seq` type class representing a sequence of things. A sequence supports two operations:

1. An `empty` method that returns whether or not it is empty, and
2. A `next` method which takes one element off the sequence and returns it along with a sequence of the rest of the elements.

Sequences are a very powerful abstraction, which exists in many programming languages (see for example [iterators in Rust](https://doc.rust-lang.org/book/ch13-02-iterators.html)). They allow operations to be performed on a sequence, such as `map` or `filter`, while abstracting away the actual representation of the data. Sequences are lazy, allowing infinite sequences to exist and be processed, so long that we do not require the entire sequence.

The following compiles successfully:

```prolog
% A generic sequence.
class S : seq(T) where {
    % Is this sequence empty? Does not consume the sequence to check.
    empty(&S) -> bool;
    % Consumes the sequence, returning its first element and a sequence of all other
    % elements. If the sequence is empty, returns none.
    next(S) -> maybe((T, S))
}.

% An integer is a sequence counting up from its value.
instance int64 : seq(int64) where {
    empty(_) := false;
    next(N) := just((N, N+1))
}.

% A list is a sequence.
instance list(T) : seq(T) where {
    empty(L) := case L of & {
        [] => true;
        [_ | _] => false
    };
    next(L) := case L of {
        [] => none;
        [Head | Tail] => just((Head, Tail))
    }
}.

% Fetch the nth element of a sequence. It consumes the sequence in the process.
S : seq(T) =>
declare nth(S, int64) -> maybe(T).

nth(Seq, Index) := if(Index == 0,
    case next(Seq) of {
        just((First, _)) => just(First);
        none => none
    },
    case next(Seq) of {
        just((_, Next)) => nth(Next, Index-1);
        none => none
    }).

% Instead of providing a map function, we provide a map struct, which is an instance of
% a sequence.
struct map(S, F) = map(S, F).

S : seq(T1), F : (T1 @> T2), F : delete =>
instance map(S, F) : seq(T2) where {
    % The map is empty if the underlying sequence is empty.
    empty(&map(Seq, Fn)) := empty(Seq);
    % Applying the lambda (by reference) to the head, and providing a map on the tail.
    next(map(Seq, Fn)) := case next(Seq) of {
        just((Head, Tail)) => just((&Fn@Head, map(Tail, Fn)));
        none => none del Fn % Deleting the lambda at the end of the sequence.
    }
}.

assert [1, 2, 3] >> map(N @> N + 2) >> nth(1) == just(4).

% Like map, filter is also a struct which is an instance of seq.
struct filter(S, F) = filter(S, F).

S : seq(T), T : delete, F : (&T @> bool), F : delete =>
instance filter(S, F) : seq(T) where {
    empty(&filter(Seq, Fn)) := empty(Seq);
    next(filter(Seq, Fn)) := case next(Seq) of {
        just((Head, Tail)) => if(&Fn@(&Head),
                                  just((Head, filter(Tail, Fn))),
                                  next(filter(Tail, Fn) del Head));
        none => none del Fn
    }
}.

assert [1, 2, 3] >> filter(N @> *N == 2) >> nth(0) == just(2).

% The last function returns the last element of a sequence.
% It works in two steps. First, it retreives the first element of the sequence, and
% calls the function last_or on the rest of the sequence with the first element as
% default. last_or returns the last element of a non-empty sequence, or the give default
% for an empty sequence. 
S : seq(T), T : delete =>
declare last(S) -> maybe(T).

S : seq(T), T : delete =>
declare last_or(S, T) -> T.

last(Seq) := case next(Seq) of {
    just((Head, Tail)) => just(last_or(Tail, Head));
    none => none
}.

last_or(Seq, Default) := case next(Seq) of {
    just((Head, Tail)) => last_or(Tail, Head) del Default;
    none => Default
}.

assert [1, 2, 3, 4, 5] >> last == just(5).
assert [] >> last == none.
```
