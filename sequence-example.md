# Comprehensive Sequence Example

This example extends [a previous one](type-classes.md#sequence-example), but makes it more complete.

The following compiles successfully:

```prolog
% A generic sequence.
T : any =>
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
T : any =>
instance list(T) : seq(T) where {
    empty(L_) := case L_ of & {
        [] => true;
        [_ | _] => false
    };
    next(L) := case L of {
        [] => none;
        [Head | Tail] => just((Head, Tail))
    }
}.

% Fetch the nth element of a sequence. It consumes the sequence in the process.
T : any, S : seq(T) =>
declare nth(S, int64) -> maybe(T).

nth(Seq, Index) := case (Index == 0) of {
    true => case next(Seq) of {
        just((First, _)) => just(First);
        none => none
    };
    false => case next(Seq) of {
        just((_, Next)) => nth(Next, Index-1);
        none => none
    }
}.

% Instead of providing a map function, we provide a map struct, which is an instance of
% a sequence.
struct map(S, F) = map(S, F).

T1 : any, S : seq(T1), T2 : any, F : (T1 @> T2), F : delete =>
instance map(S, F) : seq(T2) where {
    % The map is empty if the underlying sequence is empty.
    empty(&map(Seq_, Fn_)) := empty(Seq_);
    % Applying the lambda (by reference) to the head, and providing a map on the tail.
    next(map(Seq, Fn)) := case next(Seq) of {
        just((Head, Tail)) => just((&Fn@Head, map(Tail, Fn)));
        none => none del Fn % Deleting the lambda at the end of the sequence.
    }
}.

% Todo: assert nth(map([1, 2, 3], (N @> N + 2)), 1) == just("foo").
assert nth(map([1, 2, 3], (N @> N + 2)), 1) == just(4).

% Like map, filter is also a struct which is an instance of seq.
struct filter(S, F) = filter(S, F).

T : delete, S : seq(T), F : (&T @> bool), F : delete =>
instance filter(S, F) : seq(T) where {
    empty(&filter(Seq_, Fn_)) := empty(Seq_);
    next(filter(Seq, Fn)) := case next(Seq) of {
        just((Head, Tail)) => if(&Fn@(&Head),
                                just((Head, filter(Tail, Fn))),
                                next(filter(Tail, Fn) del Head));
        none => none del Fn
    }
}.

assert nth(filter([1, 2, 3], (N @> *N == 2)), 0) == just(2).

% The last function returns the last element of a sequence.
% It works in two steps. First, it retreives the first element of the sequence, and
% calls the function last_or on the rest of the sequence with the first element as
% default. last_or returns the last element of a non-empty sequence, or the give default
% for an empty sequence. 
T : delete, S : seq(T) =>
declare last(S) -> maybe(T).

T : delete, S : seq(T) =>
declare last_or(S, T) -> T.

last(Seq) := case next(Seq) of {
    just((Head, Tail)) => just(last_or(Tail, Head));
    none => none
}.

last_or(Seq, Default) := case next(Seq) of {
    just((Head, Tail)) => last_or(Tail, Head) del Default;
    none => Default
}.

assert last([1, 2, 3, 4, 5]) == just(5).
assert last([]) == none.
```
