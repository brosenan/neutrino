:- op(1050, fx, assert).
:- op(1050, fx, type).
:- op(1050, fx, struct).
:- op(1050, fx, class).
:- op(1050, fx, instance).
:- op(1090, xfx, =>).
:- op(900, xfx, where).
:- op(700, fx, case).
:- op(600, xfx, of).
:- op(300, xfy, !).

:- dynamic type_signature/4.
:- discontiguous inferType/2.

run(SourceFile) :-
    catch(
        (
            open(SourceFile, read, S),
            read(S, Term),
            compile(Term)
        ),
        Exception,
        (
            formatError(Exception, ExceptionText),
            writeln_list(user_error, ExceptionText),
            halt(1)
        )).

compile(assert Expr) :-
    inferType(Expr, Type),
    matchType(Type, bool).

inferType(true, bool).
inferType(N, int64) :- integer(N).
inferType(N, float64) :- float(N).
inferType(S, string) :- string(S).

matchType(T1, T2) :-
    unify_with_occurs_check(T1, T2) ->
        true;
        throw(type_mismatch(T1, T2)).

formatError(type_mismatch(T1, T2), 
            ["Type mismatch. Expected: ", T2, ", inferred: ", T1, "."]).

writeln_list(S, [First | Rest]) :-
    write(S, First),
    writeln_list(S, Rest).

writeln_list(S, []) :-
    writeln(S, "").

type_signature(==, [T, T], bool, []).
type_signature(+, [T, T], T, []).

inferType(Term, Type) :-
    compound(Term),
    Term =.. [Name | Args],
    type_signature(Name, ArgTypes, Type, _),
    inferTypes(Args, Types),
    matchTypes(Types, ArgTypes).

inferTypes([], []).
inferTypes([Arg | Args], [Type | Types]) :-
    inferType(Arg, Type),
    inferTypes(Args, Types).

matchTypes([], []).
matchTypes([Type | Types], [ArgType | ArgTypes]) :-
    matchType(Type, ArgType),
    matchTypes(Types, ArgTypes).