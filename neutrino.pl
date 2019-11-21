:- op(1100, fx, declare).
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
:- op(100, fx, !).
:- op(100, xfx, ::).

:- dynamic type_signature/4.
:- discontiguous inferType/2.

!Goal :- Goal, !.
!Goal :- throw(unsatisfied(Goal)).

run(SourceFile) :-
    open(SourceFile, read, S),
    read_term(S, Term, [variable_names(VNs)]),
    compileAll(SourceFile, S, Term, VNs).

compileAll(SourceFile, S, Term, VNs) :-
    Term == end_of_file -> true;
    catch(
        (
            compileStatement(Term, VNs)
        ),
        Exception,
        (
            line_count(S, Line),
            (formatError(Exception, ExceptionText) -> true; 
                ExceptionText = [Exception]),
            writeln_list(user_error, [SourceFile, ":", Line, ": " | ExceptionText]),
            halt(1)
        )),
    read_term(S, NextTerm, [variable_names(NextVNs)]),
    compileAll(SourceFile, S, NextTerm, NextVNs).

compileStatement((assert Expr), _VNs) :-
    inferType(Expr, Type),
    matchType(Type, bool, Expr).

compileStatement((Func := Body), VNs) :-
    nameVars(VNs),
    Func =.. [Name | Args],
    inferType(Body, Type),
    inferTypes(Args, ArgTypes),
    sameLength(ArgTypes, SigArgTypes),
    (type_signature(Name, SigArgTypes, SigType, []) ->
        matchTypes(ArgTypes, SigArgTypes, Args),
        matchType(Type, SigType, Body)
        ;
        validateArgTypes(Args),
        assert(type_signature(Name, ArgTypes, Type, []))).

compileStatement((declare Func -> Type), _VNs) :-
    Func =.. [Name | ArgTypes],
    assert(type_signature(Name, ArgTypes, Type, [])).

inferType(_::T, T).
inferType(true, bool).
inferType(N, int64) :- integer(N).
inferType(N, float64) :- float(N).
inferType(S, string) :- string(S).

matchType(T1, T2, Expr) :-
    unify_with_occurs_check(T1, T2) ->
        true;
        throw(type_mismatch(T1, T2, Expr)).

formatError(type_mismatch(T1, T2, Expr), 
            ["Type mismatch. Expression ", Expr, " expected to be ",
             T2, ", inferred: ", T1, "."]).
formatError(cannot_infer_type(VarName),
    ["Cannot infer the type of argument ", VarName,
     ". Please provide an explicit declaration."]).

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
    !type_signature(Name, ArgTypes, Type, _),
    inferTypes(Args, Types),
    matchTypes(Types, ArgTypes, Args).

inferTypes([], []).
inferTypes([Arg | Args], [Type | Types]) :-
    inferType(Arg, Type),
    inferTypes(Args, Types).

matchTypes([], [], _).
matchTypes([Type | Types], [ArgType | ArgTypes], [Expr | Exprs]) :-
    matchType(Type, ArgType, Expr),
    matchTypes(Types, ArgTypes, Exprs).

nameVars([]).
nameVars([Name=Name::_ | Rest]) :-
    nameVars(Rest).

validateArgTypes([]).
validateArgTypes([Name::Type | Args]) :-
    (\+ground(Type) ->
        throw(cannot_infer_type(Name));
        true),
    validateArgTypes(Args).

sameLength(L1, L2) :-
    length(L1, Len),
    length(L2, Len).    