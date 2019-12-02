:- op(1100, fx, declare).
:- op(1050, fx, assert).
:- op(1050, fx, union).
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
:- dynamic type/1.
:- dynamic union_type/2.
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

compileStatement((union Union = Options), VNs) :-
    Union =.. [Name | Args],
    validateVars(Args),
    assert(type(Union)),
    assertOptionSignatures(Options, Union),
    assert(union_type(Union, Options)),
    nameVars(VNs),
    verifyTypeVariables(Args),
    walk(Options, verifyVarIsType(Name)),
    validateOptions(Options).

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
formatError(type_expected(T), ["Expected type, found ", T, "."]).
formatError(var_expected(V), ["Expected variable, found ", V, "."]).
formatError(var_not_introduced(V, T), ["Variable ", V,
    " is not introduced in the definition of ", T, "."]).
formatError(bad_union_type(Type, Expr), 
    ["Expected union type expression. Found ", Expr,
     " of non-union type ", Type, "."]).
formatError(incomplete_case_expr(Options, Type), 
    ["Incomplete case expression for type ", Type,
     ". Missing cases: ", Options, "."]).
formatError(case_mismatch(TyOpName, OpName),
    ["Expected case ", TyOpName, ", found ", OpName, "."]).


writeln_list(S, [First | Rest]) :-
    write(S, First),
    writeln_list(S, Rest).

writeln_list(S, []) :-
    writeln(S, "").

type_signature(==, [T, T], bool, []).
type_signature(+, [T, T], T, []).

inferType(Term, Type) :-
    inferTypeSpecial(Term, Type) ->
        true
        ;
        callable(Term),
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

validateOptions(Options) :-
    Options = Opt1 + Opt2 ->
        validateOptions(Opt1),
        validateOptions(Opt2);
        validateOption(Options).

validateOption(Option) :-
    Option =.. [_ | Types],
    validateTypes(Types).

validateTypes([]).
validateTypes([Type | Types]) :-
    validateType(Type),
    validateTypes(Types).

validateType(T) :-
    type(T) ->
        true;
        throw(type_expected(T)).

type(int64).
type(string).
type(_::type(_)).

validateVars([]).
validateVars([Var | Vars]) :-
    validateVar(Var),
    validateVars(Vars).

validateVar(Var) :-
    var(Var) ->
        true;
        throw(var_expected(Var)).

walk(Term, Functor) :-
    call(Functor, Term),
    compound(Term) ->
        Term =.. [_ | Args],
        walkArgs(Args, Functor)
        ;
        true.

walkArgs([], _).
walkArgs([First | Rest], Functor) :-
    walk(First, Functor),
    walkArgs(Rest, Functor).

verifyVarIsType(TypeName, X) :-
    (compound(X),
    X = Name :: Kind ->
        (var(Kind) ->
            throw(var_not_introduced(Name, TypeName))
            ;
            true)
        ;
        true).

verifyTypeVariables([]).
verifyTypeVariables([Arg | Args]) :-
    verifyTypeVariable(Arg),
    verifyTypeVariables(Args).

verifyTypeVariable(Name::Kind) :-
    var(Kind) ->
        Kind = type(_)
        ;
        throw(double_use_of_var(Name)).

inferTypeSpecial(case Expr of {Options}, OutType) :-
    !inferType(Expr, InType),
    (union_type(InType, TypeOptions) ->
        true
        ;
        throw(bad_union_type(InType, Expr))),
    validateCaseOptions(Options, TypeOptions, InType).

assertOptionSignatures(Options, Type) :-
    callable(Options),
    Options = Op1 + Op2 ->
        assertOptionSignatures(Op1, Type),
        assertOptionSignatures(Op2, Type)
        ;
        Options =.. [Name | Args],
        assert(type_signature(Name, Args, Type, [])).

validateCaseOptions(Options, TypeOptions, Type) :-
    TypeOptions = TyOp1 + TyOp2 ->
        (Options = (Pattern1 => Value1; Op2) ->
            Pattern1 =.. [Op1Name | _],
            TyOp1 =.. [TyOp1Name | _],
            (Op1Name == TyOp1Name ->
                true
                ;
                throw(case_mismatch(TyOp1Name, Op1Name)))
            ;
            throw(incomplete_case_expr(TyOp2, Type)))
        ;
        true.