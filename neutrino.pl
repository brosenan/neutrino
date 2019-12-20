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
:- op(200, fx, &).
:- op(100, fx, !).
:- op(100, xfx, ::).

:- dynamic type_signature/4.
:- dynamic type/1.
:- dynamic union_type/2.
:- dynamic is_constructor/2.
:- discontiguous inferType/2.

!Goal :- Goal, !.
!Goal :- throw(unsatisfied(Goal)).

run(SourceFile) :-
    open(SourceFile, read, S),
    read_term(S, Term, [variable_names(VNs)]),
    compileAll(SourceFile, S, Term, VNs).

compileAll(SourceFile, S, Term, VNs) :-
    Term == end_of_file ->
        true
        ;
        catch(
            (
                compileStatement(Term, VNs)
            ),
            Exception,
            (
                line_count(S, Line),
                (formatError(Exception, ExceptionText) ->
                    true
                    ; 
                    ExceptionText = [Exception]),
                writeln_list(user_error, [SourceFile, ":", Line, ": " | ExceptionText]),
                halt(1)
            )),
        read_term(S, NextTerm, [variable_names(NextVNs)]),
        compileAll(SourceFile, S, NextTerm, NextVNs).

compileStatement((assert Expr), VNs) :-
    nameVars(VNs),
    inferType(Expr, Type),
    matchType(Type, bool, Expr).

compileStatement((Func := Body), VNs) :-
    nameVars(VNs),
    walk(Func, replaceSingletosWithDelete, [], _),
    Func =.. [Name | Args],
    inferType(Body, Type),
    inferTypes(Args, ArgTypes),
    validateLValues(Args, ArgTypes),
    sameLength(ArgTypes, SigArgTypes),
    (type_signature(Name, SigArgTypes, SigType, []) ->
        matchTypes(ArgTypes, SigArgTypes, Args),
        matchType(Type, SigType, Body)
        ;
        validateArgTypes(Args),
        assert(type_signature(Name, ArgTypes, Type, []))),
    walk(Func, stateMap(introduceVars), [], VarStateBeforeBody),
    walk(Body, stateMap(consumeVars), VarStateBeforeBody, VarStateAfterBody),
    transitionAll(removeVars, VarStateAfterBody, _).

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
    walk(Options, verifyVarIsType(Name), [], _),
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
formatError(case_arity_mismatch(OpName, PatternArity, TyOpArity),
    ["Expected ", OpName, " to have an arity of ", TyOpArity,
     ". Found arity of ", PatternArity, "."]).
formatError(not_lvalue(LValue, Type),
    ["Expected l-value of type ", Type, ". Found ", LValue, "."]).
formatError(var_already_introduced(Var),
    ["Variable ", Var, " has already been introduced in this context."]).
formatError(var_not_introduced(Var),
    ["Variable ", Var, " has not been introduced in this context."]).
formatError(var_used_more_than_once(Var, Type),
    ["Variable ", Var, " of non-basic type ", Type, " is used more than once."]).
formatError(var_not_used(Var, Type),
    ["Variable ", Var, " of non-basic type ", Type, " is not used in this context."]).

writeln_list(S, [First | Rest]) :-
    write(S, First),
    writeln_list(S, Rest).

writeln_list(S, []) :-
    writeln(S, "").

type_signature(==, [T, T], bool, []).
type_signature(+, [T, T], T, []).
type_signature(true, [], bool, []).
type_signature(false, [], bool, []).

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

type(Type) :- basicType(Type).
type(string).
type(_::type(_)).

basicType(int64).
basicType(float64).

validateVars([]).
validateVars([Var | Vars]) :-
    validateVar(Var),
    validateVars(Vars).

validateVar(Var) :-
    var(Var) ->
        true;
        throw(var_expected(Var)).

verifyVarIsType(TypeName, X, _, _) :-
    compound(X),
    X = Name :: Kind,
    (var(Kind) ->
        throw(var_not_introduced(Name, TypeName))
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
    validateCaseOptions(Options, TypeOptions, InType, OutType).

inferTypeSpecial(delete, _).

assertOptionSignatures(Options, Type) :-
    callable(Options),
    Options = Op1 + Op2 ->
        assertOptionSignatures(Op1, Type),
        assertOptionSignatures(Op2, Type)
        ;
        Options =.. [Name | Args],
        assert(type_signature(Name, Args, Type, [])),
        length(Args, Arity),
        assert(is_constructor(Name, Arity)).

validateCaseOptions(Options, TypeOptions, Type, OutType) :-
    TypeOptions = TyOp1 + TyOp2 ->
        (Options = (Pattern1 => Value1; Op2) ->
            validateCaseOption(Pattern1, Value1, TyOp1, OutType),
            validateCaseOptions(Op2, TyOp2, Type, OutType)
            ;
            throw(incomplete_case_expr(TyOp2, Type)))
        ;
        !(Options = (Pattern => Value)),
        validateCaseOption(Pattern, Value, TypeOptions, OutType).

validateCaseOption(Pattern, Value, Option, OutType) :-
    Pattern =.. [PatternName | PatternArgs],
    Option =.. [OptionName | OptionArgs],
    (OptionName == PatternName ->
        true
        ;
        throw(case_mismatch(OptionName, PatternName))),
    length(PatternArgs, PatternArity),
    length(OptionArgs, OptionArity),
    (PatternArity == OptionArity ->
        true
        ;
        throw(case_arity_mismatch(OptionName, PatternArity, OptionArity))),
    validateLValues(PatternArgs, OptionArgs),
    inferType(Pattern, _),
    inferType(Value, ValueType),
    matchType(ValueType, OutType, Value).

validateLValues([], []).
validateLValues([LValue | LValues], [Type | Types]) :-
    validateLValue(LValue, Type),
    validateLValues(LValues, Types).

validateLValue(LValue, Type) :-
    lValue(LValue) ->
        true
        ;
        throw(not_lvalue(LValue, Type)).

lValue(LValue) :-
    LValue = (_::_).
lValue(delete).

fake_simpleWalkPredicate(2, a, b).

:- begin_tests(walk).

test(walk_simple) :-
    walk(2, fake_simpleWalkPredicate, a, B),
    B == b.

test(walk_not_matching_anything) :-
    walk(4, fake_simpleWalkPredicate, a, A),
    A == a.

test(walk_compund) :-
    walk(foo(2, 3), fake_simpleWalkPredicate, a, B),
    B == b.

test(walk_compund_matching_second_arg) :-
    walk(foo(3, 2), fake_simpleWalkPredicate, a, B),
    B == b.

:- end_tests(walk).

walk(Term, Pred, Start, End) :-
    call(Pred, Term, Start, End) ->
        true
        ;
        (compound(Term) ->
            Term =.. [_ | Args],
            walkArgs(Args, Pred, Start, End)
        ;
        Start = End).

walkArgs([], _, State, State).
walkArgs([First | Rest], Pred, Start, End) :-
    walk(First, Pred, Start, Mid),
    walkArgs(Rest, Pred, Mid, End).

fake_transition(_, default, a).
fake_transition(Key, a, b(Key)).
fake_transition(_, b(_), c).

:- begin_tests(stateMap).

% In an empty state, the key is added with the default value.
test(new_key) :-
    stateMap(fake_transition, x, [], L),
    L == [x=a].

% If there is already a state for that key, it is transformed
% using the transition predicate
test(existing_key) :-
    stateMap(fake_transition, y, [x=a, y=a], L),
    L == [x=a, y=b(y)].

% If the transition predicate fails, do not transition.
test(no_transition) :-
    stateMap(fake_transition, x, [x=p, y=a], L),
    L == [x=p, y=a].

:- end_tests(stateMap).

stateMap(Pred, Key, [], [Key=Default]) :-
    call(Pred, Key, default, Default).

stateMap(Pred, Key, [Key1=StateIn | RestIn], [Key1=StateOut | RestOut]) :-
    (Key = Key1 ->
        (call(Pred, Key, StateIn, StateOut) ->
            true
            ;
            StateIn = StateOut),
        RestIn = RestOut
        ;
        StateIn = StateOut,
        stateMap(Pred, Key, RestIn, RestOut)).

:- begin_tests(transitionAll).

test(empty_state) :-
    transitionAll(fake_transition, [], L),
    L == [].

test(non_empty_state) :-
    transitionAll(fake_transition, [x=a, y=b(y), z=p], L),
    L == [x=b(x), y=c, z=p].

:- end_tests(transitionAll).

transitionAll(_, [], []).
transitionAll(Transition, [Key=ValueIn | RestIn], [Key=ValueOut | RestOut]) :-
    (call(Transition, Key, ValueIn, ValueOut) ->
        true
        ;
        ValueIn = ValueOut),
    transitionAll(Transition, RestIn, RestOut).

introduceVars(_::_, default, new).
introduceVars(Var::_, new, _) :-
    throw(var_already_introduced(Var)).

consumeVars(Var::_, default, _) :-
    throw(var_not_introduced(Var)).
consumeVars(_::Type, new, consumed) :-
    \+basicType(Type).
consumeVars(Var::Type, consumed, _) :-
    throw(var_used_more_than_once(Var, Type)).

removeVars(Var::Type, new, _) :-
    \+basicType(Type),
    throw(var_not_used(Var, Type)).

replaceSingletosWithDelete(Var, S, S) :-
    var(Var) ->
        Var = delete
        ;
        Var = _ :: _.

:- begin_tests(assembly).

test(assemble_number) :-
    assembly(42, Val, [], Assembly),
    Val == 42,
    Assembly == [].

test(string) :-
    assembly("hello", Val, [], Assembly),
    Val == "hello",
    Assembly == [].

test(var) :-
    assembly('V'::some_type, Val, [], Assembly),
    Val == 'V'::some_type,
    Assembly == [].

test(var_ref) :-
    assembly(&'V'::some_type, Val, [], Assembly),
    Val == &'V'::some_type,
    Assembly == [].

test(simple_func) :-
    assembly(1+2, Val, [], Assembly),
    Assembly == [call(+, [1, 2], Val)].

test(nested_func) :-
    assembly(1+2+3, Val, [], Assembly),
    (Val, Assembly) =@= (Val, [call(+, [1, 2], X),
                               call(+, [X, 3], Val)]).

test(case) :-
    compileStatement((union foobar1 = foo1(int64) + bar1(float64)), []),
    assembly((case foo1(42) of {
        foo1('A'::int64) => 'A'::int64 == 1;
        bar1('B'::float64) => 'B'::float64 == 1.0
    }), Val, [], Asm),
    (Val, Asm) =@= (Val, [construct(foo1, [42], X),
                          case(X, 
                              [[destruct(X, foo1, ['A'::int64]),
                              call(==, ['A'::int64, 1], Val)],
                              [destruct(X, bar1, ['B'::float64]),
                              call(==, ['B'::float64, 1.0], Val)]])]).

:- end_tests(assembly).

assemblySpecial(Expr, Expr, Asm, Asm) :-
    isSimpleExpr(Expr).

assemblySpecial((case Expr of {Branches}), Val, AsmIn, AsmOut) :-
    branchesAssembly(ExprVal, Branches, Val, BranchesAsm),
    assembly(Expr, ExprVal, [case(ExprVal, BranchesAsm) | AsmIn], AsmOut).

assembly(Expr, Val, AsmIn, AsmOut) :-
    assemblySpecial(Expr, Val, AsmIn, AsmOut) ->
        true
        ;
        Expr =.. [Name | Args],
        length(Args, Arity),
        (is_constructor(Name, Arity) ->
            assemblies(Args, Vals, [construct(Name, Vals, Val) | AsmIn], AsmOut)
            ;
            assemblies(Args, Vals, [call(Name, Vals, Val) | AsmIn], AsmOut)).

assemblies([], [], Asm, Asm).
assemblies([Expr | Exprs], [Val | Vals], AsmIn, AsmOut) :-
    assembly(Expr, Val, AsmIn, AsmMid),
    assemblies(Exprs, Vals, AsmMid, AsmOut).

isSimpleExpr(Expr) :- number(Expr).
isSimpleExpr(Expr) :- string(Expr).
isSimpleExpr(_::_).
isSimpleExpr(&_::_).

branchesAssembly(Expr, Branches, Val, BranchesAsm) :-
    Branches = (FirstBranch; RestBranches) ->
        BranchesAsm = [FirstAsm | RestAsm],
        branchesAssembly(Expr, FirstBranch, Val, [FirstAsm]),
        branchesAssembly(Expr, RestBranches, Val, RestAsm)
        ;
        !(Branches = (Pattern => Result)),
        Pattern =.. [Name | Args],
        BranchesAsm = [[destruct(Expr, Name, Args) | ResultAsm]],
        assembly(Result, Val, [], ResultAsm).
