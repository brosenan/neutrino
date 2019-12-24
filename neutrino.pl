:- op(1090, xfx, =>).
:- op(1070, fx, declare).
:- op(1050, fx, assert).
:- op(1050, fx, union).
:- op(1050, fx, struct).
:- op(1050, fx, class).
:- op(1050, fx, instance).
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
:- dynamic type_class/3.
:- dynamic class_instance/2.
:- dynamic fake_type/1.
:- dynamic is_struct/1.

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
                !compileStatement(Term, VNs)
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
    inferType(Expr, Type, Assumptions),
    matchType(Type, bool, Expr),
    checkAssumptions(Assumptions),
    assembly(Expr, Result, [], Asm),
    lineariryCheck(Asm, Result, [], _).

compileStatement((Func := Body), VNs) :-
    nameVars(VNs),
    compileFunctionDefinition((Func := Body), _).

compileStatement((declare Decl), VNs) :-
    compileStatement((declare Decl), VNs, []).

compileStatement((union Union = Options), VNs) :-
    Union =.. [Name | Args],
    validateVars(Args),
    assert(type(Union)),
    assertOptionSignatures(Options, Union),
    sumToList(Options, OptionList),
    assert(union_type(Union, OptionList)),
    nameVars(VNs),
    verifyTypeVariables(Args),
    walk(Options, verifyVarIsType(Name), [], _),
    validateOptions(Options).

compileStatement((struct Type = Constructor), VNs) :-
    Constructor =.. [ConsName | ConsArgs],
    assert(type_signature(ConsName, ConsArgs, Type, [])),
    length(ConsArgs, ConsArity),
    assert(is_struct(ConsName/ConsArity)),
    assert(is_constructor(ConsName, ConsArity)),
    Type =.. [Name | Args],
    validateVars(Args),
    nameVars(VNs),
    verifyTypeVariables(Args),
    walk(Constructor, verifyVarIsType(Name), [], _).

compileStatement((class T:C where {Decls}), _VNs) :-
    declareClassFunctions(Decls, [T:C]),
    assert(type_class(C, T, Decls)),
    (var(T) ->
        true
        ;
        throw(instance_type_not_var_in_class_decl(T, C))),
    validateClassDecls(Decls, C, T).

compileStatement((instance T:C where {Defs}), VNs) :-
    (type_class(C, T, Decls) ->
        true
        ;
        throw(type_class_does_not_exist(C))),
    assert(class_instance(T, C)),
    !nameVars(VNs),
    !validateInstance(Decls, Defs, T, C),
    compileMethods(Defs, [T:C]).

compileStatement((Context => Statement), VNs) :-
    tupleToList(Context, ContextAsList),
    compileStatement(Statement, VNs, ContextAsList).

compileStatement((declare Func -> Type), _VNs, Context) :-
    Func =.. [Name | ArgTypes],
    assert(type_signature(Name, ArgTypes, Type, Context)).

compileFunctionDefinition((Func := Body), TypeContext) :-
    walk(Func, replaceSingletosWithDelete, [], _),
    Func =.. [Name | Args],
    inferType(Body, Type, BodyAssumptions),
    inferTypes(Args, ArgTypes, ArgAssumptions),
    validateLValues(Args, ArgTypes),
    sameLength(ArgTypes, SigArgTypes),
    (type_signature(Name, SigArgTypes, SigType, TypeContext) ->
        !saturateTypes(TypeContext),
        matchTypes(ArgTypes, SigArgTypes, Args),
        matchType(Type, SigType, Body),
        checkAssumptions(BodyAssumptions),
        checkAssumptions(ArgAssumptions)
        ;
        validateArgTypes(Args),
        assert(type_signature(Name, ArgTypes, Type, []))),
    !destructAssemblies(Args, DestArgs, [], DestAsm),
    !assembly(Body, Result, DestAsm, Asm),
    !stateMapList(introduceVars, DestArgs, [], VarState1),
    !lineariryCheck(Asm, Result, VarState1, _).


inferType(_::T, T, []).
inferType(N, int64, []) :- integer(N).
inferType(N, float64, []) :- float(N).
inferType(S, string, []) :- string(S).

inferType(Term, Type, Assumptions) :-
    inferTypeSpecial(Term, Type, Assumptions) ->
        true
        ;
        !my_callable(Term),
        Term =.. [Name | Args],
        sameLength(Args, ArgTypes),
        (type_signature(Name, ArgTypes, Type, Context) ->
            true
            ;
            functor(Term, Name, Arity),
            throw(undefined_expression(Name/Arity))),
        !inferTypes(Args, Types, ArgAssumptions),
        !append(Context, ArgAssumptions, Assumptions),
        !matchTypes(Types, ArgTypes, Args).

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
formatError(too_few_methods(T, C),
    ["Instance ", T, " of class ", C,
     " defines less methods than required by the class."]).
formatError(method_mismatch(DeclMethod, DefMethod, T, C),
    ["Expected ", DeclMethod, " in instance ", T, " of class ", C,
     ". Found ", DefMethod, "."]).
formatError(too_many_methods(T, C),
    ["Instance ", T, " of class ", C,
     " defines more methods than required by the class."]).
formatError(instance_type_not_var_in_class_decl(T, C), 
    ["Expected instance type to be a free variable in the declaration of class ", C,
    ". Found: ", T, "."]).
formatError(method_does_not_depend_on_instance_type(Name, C),
    ["Method ", Name,
     " does not depend on the instance type in the declaration of class ", C, "."]).
formatError(type_not_instance(T, C),
    ["Type ", T, " is not an instance of class ", C, "."]).
formatError(type_class_does_not_exist(C),
    ["Type class ", C, " does not exist."]).
formatError(undefined_expression(Functor),
    ["Undefined expression ", Functor, "."]).


writeln_list(S, [First | Rest]) :-
    write(S, First),
    writeln_list(S, Rest).

writeln_list(S, []) :-
    writeln(S, "").

type_signature(==, [T, T], bool, []).
type_signature(+, [T, T], T, []).
type_signature(-, [T, T], T, []).
type_signature(strlen, [&string], int64, []).

inferTypes([], [], []).
inferTypes([Arg | Args], [Type | Types], Assumptions) :-
    inferType(Arg, Type, Assumptions1),
    inferTypes(Args, Types, Assumptions2),
    append(Assumptions1, Assumptions2, Assumptions).

matchTypes([], [], _).
matchTypes([Type | Types], [ArgType | ArgTypes], [Expr | Exprs]) :-
    matchType(Type, ArgType, Expr),
    matchTypes(Types, ArgTypes, Exprs).

nameVars([]).
nameVars([Name=Name::_ | Rest]) :-
    nameVars(Rest).

validateArgTypes(Args) :-
    walk(Args, validateVarType, [], _).

validateVarType(Name::Type, State, State) :-
    \+ground(Type) ->
        throw(cannot_infer_type(Name))
        ;
        true.

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
basicType(&_).

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

inferTypeSpecial(case Expr of {Branches}, OutType, Assumptions) :-
    inferCaseExprType(Expr, Branches, OutType, Assumptions, (=)).

inferTypeSpecial(case Expr of & {Branches}, OutType, Assumptions) :-
    inferCaseExprType(Expr, Branches, OutType, Assumptions, makeRef).

inferTypeSpecial('_', _, []).

inferTypeSpecial(_::Type, Type, []).

inferCaseExprType(Expr, Branches, OutType, Assumptions, Modifier) :-
    !inferType(Expr, InType, ExprAssumptions),
    !validateCaseOptions(
        Branches, InType, OutType, CaseAssumptions, 0, OptionCount, Modifier),
    !union_type(InType, Options),
    (length(Options, OptionCount) ->
        true
        ;
        !nth0(OptionCount, Options, Missing),
        throw(incomplete_case_expr(Missing, InType))),
    append(ExprAssumptions, CaseAssumptions, Assumptions).

makeRef(T, RefT) :-
    nonvar(T), basicType(T) ->
        RefT = T
        ;
        RefT = &T.

union_type(&Type, Options) :-
    union_type(Type, Options).

assertOptionSignatures(Options, Type) :-
    my_callable(Options),
    Options = Op1 + Op2 ->
        assertOptionSignatures(Op1, Type),
        assertOptionSignatures(Op2, Type)
        ;
        Options =.. [Name | Args],
        assert(type_signature(Name, Args, Type, [])),
        length(Args, Arity),
        assert(is_constructor(Name, Arity)).

validateCaseOptions((Branch; Branches),
        Type, OutType, Assumptions, FirstIndex, LastIndex, Modifier) :-
    !validateCaseOption(Branch, Type, OutType, Assumptions1, FirstIndex, Modifier),
    NextIndex is FirstIndex + 1,
    !validateCaseOptions(
        Branches, Type, OutType, Assumptions2, NextIndex, LastIndex, Modifier),
    append(Assumptions1, Assumptions2, Assumptions).

validateCaseOptions((Pattern => Expr), 
        Type, OutType, Assumptions, Index, LastIndex, Modifier) :-
    !validateCaseOption((Pattern => Expr), Type, OutType, Assumptions, Index, Modifier),
    LastIndex is Index + 1.

validateCaseOption((Pattern => Value), InType, OutType, Assumptions, Index, Modifier) :-
    walk(Pattern, replaceSingletosWithDelete, [], _),
    Pattern =.. [PatternName | PatternArgs],
    !inferTypes(PatternArgs, PatternInferredTypes, _),
    sameLength(PatternArgs, PatternTypes),
    !type_signature(PatternName, PatternTypes, InferredType, _),
    !modifyAll(PatternTypes, Modifier, PatternModTypes),
    !call(Modifier, InferredType, ModType),
    !matchTypes(PatternModTypes, PatternInferredTypes, PatternArgs),
    !matchType(ModType, InType, Pattern),
    (union_type(InferredType, Options) ->
        true
        ;
        throw(bad_union_type(InType, Pattern))),
    !nth0(Index, Options, Option),
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
    inferType(Value, ValueType, Assumptions),
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
lValue('_').
lValue(Cons) :-
    my_callable(Cons),
    Cons =.. [Name | Args],
    length(Args, Arity),
    is_struct(Name/Arity).
    % length(Types, Arity),
    % !type_signature(Name, Types, Type, _),
    % !validateLValues(Args, Types).

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

% Free variables are ignored.
test(free_vars) :-
    stateMap(fake_transition, _, [x=a], L),
    L == [x=a].

:- end_tests(stateMap).

stateMap(Pred, Key, [], StateOut) :-
    var(Key) ->
        StateOut = []
        ;
        call(Pred, Key, default, Default),
        StateOut = [Key=Default].

stateMap(Pred, Key, [Key1=StateIn | RestIn], [Key1=StateOut | RestOut]) :-
    var(Key) ->
        StateOut = StateIn,
        RestOut = RestIn
        ;
        (Key = Key1 ->
            (call(Pred, Key, StateIn, StateOut) ->
                true
                ;
                StateIn = StateOut),
            RestIn = RestOut
            ;
            StateIn = StateOut,
            stateMap(Pred, Key, RestIn, RestOut)).

stateMapList(_, [], State, State).
stateMapList(Pred, [Key | Keys], StateIn, StateOut) :-
    stateMap(Pred, Key, StateIn, StateMid),
    stateMapList(Pred, Keys, StateMid, StateOut).

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
        Var = '_'
        ;
        Var = _ :: _.

:- begin_tests(assembly).

% Numbers and strings are assembled using the constant command.
test(assemble_number) :-
    assembly(42, Val, [], Assembly),
    Assembly == [constant(42, Val)].

test(string) :-
    assembly("hello", Val, [], Assembly),
    Assembly == [constant("hello", Val)].

% Variables and variable references are returned as their own value.
test(var) :-
    assembly('V'::some_type, Val, [], Assembly),
    Val == 'V'::some_type,
    Assembly == [].

test(var_ref) :-
    assembly(&'V'::some_type, Val, [], Assembly),
    Val == &'V'::some_type,
    Assembly == [].

% Functions use the call command.
test(simple_func) :-
    assembly(1+2, Val, [], Assembly),
    (Val, Assembly) =@= (Val, [constant(2, Two),
                               constant(1, One),
                               call(+, [One, Two], Val)]).

% Nested functions are assembled bottom-up.
test(nested_func) :-
    assembly(1+2+3, Val, [], Assembly),
    (Val, Assembly) =@= (Val, [constant(3, Three),
                               constant(2, Two), 
                               constant(1, One),
                               call(+, [One, Two], X),
                               call(+, [X, Three], Val)]).

% Case expressions are assembled by first assembling the expression being matched,
% then for each branch assembling a sequence that first destructs the pattern and
% then builds the expression it maps to.
test(case) :-
    compileStatement((union foobar1 = foo1(int64) + bar1(float64)), []),
    assembly((case foo1(42) of {
        foo1('A'::int64) => 'A'::int64 == 1;
        bar1('_') => false
    }), Val, [], Asm),
    (Val, Asm) =@= (Val, [constant(42, FourtyTwo),
                          construct(foo1, [FourtyTwo], X),
                          case(X, 
                              [[destruct(X, foo1, ['A'::int64]),
                              constant(1, One),
                              call(==, ['A'::int64, One], Val)],
                              [destruct(X, bar1, [Y]),
                              delete(Y),
                              construct(false, [], Val)]])]).

:- end_tests(assembly).

assemblySpecial(Expr, Val, Asm, [constant(Expr, Val) | Asm]) :-
    isSimpleExpr(Expr).

assemblySpecial(Name::Type, Name::Type, Asm, Asm).
assemblySpecial(&Name::Type, &Name::Type, Asm, Asm).

assemblySpecial((case Expr of {Branches}), Val, AsmIn, AsmOut) :-
    branchesAssembly(ExprVal, Branches, Val, BranchesAsm),
    assembly(Expr, ExprVal, [case(ExprVal, BranchesAsm) | AsmIn], AsmOut).

assemblySpecial((case Expr of & {Branches}), Val, AsmIn, AsmOut) :-
    assemblySpecial((case Expr of {Branches}), Val, AsmIn, AsmOut).

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

branchesAssembly(Expr, Branches, Val, BranchesAsm) :-
    Branches = (FirstBranch; RestBranches) ->
        branchesAssembly(Expr, FirstBranch, Val, FirstAsm),
        branchesAssembly(Expr, RestBranches, Val, RestAsm),
        append(FirstAsm, RestAsm, BranchesAsm)
        ;
        !(Branches = (Pattern => Result)),
        Pattern =.. [Name | Args],
        assembly(Result, Val, [], ResultAsm),
        destructAssemblies(Args, DestArgs, ResultAsm, DestAsm),
        BranchesAsm = [[destruct(Expr, Name, DestArgs) | DestAsm]].

destructAssemblies([], [], Asm, Asm).
destructAssemblies([Var::Type | Args], [Var::Type | DestArgs], AsmIn, AsmOut) :-
    destructAssemblies(Args, DestArgs, AsmIn, AsmOut).
destructAssemblies(['_' | Args], [X | DestArgs], AsmIn, AsmOut) :-
    destructAssemblies(Args, DestArgs, [delete(X) | AsmIn], AsmOut).
destructAssemblies([Cons | MoreArgs], [X | MoreDestArgs], AsmIn, AsmOut) :-
    my_callable(Cons),
    functor(Cons, Name, Arity),
    is_struct(Name/Arity),
    Cons =.. [_ | Args],
    destructAssemblies(Args, DestArgs, AsmIn, AsmMid),
    destructAssemblies(MoreArgs, MoreDestArgs, 
        [destruct(X, Name, DestArgs) | AsmMid], AsmOut).

lineariryCheck([], Result, StateIn, StateOut) :-
    !stateMap(consumeVars, Result, StateIn, State1),
    transitionAll(removeVars, State1, StateOut).

lineariryCheck([destruct(In, _, Outs) | Asm], Result, StateIn, StateOut) :-
    stateMap(consumeVars, In, StateIn, State1),
    stateMapList(introduceVars, Outs, State1, State2),
    !lineariryCheck(Asm, Result, State2, StateOut).

lineariryCheck([construct(_, Ins, Out) | Asm], Result, StateIn, StateOut) :-
    stateMapList(consumeVars, Ins, StateIn, State1),
    stateMap(introduceVars, Out, State1, State2),
    !lineariryCheck(Asm, Result, State2, StateOut).

lineariryCheck([call(_, Ins, Out) | Asm], Result, StateIn, StateOut) :-
    stateMapList(consumeVars, Ins, StateIn, State1),
    stateMap(introduceVars, Out, State1, State2),
    !lineariryCheck(Asm, Result, State2, StateOut).

lineariryCheck([constant(_, Out) | Asm], Result, StateIn, StateOut) :-
    stateMap(introduceVars, Out, StateIn, State1),
    !lineariryCheck(Asm, Result, State1, StateOut).

lineariryCheck([delete(X) | Asm], Result, StateIn, StateOut) :-
    stateMap(consumeVars, X, StateIn, State1),
    !lineariryCheck(Asm, Result, State1, StateOut).

lineariryCheck([case(Expr, [Branch | Branches]) | Asm], Result, StateIn, StateOut) :-
    append(Branch, Asm, TotalAsm),
    !lineariryCheck(TotalAsm, Result, StateIn, StateOut),
    lineariryCheck([case(Expr, Branches) | Asm], Result, StateIn, _).

lineariryCheck([case(_, []) | _], _, State, State).

declareClassFunctions((Func -> Type), TypeContext) :-
    Func =.. [Name | Args],
    assert(type_signature(Name, Args, Type, TypeContext)).

declareClassFunctions((Decl; Decls), TypeContext) :-
    declareClassFunctions(Decl, TypeContext),
    declareClassFunctions(Decls, TypeContext).

validateInstance((FirstDecl; RestDecl), Defs, T, C) :-
    Defs = (FirstDef; RestDef) ->
        !validateInstance(FirstDecl, FirstDef, T, C),
        !validateInstance(RestDecl, RestDef, T, C)
        ;
        throw(too_few_methods(T, C)).

validateInstance((Func -> _Type), Defs, T, C) :-
    Defs = (Head := _Body) ->
        Func =.. [DeclName | DeclArgs],
        Head =.. [DefName | DefArgs],
        length(DeclArgs, DeclArity),
        length(DefArgs, DefArity),
        (DeclName/DeclArity == DefName/DeclArity ->
            true
            ;
            throw(method_mismatch(DeclName/DeclArity, DefName/DefArity, T, C)))
        ;
        throw(too_many_methods(T, C)).

validateClassDecls((Decl1; Decl2), C, T) :-
    validateClassDecls(Decl1, C, T),
    validateClassDecls(Decl2, C, T).

validateClassDecls((Func -> _), C, T) :-
    term_variables(Func, Vars),
    \+((member(Var, Vars), Var == T)) ->
        functor(Func, Name, _),
        throw(method_does_not_depend_on_instance_type(Name, C))
        ;
        true.

compileMethods((Method1; Method2), TypeContext) :-
    compileMethods(Method1, TypeContext),
    compileMethods(Method2, TypeContext).

compileMethods((Func := Expr), TypeContext) :-
    compileFunctionDefinition((Func := Expr), TypeContext).

tupleToList(Tuple, List) :-
    Tuple = (A,B) ->
        List = [A | BList],
        tupleToList(B, BList)
        ;
        List = [Tuple].

saturateTypes([]).
saturateTypes([T:C | Rest]) :-
    term_variables(T:C, Vars),
    assignFakeTypes(Vars),
    (fake_type(T) ->
        assert(class_instance(T, C))
        ;
        true),
    saturateTypes(Rest).

assignFakeTypes([]).
assignFakeTypes([V | Vars]) :-
    gensym(unknown_type, V),
    assert(fake_type(V)),
    assignFakeTypes(Vars).

checkAssumptions([]).
checkAssumptions([T:C | Rest]) :-
    class_instance(T, C) ->
        checkAssumptions(Rest)
        ;
        throw(type_not_instance(T, C)).

sumToList(Sum, List) :-
    Sum = A+B ->
        sumToList(A, L1),
        sumToList(B, L2),
        append(L1, L2, List)
        ;
        List = [Sum].

my_callable(X) :- callable(X).
my_callable([]).

modifyAll([], _, []).
modifyAll([A | As], Modifier, [B | Bs]) :-
    call(Modifier, A, B),
    modifyAll(As, Modifier, Bs).

% Prelude
:- compileStatement((union bool = true + false), []).
:- compileStatement((union list(T) = [] + [T | list(T)]), ['T'=T]).
:- compileStatement((union maybe(T) = just(T) + none), ['T'=T]).
:- compileStatement((struct (A, B) = (A, B)), ['A'=A, 'B'=B]).

