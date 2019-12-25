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
:- dynamic function_impl/5.

:- discontiguous constant_propagation/2.
:- discontiguous type_signature/4.

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
    once(assembly(Expr, Result, [], Asm)),
    linearityCheck(Asm, Result, [], _),
    unnameVars((Asm, Result), (UnAsm, UnResult)),
    specialize(UnAsm, _),
    \+(UnResult == true) ->
        throw(assertion_failed(UnResult))
        ;
        true.

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
    walk(Constructor, verifyVarIsType(Name), [], _),
    validateTypes(ConsArgs).

compileStatement((class T:C where {Decls}), _VNs) :-
    declareClassFunctions(Decls, [T:C]),
    assert(type_class(C, T, Decls)),
    (var(T) ->
        true
        ;
        throw(instance_type_not_var_in_class_decl(T, C))),
    validateClassDecls(Decls, C, T).

compileStatement((instance T:C where {Defs}), VNs) :-
    compileStatement((instance T:C where {Defs}), VNs, []).

compileStatement((Context => Statement), VNs) :-
    tupleToList(Context, ContextAsList),
    compileStatement(Statement, VNs, ContextAsList).

compileStatement((declare Func -> Type), _VNs, Context) :-
    Func =.. [Name | ArgTypes],
    assert(type_signature(Name, ArgTypes, Type, Context)).

compileStatement((instance T:C where {Defs}), VNs, Context) :-
    (type_class(C, T, Decls) ->
        true
        ;
        throw(type_class_does_not_exist(C))),
    assert(class_instance(T, C)),
    !nameVars(VNs),
    !validateInstance(Decls, Defs, T, C),
    saturateTypes(Context),
    compileMethods(Defs, [T:C]).

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
        TypeContext = [],
        assert(type_signature(Name, ArgTypes, Type, TypeContext))),
    !assembly(Body, Result, [], BodyAsm),
    !destructAssemblies(Args, DestArgs, BodyAsm, Asm),
    !stateMapList(introduceVars, DestArgs, [], VarState1),
    !linearityCheck(Asm, Result, VarState1, _),
    unnameVars((TypeContext, DestArgs, Asm, Result), 
               (UnTypeContext, UnArgs, UnAsm, UnResult)),
    assert(function_impl(Name, UnTypeContext, UnArgs, UnAsm, UnResult)).


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
formatError(lend_and_consume(Name),
    ["Attempt to both consume and lend variable ", Name, " in a single step."]).
formatError(assertion_failed(Result),
    ["Assertion has failed. Expected true, got ", Result, "."]).


writeln_list(S, [First | Rest]) :-
    write(S, First),
    writeln_list(S, Rest).

writeln_list(S, []) :-
    writeln(S, "").

type_signature(==, [T, T], bool, []).
constant_propagation(A==B, V) :- A == B -> V = true; V = false.

type_signature(int64_plus, [int64, int64], int64, []).
constant_propagation(int64_plus(A, B), V) :- V is A+B.

type_signature(float64_plus, [float64, float64], float64, []).
constant_propagation(float64_plus(A, B), V) :- V is A+B.

type_signature(int64_minus, [int64, int64], int64, []).
constant_propagation(int64_minus(A, B), V) :- V is A-B.

type_signature(float64_minus, [float64, float64], float64, []).
constant_propagation(float64_minus(A, B), V) :- V is A-B.

type_signature(strlen, [&string], int64, []).
constant_propagation(strlen(S), Len) :- string_length(S, Len).

type_signature(strcat, [string, string], string, []).
constant_propagation(strcat(S1, S2), S) :- string_concat(S1, S2, S).

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

inferTypeSpecial(&Expr, &Type, Assumptions) :-
    my_callable(Expr),
    functor(Expr, Name, Arity),
    is_struct(Name/Arity) ->
        length(ArgTypes, Arity),
        type_signature(Name, ArgTypes, Type, Assumptions1),
        modifyAll(ArgTypes, makeRef, ModArgTypes),
        Expr =.. [_ | Args],
        inferTypes(Args, InferredTypes, Assumptions2),
        append(Assumptions1, Assumptions2, Assumptions),
        matchTypes(InferredTypes, ModArgTypes, Args)
        ;
        Expr=_::_,
        inferType(Expr, Type, Assumptions).

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
    \+ \+ validateCasePattern(Pattern, Index),
    Pattern =.. [PatternName | PatternArgs],
    !inferTypes(PatternArgs, PatternInferredTypes, _),
    sameLength(PatternArgs, PatternTypes),
    !type_signature(PatternName, PatternTypes, InferredType, _),
    !modifyAll(PatternTypes, Modifier, PatternModTypes),
    !call(Modifier, InferredType, ModType),
    !matchTypes(PatternModTypes, PatternInferredTypes, PatternArgs),
    !matchType(ModType, InType, Pattern),
    inferType(Value, ValueType, Assumptions),
    matchType(ValueType, OutType, Value).

validateCasePattern(Pattern, Index) :-
    inferType(Pattern, Type, _),
    (union_type(Type, Options) ->
        true
        ;
        throw(bad_union_type(Type, Pattern))),
    !nth0(Index, Options, Option),
    Option =.. [OptionName | OptionArgs],
    Pattern =.. [PatternName | PatternArgs],
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
    validateLValues(PatternArgs, OptionArgs).


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
    is_struct(Name/Arity),
    length(Types, Arity),
    !type_signature(Name, Types, _, _),
    !validateLValues(Args, Types).

lValue(&Val) :-
    my_callable(Val),
    functor(Val, Name, Arity),
    is_struct(Name/Arity),
    lValue(Val).

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
consumeVars(Var::_, borrowed, _) :-
    throw(lend_and_consume(Var)).

borrowVars(_::_, new, borrowed).
borrowVars(Name::_, consumed, _) :-
    throw(lend_and_consume(Name)).

removeVars(Var::Type, new, _) :-
    \+basicType(Type),
    throw(var_not_used(Var, Type)).

replaceSingletosWithDelete(Var, S, S) :-
    var(Var) ->
        Var = '_'
        ;
        Var = _ :: _.

:- begin_tests(assembly).

% Numbers and strings are assembled using the literal command.
test(assemble_number) :-
    assembly(42, Val, [], Assembly),
    Assembly == [literal(42, Val)].

test(string) :-
    assembly("hello", Val, [], Assembly),
    Assembly == [literal("hello", Val)].

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
    once(assembly(1+2, Val::int64, [], Assembly)),
    (Val, Assembly) =@= (Val,
                        [literal(2, Two::int64),
                         literal(1, One::int64),
                         call(+, [One::int64, Two::int64], [int64:plus], Val::int64)]).

% Nested functions are assembled bottom-up.
test(nested_func) :-
    once(assembly(1+2+3, Val::int64, [], Assembly)),
    (Val, Assembly) =@= (Val,
                        [literal(3, Three::int64),
                         literal(2, Two::int64), 
                         literal(1, One::int64),
                         call(+, [One::int64, Two::int64], [int64:plus], X::int64),
                         call(+, [X::int64, Three::int64], [int64:plus], Val::int64)]).

% Case expressions are assembled by first assembling the expression being matched,
% then for each branch assembling a sequence that first destructs the pattern and
% then builds the expression it maps to.
test(case) :-
    compileStatement((union foobar1 = foo1(int64) + bar1(float64)), []),
    assembly((case foo1(42) of {
        foo1('A'::int64) => 'A'::int64 == 1;
        bar1('_') => false
    }), Val, [], Asm),
    (Val, Asm) =@= (V::bool,
                    [literal(42, FourtyTwo::int64),
                     construct(foo1, [FourtyTwo::int64], X::foobar1),
                     case(X::foobar1,
                        [[destruct(X::foobar1, foo1, ['A'::int64]),
                          literal(1, One::int64),
                          call(==, ['A'::int64, One::int64], [], V::bool)],
                         [destruct(X::foobar1, bar1, [Y]),
                          delete(Y),
                          construct(false, [], V::bool)]])]).


% Case expression over reference types differ from normal case expressions
% in that instead of using destruct they use destruct_ref (which extracts
% the arguments but does not destroy the object), and in that deletes are
% not placed.
test(case_ref) :-
    compileStatement((union foobar2 = foo2(string) + bar2(string)), []),
    assembly((case foo2("hello") of & {
        foo2('A'::(&string)) => 'A'::(&string) == 'A'::(&string);
        bar2('_') => false
    }), Val, [], Asm),
    (Val, Asm) =@= (V::bool,
                    [literal("hello", Hello::string),
                     construct(foo2, [Hello::string], X::foobar2),
                     case(X::foobar2,
                     [[destruct_ref(X::foobar2, foo2, ['A'::(&string)]),
                       call(==, ['A'::(&string), 'A'::(&string)], [], V::bool)],
                      [destruct_ref(X::foobar2, bar2, [_]),
                       construct(false, [], V::bool)]])]).
:- end_tests(assembly).

assemblySpecial(Expr, Val::Type, Asm, [literal(Expr, Val::Type) | Asm]) :-
    isSimpleExpr(Expr),
    inferType(Expr, Type, _).

assemblySpecial(Name::Type, Name::Type, Asm, Asm).
assemblySpecial(&Name::Type, &Name::Type, Asm, Asm).

assemblySpecial((case Expr of {Branches}), Val, AsmIn, AsmOut) :-
    branchesAssembly(ExprVal, Branches, Val, BranchesAsm),
    assembly(Expr, ExprVal, [case(ExprVal, BranchesAsm) | AsmIn], AsmOut).

assemblySpecial((case Expr of & {Branches}), Val, AsmIn, AsmOut) :-
    branchesAssembly(ExprVal, Branches, Val, BranchesAsm),
    transformBranchesForRef(BranchesAsm, RefBranchesAsm),
    assembly(Expr, ExprVal, [case(ExprVal, RefBranchesAsm) | AsmIn], AsmOut).

assembly(Expr, Val, AsmIn, AsmOut) :-
    assemblySpecial(Expr, Val, AsmIn, AsmOut) ->
        true
        ;
        Expr =.. [Name | Args],
        length(Args, Arity),
        length(ArgTypes, Arity),
        type_signature(Name, ArgTypes, Type, Assumptions),
        Val = _::Type,
        (is_constructor(Name, Arity) ->
            assemblies(Args, Vals, [construct(Name, Vals, Val) | AsmIn], AsmOut)
            ;
            assemblies(Args, Vals, [call(Name, Vals, Assumptions, Val) | AsmIn], AsmOut)),
        inferTypes(Vals, ArgTypes, _).

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

:- begin_tests(destructAssemblies).

test(struct) :-
    destructAssemblies([(_::int64, '_')], Args, [], Asm),
   (Args, Asm) =@= ([X], [destruct(X, ',', [_::int64, Y]), delete(Y)]).

:- end_tests(destructAssemblies).

destructAssemblies([], [], Asm, Asm).

destructAssemblies([Var::Type | Args], [Var::Type | DestArgs], AsmIn, AsmOut) :-
    destructAssemblies(Args, DestArgs, AsmIn, AsmOut).

destructAssemblies(['_' | Args], [X | DestArgs], AsmIn, AsmOut) :-
    destructAssemblies(Args, DestArgs, [delete(X) | AsmIn], AsmOut).

destructAssemblies([&Cons | MoreArgs], [X | MoreDestArgs], AsmIn, AsmOut) :-
    my_callable(Cons),
    functor(Cons, Name, Arity),
    is_struct(Name/Arity),
    Cons =.. [_ | Args],
    destructAssemblies(Args, DestArgs, [], ArgsAsm),
    transformBranchForRef(ArgsAsm, ArgsAsmForRef),
    destructAssemblies(MoreArgs, MoreDestArgs, [], MoreAsm),
    append([destruct_ref(X, Name, DestArgs) | ArgsAsmForRef], MoreAsm, AsmMid),
    append(AsmMid, AsmIn, AsmOut).

destructAssemblies([Cons | MoreArgs], [X | MoreDestArgs], AsmIn, AsmOut) :-
    my_callable(Cons),
    functor(Cons, Name, Arity),
    is_struct(Name/Arity),
    Cons =.. [_ | Args],
    destructAssemblies(Args, DestArgs, AsmIn, AsmMid),
    destructAssemblies(MoreArgs, MoreDestArgs, 
        [destruct(X, Name, DestArgs) | AsmMid], AsmOut).

linearityCheck([], Result, StateIn, StateOut) :-
    !consumeVar(Result, StateIn, State1),
    transitionAll(removeVars, State1, StateOut).

linearityCheck([First | Rest], Result, StateIn, StateOut) :-
    linearityCheckStep(First, Result, StateIn, State1),
    unlendVars(State1, State2),
    linearityCheck(Rest, Result, State2, StateOut).

linearityCheck([case(Expr, [Branch | Branches]) | Asm], Result, StateIn, StateOut) :-
    append(Branch, Asm, TotalAsm),
    !linearityCheck(TotalAsm, Result, StateIn, StateOut),
    linearityCheck([case(Expr, Branches) | Asm], Result, StateIn, _).

linearityCheck([case(_, []) | _], _, State, State).

linearityCheckStep(destruct(In, _, Outs), _, StateIn, StateOut) :-
    consumeVar(In, StateIn, State1),
    introduceVarList(Outs, State1, StateOut).

linearityCheckStep(destruct_ref(In, Name, Outs), Result, StateIn, StateOut) :-
    linearityCheckStep(destruct(In, Name, Outs), Result, StateIn, StateOut).

linearityCheckStep(construct(_, Ins, Out), _, StateIn, StateOut) :-
    consumeVarList(Ins, StateIn, State1),
    introduceVar(Out, State1, StateOut).

linearityCheckStep(call(_, Ins, _, Out), _, StateIn, StateOut) :-
    !consumeVarList(Ins, StateIn, State1),
    introduceVar(Out, State1, StateOut).

linearityCheckStep(literal(_, Out), _, StateIn, StateOut) :-
    introduceVar(Out, StateIn, StateOut).

linearityCheckStep(delete(X), _, StateIn, StateOut) :-
    consumeVar(X, StateIn, StateOut).

consumeVar(Var, StateIn, StateOut) :-
    \+ \+((Var = V::_, var(V))) ->
        StateOut = StateIn
        ;
        (Var = _::_ ->
            stateMap(consumeVars, Var, StateIn, StateOut)
            ;
            (Var = &Name::Type ->
                !stateMap(borrowVars, Name::Type, StateIn, StateOut)
                ;
                throw(unsupported_variable(Var)))).

consumeVarList([], State, State).
consumeVarList([Var | Vars], StateIn, StateOut) :-
    consumeVar(Var, StateIn, StateMid),
    consumeVarList(Vars, StateMid, StateOut).

introduceVar(Var, StateIn, StateOut) :-
    \+ \+((Var = V::_, var(V))) ->
        StateOut = StateIn
        ;
        stateMap(introduceVars, Var, StateIn, StateOut).

introduceVarList([], State, State).
introduceVarList([Var | Vars], StateIn, StateOut) :-
    introduceVar(Var, StateIn, StateMid),
    introduceVarList(Vars, StateMid, StateOut).

unlendVars([], []).
unlendVars([Key=StateIn | StatesIn], [Key=StateOut | StatesOut]) :-
    (StateIn = borrowed ->
        StateOut = new
        ;
        StateOut = StateIn),
    unlendVars(StatesIn, StatesOut).

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

fake_type(Name::_) :-
    atom(Name).

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

transformBranchesForRef([], []).
transformBranchesForRef([BranchAsm | BranchesAsm], [RefBranchAsm | RefBranchesAsm]) :-
    transformBranchForRef(BranchAsm, RefBranchAsm),
    transformBranchesForRef(BranchesAsm, RefBranchesAsm).

transformBranchForRef([], []).
transformBranchForRef([Cmd | Cmds], CmdsRef) :-
    Cmd = destruct(Expr, Name, Args) ->
        transformBranchForRef([destruct_ref(Expr, Name, Args) | Cmds], CmdsRef)
        ;
        (Cmd = delete(_) ->
            transformBranchForRef(Cmds, CmdsRef)
            ;
            CmdsRef = [Cmd | RestCmdsRef],
            transformBranchForRef(Cmds, RestCmdsRef)).

transformCommandForRef(destruct(Expr, Name, Args), destruct_ref(Expr, Name, Args)).

:- begin_tests(unnameVars).

% unnameVars reverses the operation of nameVars. While the latter works on a list of
% variable names and unifies the variables with Name::_, unnameVars walks a term,
% identifies all elements of the form Name::_, assigns a free variable to each, and
% then replaces each such element with its corresponding variable.
test(unnameVars) :-
    assignFakeTypes([FakeType]),
    unnameVars(foo('A'::int64, 'B'::_)+
               foo('A'::int64, C::string)+
               foo(C::string, _) + 
               [FakeType:seq(FakeType)], Unnamed),
    Unnamed =@= foo(A, _B)+
                foo(A, C)+
                foo(C, _)+
                [T:seq(T)].

:- end_tests(unnameVars).

unnameVars(Term, Unnamed) :-
    walk(Term, findNames, [], Names),
    replaceInTerm(Term, replaceNamed(Names), Unnamed).

findNames(Term, StateIn, StateOut) :-
    Term = Name::_ ->
        (atom(Name),  \+member(Name=_, StateIn) ->
            StateOut = [Name=_ | StateIn]
            ;
            StateOut = StateIn)
        ;
        fake_type(Term) ->
            (\+member(Term=_, StateIn) ->
                StateOut = [Term=_ | StateIn]
                ;
                StateOut = StateIn)
            ;
            fail.

replaceNamed(Names, Term, Unnamed) :-
    Term = Name::_ ->
        (var(Name) ->
            Unnamed = Name
            ;
            member(Name=Unnamed, Names))
        ;
        fake_type(Term) ->
            member(Term=Unnamed, Names)
            ;
            fail.

replaceInTerm(Term, Pred, Replaced) :-
    call(Pred, Term, Replaced) ->
        true
        ;
        my_callable(Term) ->
            Term =.. [Name | Terms],
            replaceInTerms(Terms, Pred, ReplacedTerms),
            Replaced =.. [Name | ReplacedTerms]
            ;
            Replaced = Term.

replaceInTerms([], _, []).
replaceInTerms([Term | Terms], Pred, [RepTerm | RepTerms]) :-
    replaceInTerm(Term, Pred, RepTerm),
    replaceInTerms(Terms, Pred, RepTerms).

:- begin_tests(specialize).

% The specialize predicate evaluates one list of assembly instructions into another,
% "simpler" list, possibly unifying variables in the process.

% For example, the literal command is specialized by removing it and unifying the
% result variable with the literal value.
test(literal) :-
    specialize([literal(42, FortyTwo),
                literal("hello", Hello)], Asm),
    FortyTwo == 42,
    Hello == "hello",
    Asm == [].

% Construct commands construct compound terms based on their arguments.
test(construct) :-
    specialize([construct(foo, [1, 2], X),
                construct(bar, [X, 3], Y)], Asm),
    Y == bar(foo(1, 2), 3),
    Asm == [].

% Destruct works by destructing compound terms into their components.
test(destruct) :-
    specialize([destruct(bar(foo(1, 2), 3), bar, [Foo, Three]),
                destruct(Foo, foo, [One, Two])], Asm),
    One == 1,
    Two == 2,
    Three == 3,
    Asm == [].

% destruct_ref works the same way.
test(destruct_ref) :-
    specialize([destruct_ref(bar(foo(1, 2), 3), bar, [Foo, Three]),
                destruct_ref(Foo, foo, [One, Two])], Asm),
    One == 1,
    Two == 2,
    Three == 3,
    Asm == [].

% If given a free variable, destruct remains unevaluated, but allows evaluation continue.
test(destruct_free_var) :-
    specialize([destruct(X, foo, [Y, Z]),
                literal(3, Three)], Asm),
    Three = 3,
    Asm = [destruct(X, foo, [Y, Z])].

% Deletes are simply ignored (and removed).
test(destruct_free_var) :-
    specialize([delete(_),
                delete(_)], Asm),
    Asm = [].

% Calls to implemented functions are replaced with their bodies.
test(call_implemented) :-
    specialize([call(+, [A, B], [int64:plus], C),
                call(+, [C, D], [int64:plus], R)], Asm),
    Asm == [call(int64_plus, [A, B], [], C),
            call(int64_plus, [C, D], [], R)].

% When calling a built-in function that has a Prolog implementation with all arguments
% ground (no free variables), the Prolog implementation is used to evaluate the result (constant propagation)
test(constant_propagation) :-
    specialize([call(+, [1, 2], [int64:plus], X),
                call(+, [X, 3], [int64:plus], R)], Asm),
    Asm == [],
    R == 6.

% A case command is not specialized if the expression it destructs is a free variable.
% In such a case, the command is left untouched, but further commands are specialized.
test(case_with_free_var) :-
    specialize([
        case(X, [
            [destruct(X, foo, [A, B]),
            construct(bar, [B, A], Res)],
            [destruct(X, bar, [X, Y]),
            construct(foo, [Y, X], Res)]]),
        literal(2, Two)], Asm),
    Two == 2,
    Asm == [case(X, [
            [destruct(X, foo, [A, B]),
            construct(bar, [B, A], Res)],
            [destruct(X, bar, [X, Y]),
            construct(foo, [Y, X], Res)]])].

% If the expression is not a free variable, a single branch is chosen based on the value
% of the expression (the first command in the branch is expected to destruct the 
% expression). That branch replaces the case command and is specialized.
test(case_with_concrete_expr) :-
    specialize([
        case(bar(X1, Y1), [
            [destruct(bar(X1, Y1), foo, [A, B]),
            construct(bar, [B, A], Res)],
            [destruct(bar(X1, Y1), bar, [X, Y]),
            construct(foo, [Y, X], Res)]]),
        literal(2, Two)], Asm),
    Two == 2,
    Res == foo(Y1, X1),
    Asm == [].


:- end_tests(specialize).

specialize([], []).
specialize([literal(Val, Val) | AsmIn], AsmOut) :-
    specialize(AsmIn, AsmOut).
specialize([construct(Name, Args, Term) | AsmIn], AsmOut) :-
    Term =.. [Name | Args],
    specialize(AsmIn, AsmOut).
specialize([destruct(Term, Name, Args) | AsmIn], AsmOut) :-
    var(Term) ->
        AsmOut = [destruct(Term, Name, Args) | AsmMid],
        specialize(AsmIn, AsmMid)
        ;
        Term =.. [Name | Args],
        specialize(AsmIn, AsmOut).
specialize([destruct_ref(Term, Name, Args) | AsmIn], AsmOut) :-
    specialize([destruct(Term, Name, Args) | AsmIn], AsmOut).
specialize([delete(_) | AsmIn], AsmOut) :-
    specialize(AsmIn, AsmOut).
specialize([call(Name, Args, TypeGuard, Result) | AsmIn], AsmOut) :-
    function_impl(Name, TypeGuard, Args, Body, Result) ->
        append(Body, AsmIn, Asm1),
        specialize(Asm1, AsmOut)
        ;
        ground(Args),
        Func =.. [Name | Args],
        constant_propagation(Func, Result) ->
            specialize(AsmIn, AsmOut)
            ;
            AsmOut = [call(Name, Args, TypeGuard, Result) | Asm2],
            specialize(AsmIn, Asm2).
specialize([case(Expr, Branches) | AsmIn], AsmOut) :-
    var(Expr) ->
        AsmOut = [case(Expr, Branches) | Asm1],
        specialize(AsmIn, Asm1)
        ;
        selectAsmBranch(Branches, Branch),
        append(Branch, AsmIn, Asm2),
        specialize(Asm2, AsmOut).

selectAsmBranch([[DestructCmd | RestOfFirstBranch] | Branches], Branch) :-
    specialize([DestructCmd], []) ->
        Branch = RestOfFirstBranch
        ;
        selectAsmBranch(Branches, Branch).

% Prelude
:- compileStatement((union bool = true + false), []).
:- compileStatement((union list(T) = [] + [T | list(T)]), ['T'=T]).
:- compileStatement((union maybe(T) = just(T) + none), ['T'=T]).
:- compileStatement((struct (A, B) = (A, B)), ['A'=A, 'B'=B]).
:- compileStatement((class T : plus where { T+T->T }), ['T'=T]).
:- compileStatement((instance int64 : plus where { A+B := int64_plus(A, B) }),
    ['A'=A, 'B'=B]).
:- compileStatement((instance float64 : plus where { A+B := float64_plus(A, B) }),
    ['A'=A, 'B'=B]).
:- compileStatement((instance string : plus where { A+B := strcat(A, B) }),
    ['A'=A, 'B'=B]).
:- compileStatement((class T : minus where { T-T->T }), ['T'=T]).
:- compileStatement((instance int64 : minus where { A-B := int64_minus(A, B) }),
    ['A'=A, 'B'=B]).
:- compileStatement((instance float64 : minus where { A-B := float64_minus(A, B) }),
    ['A'=A, 'B'=B]).
