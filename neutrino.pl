:- op(1090, xfx, =>).
:- op(1090, xfx, :=).
:- op(1070, fx, declare).
:- op(1050, xfy, @>).
:- op(1050, fx, assert).
:- op(1050, fx, union).
:- op(1050, fx, struct).
:- op(1050, fx, class).
:- op(1050, fx, instance).
:- op(1020, xfx, del).
:- op(900, xfx, where).
:- op(700, fx, case).
:- op(600, xfx, of).
:- op(300, yfx, !).
:- op(300, yfx, @).
:- op(200, fx, &).
:- op(200, fx, *).
:- op(100, fx, !).
:- op(100, xfx, ::).

:- dynamic type_signature/4.
:- dynamic type/1.
:- dynamic union_type/2.
:- dynamic is_constructor/2.
:- dynamic type_class/4.
:- dynamic class_instance/3.
:- dynamic fake_type/1.
:- dynamic is_struct/1.
:- dynamic function_impl/5.

:- discontiguous constant_propagation/2.
:- discontiguous type_signature/4.
:- discontiguous class_instance/3.
:- discontiguous syntacticMacro/2.

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

compileStatement((assert ExprWithMacros), VNs) :-
    nameVars(VNs),
    applyMacros(ExprWithMacros, Expr),
    inferType(Expr, Type, Assumptions),
    matchType(Type, bool, Expr),
    checkAssumptions(Assumptions),
    once(assembly(Expr, Result, [], Asm)),
    linearityCheck(Asm, Result, [], _),
    unnameVars((Asm, Result), (UnAsm, UnResult), _),
    specialize(UnAsm, Residual),
    (\+(UnResult == true) ->
        throw(assertion_failed(UnResult, Residual))
        ;
        true).

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
    !compileUnionDeleteInstance(Union, OptionList, VNs),
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
    !compileStructDeleteInstance(Type, Constructor),
    nameVars(VNs),
    verifyTypeVariables(Args),
    walk(Constructor, verifyVarIsType(Name), [], _),
    validateTypes(ConsArgs).

compileStatement((class T:C where {Decls}), VNs) :-
    compileStatement((class T:C where {Decls}), VNs, []).

compileStatement((instance T:C where {Defs}), VNs) :-
    compileStatement((instance T:C where {Defs}), VNs, []).

compileStatement((Context => Statement), VNs) :-
    tupleToList(Context, ContextAsList),
    compileStatement(Statement, VNs, ContextAsList).

compileStatement((declare Func -> Type), _VNs, Context) :-
    Func =.. [Name | ArgTypes],
    assert(type_signature(Name, ArgTypes, Type, Context)).

compileStatement((instance T:C where {Defs}), VNs, Context) :-
    (type_class(C, T, Decls, ClassContext) ->
        true
        ;
        throw(type_class_does_not_exist(C))),
    assert(class_instance(T, C, Context)),
    !nameVars(VNs),
    !validateInstance(Decls, Defs, T, C),
    saturateTypes(Context),
    compileMethods(Defs, ClassContext).

compileStatement((class T:C where {Decls}), VNs, Context) :-
    ClassCtx = [T:C | Context],
    declareClassFunctions(Decls, ClassCtx),
    assert(type_class(C, T, Decls, ClassCtx)),
    (var(T) ->
        true
        ;
        throw(instance_type_not_var_in_class_decl(T, C))),
    validateClassDecls(Decls, C, T),
    nameVars(VNs),
    validateDecls(Decls, ClassCtx).

compileFunctionDefinition((Func := Body), TypeContext) :-
    walk(Func, replaceSingletosWithDelete, [], _),
    applyMacros(Body, BodyAfterMacros),
    Func =.. [Name | Args],
    inferType(BodyAfterMacros, Type, BodyAssumptions),
    inferTypes(Args, ArgTypes, ArgAssumptions),
    validateLValues(Args, ArgTypes),
    sameLength(ArgTypes, SigArgTypes),
    (type_signature(Name, SigArgTypes, SigType, TypeContext) ->
        !saturateTypes(TypeContext),
        matchTypes(ArgTypes, SigArgTypes, Args),
        matchType(Type, SigType, BodyAfterMacros),
        checkAssumptions(BodyAssumptions),
        checkAssumptions(ArgAssumptions)
        ;
        validateArgTypes(Args),
        TypeContext = [],
        assert(type_signature(Name, ArgTypes, Type, TypeContext))),
    !assembly(BodyAfterMacros, Result, [], BodyAsm),
    !destructAssemblies(Args, DestArgs, BodyAsm, Asm),
    walk(Asm, enumerateVars, 0, _),
    !stateMapList(introduceVars, DestArgs, [], VarState1),
    !linearityCheck(Asm, Result, VarState1, _),
    unnameVars((TypeContext, DestArgs, Asm, Result), 
               (UnTypeContext, UnArgs, UnAsm, UnResult), _),
    assert(function_impl(Name, UnTypeContext, UnArgs, UnAsm, UnResult)).

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
    ["Type ", T, " is not an instance of class ", C,
    ". It is, however, an instance of ", Classes]) :-
        findall(C1, class_instance(T, C1, _), Classes).
formatError(type_class_does_not_exist(C),
    ["Type class ", C, " does not exist."]).
formatError(undefined_expression(Functor),
    ["Undefined expression ", Functor, "."]).
formatError(lend_and_consume(Name),
    ["Attempt to both consume and lend variable ", Name, " in a single step."]).
formatError(assertion_failed(Result, Residual),
    ["Assertion has failed. Expected true, got ", Result,
     ". Redsidual code: ", Residual]).
formatError(type_var_not_declared(Type),
    ["Type variable ", Type, " has not been declared in this context."]).

writeln_list(S, [First | Rest]) :-
    write(S, First),
    writeln_list(S, Rest).

writeln_list(S, []) :-
    writeln(S, "").

type_signature(*, [&T], T, [T:basic_type]).

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

type_signature(delete_string, [string, T], T, [T:any]).
constant_propagation(delete_string(_, X), X).

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
type(_::type).

basicType(T) :- class_instance(T, basic_type, _).

class_instance(int64, basic_type, []).
class_instance(float64, basic_type, []).
class_instance(&_, basic_type, []).
class_instance(T, basic_type, []) :-
    union_type(T, Options),
    forall(member(Opt, Options), functor(Opt, _, 0)).

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
        Kind = type
        ;
        throw(double_use_of_var(Name)).

inferTypeSpecial(_::T, T, []).
inferTypeSpecial(N, int64, []) :- integer(N).
inferTypeSpecial(N, float64, []) :- float(N).
inferTypeSpecial(S, string, []) :- string(S).

inferTypeSpecial(case Expr of {Branches}, OutType, Assumptions) :-
    inferCaseExprType(Expr, Branches, OutType, Assumptions, (=)).

inferTypeSpecial(case Expr of & {Branches}, OutType, Assumptions) :-
    inferCaseExprType(Expr, Branches, OutType, Assumptions, makeRef).

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

makeRef(T, &T).

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

lValue(_::_).

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
        Var = '_'::_
        ;
        Var = _::_.

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
                        [literal(1, One::int64),
                         literal(2, Two::int64),
                         call(+, [One::int64, Two::int64], [int64:plus], Val::int64)]).

% Nested functions are assembled bottom-up.
test(nested_func) :-
    once(assembly(1+2+3, Val::int64, [], Assembly)),
    (Val, Assembly) =@= (Val,
                        [literal(1, One::int64),
                         literal(2, Two::int64), 
                         call(+, [One::int64, Two::int64], [int64:plus], X::int64),
                         literal(3, Three::int64),
                         call(+, [X::int64, Three::int64], [int64:plus], Val::int64)]).

% Case expressions are assembled by first assembling the expression being matched,
% then for each branch assembling a sequence that first destructs the pattern and
% then builds the expression it maps to.
test(case) :-
    compileStatement((union foobar1 = foo1(int64) + bar1(float64)), []),
    assembly((case foo1(42) of {
        foo1('A'::int64) => 'A'::int64 == 1;
        bar1('_'::float64) => false
    }), Val, [], Asm),
    (Val, Asm) =@= (Val,
                    [literal(42,FourtyTwo::int64),
                     construct(foo1,[FourtyTwo::int64],X::foobar1),
                     case(X::foobar1,
                        [[destruct(X::foobar1,foo1,['A'::int64]),
                         literal(1,One::int64),
                         call(==,['A'::int64,One::int64],[],V1::bool),
                         assign(V1::bool, Val)],
                       [destruct(X::foobar1,bar1,[Y::float64]),
                        literal(0,Dummy::int64),
                        call(del,[Dummy::int64,Y::float64],[float64:delete,int64:any],_),
                        construct(false,[],V2::bool),
                         assign(V2::bool, Val)]])]).


% Case expression over reference types differ from normal case expressions
% in that instead of using destruct they use destruct_ref (which extracts
% the arguments but does not destroy the object), and in that deletes are
% not placed.
test(case_ref) :-
    compileStatement((union foobar2 = foo2(string) + bar2(string)), []),
    assembly((case foo2("hello") of & {
        foo2('A'::(&string)) => 'A'::(&string) == 'A'::(&string);
        bar2('_'::(&string)) => false
    }), Val, [], Asm),
    (Val, Asm) =@= (Val,
                    [literal("hello", Hello::string),
                     construct(foo2, [Hello::string], X::foobar2),
                     case(X::foobar2,
                     [[destruct_ref(X::foobar2, foo2, ['A'::(&string)]),
                       call(==, ['A'::(&string), 'A'::(&string)], [], V1::bool),
                       assign(V1::bool, Val)],
                      [destruct_ref(X::foobar2, bar2, [_::(&string)]),
                       construct(false, [], V2::bool),
                       assign(V2::bool, Val)]])]).
:- end_tests(assembly).

assemblySpecial(Expr, Val::Type, Asm, [literal(Expr, Val::Type) | Asm]) :-
    isSimpleExpr(Expr),
    inferType(Expr, Type, _).

assemblySpecial(Name::Type, Name::Type, Asm, Asm).
assemblySpecial(&Name::Type, &Name::Type, Asm, Asm).
assemblySpecial(*Var, Var, Asm, Asm).

assemblySpecial((case Expr of {Branches}), Val, AsmIn, AsmOut) :-
    branchesAssembly(ExprVal, Branches, Val, destructAssembly, BranchesAsm),
    assembly(Expr, ExprVal, [case(ExprVal, BranchesAsm) | AsmIn], AsmOut).

assemblySpecial((case Expr of & {Branches}), Val, AsmIn, AsmOut) :-
    !branchesAssembly(ExprVal, Branches, Val, destructRefAssembly, BranchesAsm),
    !assembly(Expr, ExprVal, [case(ExprVal, BranchesAsm) | AsmIn], AsmOut).

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
            assemblies(Args, Vals,
                [call(Name, Vals, Assumptions, Val) | AsmIn], AsmOut)),
        inferTypes(Vals, ArgTypes1, _),
        reconcileTypes(ArgTypes, ArgTypes1),
        !inferTypeClasses(Assumptions).

assemblies([], [], Asm, Asm).
assemblies([Expr | Exprs], [Val | Vals], AsmIn, AsmOut) :-
    assemblies(Exprs, Vals, AsmIn, AsmMid),
    assembly(Expr, Val, AsmMid, AsmOut).

isSimpleExpr(Expr) :- number(Expr).
isSimpleExpr(Expr) :- string(Expr).

branchesAssembly(Expr, Branches, Val, DestructPred, BranchesAsm) :-
    Branches = (FirstBranch; RestBranches) ->
        !branchesAssembly(Expr, FirstBranch, Val, DestructPred, FirstAsm),
        !branchesAssembly(Expr, RestBranches, Val, DestructPred, RestAsm),
        append(FirstAsm, RestAsm, BranchesAsm)
        ;
        !(Branches = (Pattern => Result)),
        !assembly(Result, Val1::Type, [assign(Val1::Type, Val)], ResultAsm),
        !call(DestructPred, Pattern, Expr, ResultAsm, DestAsm),
        BranchesAsm = [DestAsm].

:- begin_tests(destructAssemblies).

test(struct) :-
    destructAssemblies([(_::int64, '_'::string)], Args, [], Asm),
    (Args, Asm) =@= ([X], [destruct(X, ',', [_::int64, Y::string]),
                           literal(0, Dummy::int64),
                           call(del, [Dummy::int64, Y::string], 
                                     [string:delete, int64:any], _)]).

:- end_tests(destructAssemblies).

destructAssemblies([], [], Asm, Asm).

destructAssemblies([Var::Type | Args], [X::Type | DestArgs], AsmIn, AsmOut) :-
    Var == '_' ->
        destructAssemblies(Args, DestArgs,
            [literal(0, Dummy::int64),
            call(del, [Dummy::int64, X::Type], [Type:delete, int64:any], _) 
            | AsmIn], AsmOut)
        ;
        Var = X,
        destructAssemblies(Args, DestArgs, AsmIn, AsmOut).

destructAssemblies([&Cons | MoreArgs], [X | MoreDestArgs], AsmIn, AsmOut) :-
    my_callable(Cons),
    functor(Cons, Name, Arity),
    is_struct(Name/Arity),
    destructRefAssembly(Cons, X, AsmIn, AsmMid),
    destructAssemblies(MoreArgs, MoreDestArgs, AsmMid, AsmOut).

destructAssemblies([Cons | MoreArgs], [X | MoreDestArgs], AsmIn, AsmOut) :-
    my_callable(Cons),
    functor(Cons, Name, Arity),
    is_struct(Name/Arity),
    destructAssembly(Cons, X, AsmIn, AsmMid),
    destructAssemblies(MoreArgs, MoreDestArgs, AsmMid, AsmOut).

destructAssembly(Cons, Val, AsmIn, AsmOut) :-
    Cons =.. [Name | Args],
    destructAssemblies(Args, DestArgs, AsmIn, AsmMid),
    AsmOut = [destruct(Val, Name, DestArgs) | AsmMid].

destructRefAssemblies([], [], Asm, Asm).
destructRefAssemblies([VarIn::Type | Args], [VarOut::Type | DestArgs], AsmIn, AsmOut) :-
    (VarIn == '_' ->
        true
        ;
        VarOut = VarIn),
    destructRefAssemblies(Args, DestArgs, AsmIn, AsmOut).

destructRefAssemblies([&Cons | MoreArgs], [X | MoreDestArgs], AsmIn, AsmOut) :-
    my_callable(Cons),
    functor(Cons, Name, Arity),
    is_struct(Name/Arity),
    !destructRefAssembly(Cons, X, AsmIn, AsmMid),
    !destructRefAssemblies(MoreArgs, MoreDestArgs, AsmMid, AsmOut).

destructRefAssembly(Cons, Val, AsmIn, AsmOut) :-
    Cons =.. [Name | Args],
    !destructRefAssemblies(Args, DestArgs, AsmIn, AsmMid),
    AsmOut = [destruct_ref(Val, Name, DestArgs) | AsmMid].

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

linearityCheckStep(assign(In, Out), _, StateIn, StateOut) :-
    consumeVar(In, StateIn, StateMid),
    introduceVar(Out, StateMid, StateOut).

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

declareClassFunctions((ContextTuple => Decl), TypeContext) :-
    tupleToList(ContextTuple, ContextAsList),
    append(TypeContext, ContextAsList, FullContext),
    declareClassFunctions(Decl, FullContext).

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

validateInstance((_ => Decl), Defs, T, C) :-
    validateInstance(Decl, Defs, T, C).

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

validateClassDecls((_ => Decl), C, T) :-
    validateClassDecls(Decl, C, T).

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

compileMethods((Method1; Method2), Ctx) :-
    compileMethods(Method1, Ctx),
    compileMethods(Method2, Ctx).

compileMethods((Func := Expr), Ctx) :-
    append(Ctx, _, MethodCtx),
    copy_term((Func := Expr), MethodDef),
    compileFunctionDefinition(MethodDef, MethodCtx).

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
        assert(class_instance(T, C, []))
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
    var(T) ->
        true
        % Rationale: If T is a free variable it means that the expression being
        % evaluated is a contructor that does not specify T (e.g., [] for list(T)).
        % In such cases we cannot check that T is a member of C, but checking this
        % is not important as no object of T exists in the expression.
        ;
        checkAssumption(T:C) ->
            checkAssumptions(Rest)
            ;
            throw(type_not_instance(T, C)).

% TODO: Check the class's underlying assumptions.
checkAssumption(T:C) :-
    class_instance(T, C, Context),
    checkAssumptions(Context).

sumToList(Sum, List) :-
    sumToList(Sum, [], List).

sumToList(Sum, ListIn, ListOut) :-
    Sum = A+B ->
        sumToList(A, [B | ListIn], ListOut)
        ;
        ListOut = [Sum | ListIn].

my_callable(X) :- callable(X).
my_callable(X) :- X == [].

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
        CmdsRef = [Cmd | RestCmdsRef],
        transformBranchForRef(Cmds, RestCmdsRef).

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
               [FakeType:seq(FakeType)], Unnamed, VNs),
    Unnamed =@= foo(A, B)+
                foo(A, C)+
                foo(C, _)+
                [T:seq(T)],
    Unnamed = foo(A, B)+
              foo(A, C)+
              foo(C, _)+
              [T:seq(T)],
    member('A'=A1, VNs),
    A1 == A.

:- end_tests(unnameVars).

unnameVars(Term, Unnamed, Names) :-
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

% The assign command unifies its output with its input.
test(assign) :-
    specialize([assign(2, A),
                assign(A, B)], Asm),
    B == 2,
    Asm == [].

:- end_tests(specialize).

specialize([], []).
specialize([literal(Val, Val) | AsmIn], AsmOut) :-
    trace(AsmIn),
    !specialize(AsmIn, AsmOut).
specialize([construct(Name, Args, Term) | AsmIn], AsmOut) :-
    Term =.. [Name | Args],
    trace(AsmIn),
    !specialize(AsmIn, AsmOut).
specialize([destruct(Term, Name, Args) | AsmIn], AsmOut) :-
    var(Term) ->
        AsmOut = [destruct(Term, Name, Args) | AsmMid],
        trace(AsmIn),
        !specialize(AsmIn, AsmMid)
        ;
        Term =.. [Name | Args],
        trace(AsmIn),
        !specialize(AsmIn, AsmOut).
specialize([destruct_ref(Term, Name, Args) | AsmIn], AsmOut) :-
    (Term = &RefTerm ->
        true
        ;
        RefTerm = Term),
    Asm1 = [destruct(RefTerm, Name, Args) | AsmIn], 
    trace(Asm1),
    specialize(Asm1, AsmOut).
specialize([call(Name, Args, TypeGuard, Result) | AsmIn], AsmOut) :-
    function_impl(Name, TypeGuard, Args, Body, Result) ->
        append(Body, AsmIn, Asm1),
        trace(Asm1),
        !specialize(Asm1, AsmOut)
        ;
        ground(Args),
        removeRefs(Args, ArgsNoRefs),
        Func =.. [Name | ArgsNoRefs],
        constant_propagation(Func, Result) ->
            trace(AsmIn),
            !specialize(AsmIn, AsmOut)
            ;
            AsmOut = [call(Name, Args, TypeGuard, Result) | Asm2],
            trace(AsmIn),
            !specialize(AsmIn, Asm2).
specialize([case(Expr, Branches) | AsmIn], AsmOut) :-
    var(Expr) ->
        AsmOut = [case(Expr, Branches) | Asm1],
        trace(AsmIn),
        !specialize(AsmIn, Asm1)
        ;
        selectAsmBranch(Branches, Branch),
        append(Branch, AsmIn, Asm2),
        trace(Asm2),
        !specialize(Asm2, AsmOut).

specialize([assign(Val, Val) | AsmIn], AsmOut) :-
    specialize(AsmIn, AsmOut).

% trace(Asm) :- writeln(trace=Asm).
trace(_).

selectAsmBranch([[DestructCmd | RestOfFirstBranch] | Branches], Branch) :-
    specialize([DestructCmd], []) ->
        Branch = RestOfFirstBranch
        ;
        selectAsmBranch(Branches, Branch).

class_instance(_, any, []).

validateDecls((Decl1; Decl2), Ctx) :-
    validateDecls(Decl1, Ctx),
    validateDecls(Decl2, Ctx).
    
validateDecls((MoreCtx => Decl), Ctx) :-
    tupleToList(MoreCtx, MoreCtxAsList),
    append(Ctx, MoreCtxAsList, FullCtx),
    validateDecls(Decl, FullCtx).
    
validateDecls((Func -> Type), Ctx) :-
    walk(Func, checkVarIsIn(Ctx), [], _),
    walk(Type, checkVarIsIn(Ctx), [], _).

checkVarIsIn(Ctx, Type::_, State, State) :-
    member(Type::_ : _, Ctx) ->
        true
        ;
        throw(type_var_not_declared(Type)).

compileStructDeleteInstance(Type, Constructor) :-
    functor(Constructor, Name, Arity),
    length(Underscores, Arity),
    fillWithUnderscores(Underscores),
    ConsWithUnderscores =.. [Name | Underscores],
    copy_term(Type, TypeCopy),
    compileStatement((instance TypeCopy : delete where {
        X del ConsWithUnderscores := X
    }), ['X'=X]).

fillWithUnderscores([]).
fillWithUnderscores(['_'::_ | L]) :-
    fillWithUnderscores(L).

compileUnionDeleteInstance(Union, OptionList, VNs) :-
    copy_term(Union, Union1),
    deleteUnionBranches(OptionList, X, Branches),
    compileStatement((instance Union1 : delete where {
        X del U := case U of {
            Branches
        }
    }), ['x'=X, 'u'=U | VNs]).

deleteUnionBranches([Op1, Op2 | Options], X, (Branch1; Branches)) :-
    deleteUnionBranches([Op1], X, Branch1),
    deleteUnionBranches([Op2 | Options], X, Branches).

deleteUnionBranches([Op], X, Branch) :-
    functor(Op, Name, Arity),
    length(Underscores, Arity),
    fillWithUnderscores(Underscores),
    LHS =.. [Name | Underscores],
    Branch = (LHS => X).

:- begin_tests(sum_to_list).

test(sumToList) :-
    sumToList(1+2+3+4+5, X),
    X == [1, 2, 3, 4, 5].

:- end_tests(sum_to_list).

enumerateVars(Var::_, N, N1) :-
    var(Var),
    N1 is N + 1,
    atom_number(Num, N),
    atom_concat('v', Num, Var).

applyMacros(ExprWithMacros, Expr) :-
    replaceInTerm(ExprWithMacros, syntacticReplacement, Expr).

syntacticReplacement(TermIn, TermOut) :-
    nonvar(TermIn),
    syntacticMacro(TermIn, TermOut).

syntacticMacro(
    if(Cond, Then, Else),
    (case Cond of {
        true => Then;
        false => Else
    })).

:- begin_tests(extractLambda).

test(inc_lambda) :-
    reset_gensym(lambda),
    extractLambda('X'::T, 'X'::T+1, =, '->', '!', StructDef, InstanceDef, Replacement),
    StructDef == (struct lambda1 = lambda1),
    InstanceDef =@= (instance lambda1 : (int64 -> int64) where {
        lambda1!('X'::int64) := ('X'::int64)+1
    }),
    Replacement == lambda1.

test(polymorphic_lambda) :-
    reset_gensym(lambda),
    extractLambda('X'::T, 'X'::T+'X'::T, =, '->', '!',
        _StructDef, InstanceDef, _Replacement),
    InstanceDef =@= (T1 : plus => instance lambda1 : (T1 -> T1) where {
        lambda1!('X'::T1) := ('X'::T1)+('X'::T1)
    }).

test(monomorphic_closure) :-
    reset_gensym(lambda),
    once(extractLambda('X'::Tx, 'X'::Tx+'Y'::_+1, =, '->', '!',
        StructDef, InstanceDef, Replacement)),
    StructDef =@= (struct lambda1 = lambda1(int64)),
    InstanceDef =@= (instance lambda1 : (int64 -> int64) where {
        lambda1('Y'::int64)!('X'::int64) := ('X'::int64)+('Y'::int64)+1
    }),
    Replacement == lambda1('Y'::int64).

test(polymorphic_closure) :-
    reset_gensym(lambda),
    once(extractLambda('X'::Tx, 'X'::Tx+'Y'::Ty, =, '->', '!',
        StructDef, InstanceDef, Replacement)),
    StructDef =@= (struct lambda1(T) = lambda1(T)),
    InstanceDef =@= (T : plus => instance lambda1(T) : (T -> T) where {
        lambda1('Y'::T)!('X'::T) := ('X'::T)+('Y'::T)
    }),
    Replacement == lambda1('Y'::Ty).

:- end_tests(extractLambda).

extractLambda(X, Y, TypeModifier, ClassName, MethodName,
        StructDef, InstanceDef, Replacement) :-
    StructDef = (struct LambdaStructType = LambdaStructSig),
    Replacement = LambdaCons,
    applyMacros(Y, YAfterMacros),
    gensym(lambda, LambdaName),
    inferType(X, Tx, _),
    inferType(YAfterMacros, Ty, Assumptions),
    once(lambdaTypesAndArgs(X, YAfterMacros, Types, Args)),
    LambdaStructSig =.. [LambdaName | Types],
    term_variables(Types, TypeVars),
    LambdaStructType =.. [LambdaName | TypeVars],
    LambdaCons =.. [LambdaName | Args],
    filterMetAssumptions(Assumptions, NeededAssumptions),
    Class =.. [ClassName, Tx, Ty],
    call(TypeModifier, LambdaCons, LambdaConsMod),
    Method =.. [MethodName, LambdaConsMod, X],
    InstanceDef1 = (instance LambdaStructType : Class where {
        Method := YAfterMacros
    }),
    (NeededAssumptions = [_|_] ->
        listToTuple(NeededAssumptions, Context),
        InstanceDef = (Context => InstanceDef1)
        ;
        InstanceDef = InstanceDef1).

lambdaTypesAndArgs(X, Y, Types, ClosureVars) :-
    walk(Y, findVars, [], VarsInBody),
    walk(X, findVars, [], VarsInHead),
    (setof(Var, (member(Var, VarsInBody), 
                 \+member(Var, VarsInHead)), ClosureVars); true),
    extractVarTypes(ClosureVars, Types).

extractVarTypes([], []).
extractVarTypes([_::Type | VarWithTypes], [Type | Types]) :-
    extractVarTypes(VarWithTypes, Types).

listToTuple([A], A).
listToTuple([A, B | C], (A, BC)) :-
    listToTuple([B | C], BC).

filterMetAssumptions([], []).
filterMetAssumptions([T:C | Assum], AssumOut) :-
    nonvar(T),
    checkAssumption(T:C) ->
        filterMetAssumptions(Assum, AssumOut)
        ;
        AssumOut = [T:C | AssumMid],
        filterMetAssumptions(Assum, AssumMid).


findVars(Name::Type, State, [Name::Type | State]).

syntacticMacro((X->Y), Replacement) :-
    lambdaMacro(X, Y, =, '->', '!', Replacement).

syntacticMacro((X@>Y), Replacement) :-
    lambdaMacro(X, Y, addRef, '@>', '@', Replacement).

addRef(X, &X).

lambdaMacro(X, Y, TypeModifier, ClassName, MethodName, Replacement) :-
    !extractLambda(X, Y, TypeModifier, ClassName, MethodName,
        StructDef, InstanceDef, Replacement),
    !unnameVars(StructDef, StructDef1, StructDefVNs),
    !compileStatement(StructDef1, StructDefVNs),
    !unnameVars(InstanceDef, InstanceDef1, InstanceDefVNs),
    !compileStatement(InstanceDef1, InstanceDefVNs).

removeRefs([], []).
removeRefs([Arg | Args], [ArgNoRefs | ArgsNoRefs]) :-
    (Arg = &ArgNoRefs ->
        true
        ;
        ArgNoRefs = Arg),
    removeRefs(Args, ArgsNoRefs).

inferTypeClasses([]).
inferTypeClasses([T:C | Assumptions]) :-
    var(T) ->
        true
        ;
        class_instance(T, C, InstanceAssumptions),
        inferTypeClasses(InstanceAssumptions),
        inferTypeClasses(Assumptions).

reconcileTypes([], []).
reconcileTypes([Type | Types], [Type1 | Types1]) :-
    reconcileType(Type, Type1),
    reconcileTypes(Types, Types1).

reconcileType(T1, T2) :-
    unifiable(T1, T2, _) ->
        T1 = T2
        ;
        (nonvar(T1), basicType(T1); nonvar(T2), basicType(T2)),
        (T1 = &T2 ; &T1 = T2).

syntacticMacro(A >> B, Result) :-
    applyMacros(A, A1),
    applyMacros(B, B1),
    B1 =.. [Name | Args],
    Result =.. [Name, A1 | Args].

syntacticMacro((Bind << {Statements}), BindExpr) :-
    bindExpression(Statements, Bind, BindExprWithLambdas),
    applyMacros(BindExprWithLambdas, BindExpr).

bindExpression(Statements, Bind, BindExpr) :-
    Statements = (Var := Expr; Rest) ->
        BindExpr =.. [Bind, Expr, (Var -> RestExpr)],
        bindExpression(Rest, Bind, RestExpr)
        ;
        BindExpr = Statements.

% ============= Prelude =============
:- compileStatement((class T : delete where { X : any => X del T -> X }),
    ['T'=T, 'X'=X]).
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
:- compileStatement((instance int64 : delete where { X del N := X }),
    ['X'=X, 'N'=N]).
:- compileStatement((instance float64 : delete where { X del N := X }),
    ['X'=X, 'N'=N]).
:- compileStatement((instance string : delete where 
    { X del S := delete_string(S, X) }), ['X'=X, 'S'=S]).
:- compileStatement((T1 : any, T2 : any => class F:(T1->T2) where { F!T1 -> T2 }),
    ['T1'=T1, 'T2'=T2, 'F'=F]).
:- compileStatement((T1 : any, T2 : any => class F:(T1@>T2) where { &F@T1 -> T2 }),
    ['T1'=T1, 'T2'=T2, 'F'=F]).
