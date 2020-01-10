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
:- dynamic is_option/2.
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
    assembly(Expr, Result, [], Asm),
    linearityCheck(Asm, Result, [], _),
    unnameVars((Asm, Result), (UnAsm, UnResult::bool), _),
    !specialize(UnAsm, Residual),
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
    assertOptionSignatures(Options, Union, 0, _),
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
    !compileStatement((class T:C where {Decls}), VNs, []).

compileStatement((instance T:C where {Defs}), VNs) :-
    compileStatement((instance T:C where {Defs}), VNs, []).

compileStatement((Context => Statement), VNs) :-
    tupleToList(Context, ContextAsList),
    compileStatement(Statement, VNs, ContextAsList).

compileStatement((declare Func -> Type), VNs, Context) :-
    Func =.. [Name | ArgTypes],
    assert(type_signature(Name, ArgTypes, Type, Context)),
    nameVars(VNs),
    checkTypeFlow(Func, Context, Type).

compileStatement((instance T:C where {Defs}), VNs, Context) :-
    (type_class(C, T, Decls, ClassContext) ->
        true
        ;
        throw(type_class_does_not_exist(C))),
    assert(class_instance(T, C, Context)),
    !nameVars(VNs),
    checkTypeFlow(T, Context, C),
    !validateInstance(Decls, Defs, T, C),
    compileMethods(Defs, ClassContext).

compileStatement((class T:C where {Decls}), VNs, Context) :-
    ClassCtx = [T:C | Context],
    assert(type_class(C, T, Decls, ClassCtx)),
    (var(T) ->
        true
        ;
        throw(instance_type_not_var_in_class_decl(T, C))),
    !declareClassFunctions(Decls, ClassCtx, VNs).

compileFunctionDefinition((Func := Body), TypeContext) :-
    walk(Func, replaceSingletonsWithDelete, [], _),
    applyMacros(Body, BodyAfterMacros),
    Func =.. [Name | Args],
    inferType(BodyAfterMacros, Type, BodyAssumptions),
    inferTypes(Args, ArgTypes, ArgAssumptions),
    validateLValues(Args, ArgTypes),
    sameLength(ArgTypes, SigArgTypes),
    (type_signature(Name, SigArgTypes, SigType, TypeContext) ->
        term_variables(SigArgTypes, SigArgTypeVars),
        saturateTypeVars(SigArgTypeVars),
        assertAssumptions(TypeContext),
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
    !introduceVarList(DestArgs, [], VarState1),
    !linearityCheck(Asm, Result, VarState1, _),
    unnameVars((TypeContext, DestArgs, Asm, Result), 
               (UnTypeContext, UnArgs, UnAsm, UnResult), _),
    % writeln(function_impl(Name, UnTypeContext, UnArgs, UnAsm, UnResult)),
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
formatError(type_cannot_be_inferred(Var),
    ["Type variable ", Var, " cannot be inferred in this context."]).

writeln_list(S, [First | Rest]) :-
    write(S, First),
    writeln_list(S, Rest).

writeln_list(S, []) :-
    writeln(S, "").

type_signature(*, [&T], T, [T:basic_type]).

type_signature(==, [T, T], bool, []).
constant_propagation(A::T==B::T, V::bool) :- A == B -> V = true; V = false.

type_signature(int64_plus, [int64, int64], int64, []).
constant_propagation(int64_plus(A::int64, B::int64), V::int64) :- V is A+B.

type_signature(float64_plus, [float64, float64], float64, []).
constant_propagation(float64_plus(A::float64, B::float64), V::float64) :- V is A+B.

type_signature(int64_minus, [int64, int64], int64, []).
constant_propagation(int64_minus(A::int64, B::int64), V::int64) :- V is A-B.

type_signature(float64_minus, [float64, float64], float64, []).
constant_propagation(float64_minus(A::float64, B::float64), V::float64) :- V is A-B.

type_signature(strlen, [&string], int64, []).
constant_propagation(strlen(S::(&string)), Len::int64) :- string_length(S, Len).

type_signature(strcat, [string, string], string, []).
constant_propagation(strcat(S1::string, S2::string), S::string) :- string_concat(S1, S2, S).

type_signature(delete_string, [string, T], T, []).
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
type(_::type(_)).

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
        Kind = type(_)
        ;
        throw(double_use_of_var(Name)).

inferTypeSpecial(_::T, T, []).
inferTypeSpecial(N, int64, []) :- integer(N).
inferTypeSpecial(N, float64, []) :- float(N).
inferTypeSpecial(S, string, []) :- string(S).

inferTypeSpecial(case Expr of {Branches}, OutType, Assumptions) :-
    inferCaseExprType(Expr, Branches, OutType, Assumptions).

inferTypeSpecial(_::Type, Type, []).

inferTypeSpecial(&Expr, &Type, Assumptions) :-
    my_callable(Expr),
    functor(Expr, Name, Arity),
    is_constructor(Name, Arity) ->
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

inferCaseExprType(Expr, Branches, OutType, Assumptions) :-
    !inferType(Expr, InType, ExprAssumptions),
    !validateCaseOptions(
        Branches, InType, OutType, CaseAssumptions, 0, OptionCount),
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

assertOptionSignatures(Options, Type, N0, N9) :-
    my_callable(Options),
    Options = Op1 + Op2 ->
        assertOptionSignatures(Op1, Type, N0, N1),
        assertOptionSignatures(Op2, Type, N1, N9)
        ;
        Options =.. [Name | Args],
        assert(type_signature(Name, Args, Type, [])),
        length(Args, Arity),
        assert(is_constructor(Name, Arity)),
        assert(is_option(Name/Arity, N0)),
        N9 is N0 + 1.

validateCaseOptions((Branch; Branches),
        Type, OutType, Assumptions, FirstIndex, LastIndex) :-
    !validateCaseOption(Branch, Type, OutType, Assumptions1, FirstIndex),
    NextIndex is FirstIndex + 1,
    !validateCaseOptions(
        Branches, Type, OutType, Assumptions2, NextIndex, LastIndex),
    append(Assumptions1, Assumptions2, Assumptions).

validateCaseOptions((Pattern => Expr), 
        Type, OutType, Assumptions, Index, LastIndex) :-
    !validateCaseOption((Pattern => Expr), Type, OutType, Assumptions, Index),
    LastIndex is Index + 1.

validateCaseOption((Pattern => Value), InType, OutType, Assumptions, Index) :-
    walk(Pattern, replaceSingletonsWithDelete, [], _),
    \+ \+ validateCasePattern(Pattern, Index),
    inferType(Pattern, InType, _),
    inferType(Value, ValueType, Assumptions),
    matchType(ValueType, OutType, Value).

validateCasePattern(Pattern, Index) :-
    Pattern = &RefPattern ->
        validateCasePattern(RefPattern, Index)
        ;
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

replaceSingletonsWithDelete(Var, S, S) :-
    var(Var) ->
        Var = '_'::_
        ;
        Var = _::_.

:- begin_tests(assembly).

% Numbers and strings are assembled using the literal command.
test(assemble_int) :-
    assembly(42, Val::_, [], Assembly),
    Assembly == [literal(42, Val::int64)].

test(assemble_float) :-
    assembly(42.0, Val::_, [], Assembly),
    Assembly == [literal(42.0, Val::float64)].

test(string) :-
    assembly("hello", Val::_, [], Assembly),
    Assembly == [literal("hello", Val::string)].

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
    assembly(1+2, Val::int64, [], Assembly),
    (Val, Assembly) =@= (Val,
                        [literal(1, One::int64),
                         literal(2, Two::int64),
                         call(+, [One::int64, Two::int64], [int64:plus], Val::int64)]).

% Nested functions are assembled bottom-up.
test(nested_func) :-
    assembly(1+2+3, Val::int64, [], Assembly),
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
                    [construct(foo1,[FourtyTwo::int64],X::foobar1),
                     literal(42,FourtyTwo::int64),
                     case(X::foobar1,
                        [[destruct(X::foobar1,foo1,['A'::int64]),
                         literal(1,One::int64),
                         call(==,['A'::int64,One::int64],[],V1::bool),
                         assign(V1::bool, Val)],
                       [destruct(X::foobar1,bar1,[Y::float64]),
                        literal(0,Dummy::int64),
                        call(del,[Dummy::int64,Y::float64],[float64:delete],_),
                        construct(false,[],V2::bool),
                         assign(V2::bool, Val)]])]).


% Case expression over reference types differ from normal case expressions
% in that instead of using destruct they use destruct_ref (which extracts
% the arguments but does not destroy the object), and in that deletes are
% not placed.
test(case_ref) :-
    compileStatement((union foobar2 = foo2(string) + bar2(string)), []),
    assembly((case foo2("hello") of {
        &foo2('A'::(&string)) => 'A'::(&string) == 'A'::(&string);
        &bar2('_'::(&string)) => false
    }), Val, [], Asm),
    (Val, Asm) =@= (Val,
                    [construct(foo2, [Hello::string], X::foobar2),
                     literal("hello", Hello::string),
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
assemblySpecial(*Var, *Var, Asm, Asm).

assemblySpecial((case Expr of {Branches}), Val, AsmIn, AsmOut) :-
    branchesAssembly(ExprVal, Branches, Val, destructAssembly, BranchesAsm),
    assembly(Expr, ExprVal, [case(ExprVal, BranchesAsm) | AsmIn], AsmOut).

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
            AsmOut = [construct(Name, Vals, Val) | AsmMid],
            assemblies(Args, Vals, AsmIn, AsmMid)
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
                                     [string:delete], _)]).

:- end_tests(destructAssemblies).

destructAssemblies([], [], Asm, Asm).

destructAssemblies([Arg | Args], [DstArg | DestArgs], AsmIn, AsmOut) :-
    destructAssembly(Arg, DstArg, AsmIn, AsmMid),
    destructAssemblies(Args, DestArgs, AsmMid, AsmOut).

destructAssembly(Var::Type, X::Type, AsmIn, AsmOut) :-
    Var == '_' ->
        AsmOut = [literal(0, Dummy::int64),
                  call(del, [Dummy::int64, X::Type], [Type:delete], _) 
                 | AsmIn]
        ;
        Var = X,
        AsmOut = AsmIn.

destructAssembly(&Cons, X, AsmIn, AsmOut) :-
    my_callable(Cons),
    functor(Cons, Name, Arity),
    is_constructor(Name, Arity),
    destructRefAssembly(Cons, X, AsmIn, AsmOut).

destructAssembly(Cons, X, AsmIn, AsmOut) :-
    my_callable(Cons),
    functor(Cons, Name, Arity),
    is_constructor(Name, Arity),
    Cons =.. [Name | Args],
    destructAssemblies(Args, DestArgs, AsmIn, AsmMid),
    AsmOut = [destruct(X, Name, DestArgs) | AsmMid].

destructRefAssemblies([], [], Asm, Asm).
destructRefAssemblies([Arg | Args], [DestArg | DestArgs], AsmIn, AsmOut) :-
    destructRefAssembly(Arg, DestArg, AsmIn, AsmMid),
    destructRefAssemblies(Args, DestArgs, AsmMid, AsmOut).

destructRefAssembly(VarIn::Type, VarOut::Type, Asm, Asm) :-
    VarIn == '_' ->
        true
        ;
        VarOut = VarIn.

destructRefAssembly(&Cons, Val, AsmIn, AsmOut) :-
    destructRefAssembly(Cons, Val, AsmIn, AsmOut).

destructRefAssembly(Cons, Val, AsmIn, AsmOut) :-
    my_callable(Cons),
    functor(Cons, Name, Arity),
    is_constructor(Name, Arity),
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
                (Var = *Name::Type ->
                    !stateMap(consumeVars, Name::Type, StateIn, StateOut)
                    ;
                    throw(unsupported_variable(Var))))).

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

declareClassFunctions((ContextTuple => Decl), TypeContext, VNs) :-
    tupleToList(ContextTuple, ContextAsList),
    append(TypeContext, ContextAsList, FullContext),
    declareClassFunctions(Decl, FullContext, VNs).

declareClassFunctions((Func -> Type), TypeContext, VNs) :-
    copy_term(((Func -> Type), TypeContext, VNs), (Decl, CtxCopy, VNsCopy)),
    !compileStatement(declare Decl, VNsCopy, CtxCopy).

declareClassFunctions((Decl; Decls), TypeContext, VNs) :-
    declareClassFunctions(Decl, TypeContext, VNs),
    declareClassFunctions(Decls, TypeContext, VNs).

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

fake_type(Name::_) :-
    atom(Name).

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

:- begin_tests(unnameVars).

% unnameVars reverses the operation of nameVars. While the latter works on a list of
% variable names and unifies the variables with Name::_, unnameVars walks a term,
% identifies all elements of the form Name::_, assigns a free variable to each, and
% then replaces each such element with its corresponding variable.
test(unnameVars) :-
    saturateTypeVars([FakeType]),
    unnameVars(foo('A'::int64, 'B'::_)+
               foo('A'::int64, C::string)+
               foo(C::string, _) + 
               [FakeType:seq(FakeType), _, 'T1'::type(unique7)], Unnamed, VNs),
    Unnamed =@= foo(A::int64, B::_)+
                foo(A::int64, C::string)+
                foo(C::string, _)+
                [T:seq(T), _, T1],
    Unnamed = foo(A::int64, B::_)+
                foo(A::int64, C::string)+
                foo(C::string, _)+
                [T:seq(T), _, T1],
    member('A'=A1, VNs),
    A1 == A.

:- end_tests(unnameVars).

unnameVars(Term, Unnamed, Names) :-
    copy_term(Term, Term1),
    walk(Term1, findNames, [], Names),
    replaceInTerm(Term1, replaceNamed(Names), Unnamed).

addMappingIfNotThere(Name, MappingIn, MappingOut) :-
    atom(Name) ->
        (member(Name=_, MappingIn) ->
    	    MappingOut = MappingIn
            ;
            MappingOut = [Name=_ | MappingIn])
        ;
        MappingOut = MappingIn.
    
findNames(Term, StateIn, StateOut) :-
    nonvar(Term),
    (Term = Name::Type ->
         (Type = type(Unique), atom(Unique) ->
	     addMappingIfNotThere(Name, StateIn, StateOut)
	     ;
	     addMappingIfNotThere(Name, StateIn, StateMid),
	     walk(Type, findNames, StateMid, StateOut))
         ;
         fake_type(Term) ->
             addMappingIfNotThere(Term, StateIn, StateOut)
             ;
             fail).

replaceNamed(Names, Term, Unnamed) :-
    var(Term) ->
	Unnamed = Term
        ;
        Term = Name::Type ->
            (Type = type(Unique), atom(Unique) ->
	        member(Name=Unnamed, Names)
	        ;
                !replaceInTerm(Type, replaceNamed(Names), TypeRep),
                (var(Name) ->
                        Unnamed = Name::TypeRep
                        ;
                        !member(Name=Var, Names),
                        Unnamed = Var::TypeRep))
            ;
            fake_type(Term) ->
                !member(Term=Unnamed, Names)
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
    specialize([literal(42, FortyTwo::int64),
                literal("hello", Hello::string)], Asm),
    FortyTwo == 42,
    Hello == "hello",
    Asm == [].

% Construct commands construct compound terms based on their arguments.
test(construct) :-
    specialize([construct(foo, [1, 2], X::footype),
                construct(bar, [X, 3], Y::bartype)], Asm),
    Y == bar(foo(1, 2), 3),
    Asm == [].

% Destruct works by destructing compound terms into their components.
test(destruct) :-
    specialize([destruct(bar(foo(1::int64, 2::int64)::footype, 3::int64)::bartype, bar, [Foo::footype, Three::int64]),
                destruct(Foo::footype, foo, [One::int64, Two::int64])], Asm),
    One == 1,
    Two == 2,
    Three == 3,
    Asm == [].

% destruct_ref works the same way.
test(destruct_ref) :-
    specialize([destruct_ref(bar(foo(1::int64, 2::int64)::footype, 3::int64)::(&bartype),
                             bar, [Foo::(&footype), Three::(&int64)]),
                destruct_ref(Foo::(&footype), foo, [One::(&int64), Two::(&int64)])], Asm),
    One == 1,
    Two == 2,
    Three == 3,
    Asm == [].

% If given a free variable, destruct remains unevaluated, but allows evaluation continue.
test(destruct_free_var) :-
    specialize([destruct(X::footype, foo, [Y::int64, Z::int64]),
                literal(3, Three::int64)], Asm),
    Three = 3,
    Asm = [destruct(X::footype, foo, [Y::int64, Z::int64])].

% Calls to implemented functions are replaced with their bodies.
test(call_implemented) :-
    specialize([call(+, [A, B], [int64:plus], C),
                call(+, [C, D], [int64:plus], R)], Asm),
    Asm == [call(int64_plus, [A, B], [], C),
            call(int64_plus, [C, D], [], R)].

% When calling a built-in function that has a Prolog implementation with all arguments
% ground (no free variables), the Prolog implementation is used to evaluate the result (constant propagation)
test(constant_propagation) :-
    specialize([call(+, [1::int64, 2::int64], [int64:plus], X::int64),
                call(+, [X::int64, 3::int64], [int64:plus], R)], Asm),
    Asm == [],
    R == 6::int64.

% A case command is not specialized if the expression it destructs is a free variable.
% In such a case, the command is left untouched, but further commands are specialized.
test(case_with_free_var) :-
    specialize([
        case(X, [
            [destruct(X, foo, [A, B]),
            construct(bar, [B, A], Res)],
            [destruct(X, bar, [X, Y]),
            construct(foo, [Y, X], Res)]]),
        literal(2, Two::int64)], Asm),
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
            [destruct(bar(X1, Y1)::bartype, foo, [A, B]),
            construct(bar, [B, A], Res::bartype)],
            [destruct(bar(X1, Y1)::bartype, bar, [X, Y]),
            construct(foo, [Y, X], Res::footype)]]),
        literal(2, Two::int64)], Asm),
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
specialize([literal(Val, Val::_) | AsmIn], AsmOut) :-
    trace(AsmIn),
    !specialize(AsmIn, AsmOut).
specialize([construct(Name, Args, Term::_) | AsmIn], AsmOut) :-
    Term =.. [Name | Args],
    trace(AsmIn),
    !specialize(AsmIn, AsmOut).
specialize([destruct(Term::Type, Name, DerefArgs) | AsmIn], AsmOut) :-
    var(Term) ->
        AsmOut = [destruct(Term::Type, Name, DerefArgs) | AsmMid],
        trace(AsmIn),
        !specialize(AsmIn, AsmMid)
        ;
        Term =.. [Name | Args],
        !makeDerefTypes(Args, DerefArgs),
        trace(AsmIn),
        !specialize(AsmIn, AsmOut).
specialize([destruct_ref(Term::(&Type), Name, RefArgs) | AsmIn], AsmOut) :-
    var(Term) ->
        AsmOut = [destruct_ref(Term::(&Type), Name, RefArgs) | AsmMid],
        trace(AsmIn),
        !specialize(AsmIn, AsmMid)
        ;
        Term =.. [Name | Args],
        !makeRefTypes(Args, RefArgs),
        trace(AsmIn),
        !specialize(AsmIn, AsmOut).
specialize([call(Name, Args, TypeGuard, Result) | AsmIn], AsmOut) :-
    removeRefs(Args, ArgsNoRefs),
    (function_impl(Name, TypeGuard, ArgsNoRefs, Body, Result) ->
        append(Body, AsmIn, Asm1),
        trace(Asm1),
        !specialize(Asm1, AsmOut)
        ;
        forall(member(ArgNoRefs::_, ArgsNoRefs), ground(ArgNoRefs)),
        Func =.. [Name | ArgsNoRefs],
        constant_propagation(Func, Result) ->
            trace(AsmIn),
            !specialize(AsmIn, AsmOut)
            ;
            AsmOut = [call(Name, ArgsNoRefs, TypeGuard, Result) | Asm2],
            trace(AsmIn),
            !specialize(AsmIn, Asm2)).
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
    InstanceDef = (instance Union1 : delete where {
        X del U := case U of {
            Branches
        }
    }),
    compileStatement(InstanceDef, ['x'=X, 'u'=U | VNs]).

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

enumerateVars(Var::_Type, N, N1) :-
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
    InstanceDef =@= (T1 : plus => instance lambda1(T1) : (T1 -> T1) where {
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
    inferTypeClasses(Assumptions),
    once(lambdaTypesAndArgs(X, YAfterMacros, Types, Args)),
    LambdaStructSig =.. [LambdaName | Types],
    term_variables([Types, Tx, Ty], TypeVars),
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
    \+ \+checkAssumption(T:C) ->
        filterMetAssumptions(Assum, AssumOut)
        ;
        AssumOut = [T:C | AssumMid],
        filterMetAssumptions(Assum, AssumMid).


findVars(Name::Type, State, [Name::Type | State]).

syntacticMacro((X->Y), Replacement) :-
    lambdaMacro(X, Y, =, '->', '!', Replacement).

syntacticMacro((X@>Y), Replacement) :-
    lambdaMacro(X, Y, makeRef, '@>', '@', Replacement).

lambdaMacro(X, Y, TypeModifier, ClassName, MethodName, Replacement) :-
    !extractLambda(X, Y, TypeModifier, ClassName, MethodName,
        StructDef, InstanceDef, Replacement),
    !unnameVars(StructDef, StructDef1, StructDefVNs),
    replaceInTerm(StructDef1, removeTypes, StructDef2),
    !compileStatement(StructDef2, StructDefVNs),
    !unnameVars(InstanceDef, InstanceDef1, InstanceDefVNs),
    replaceInTerm(InstanceDef1, removeTypes, InstanceDef2),
    !compileStatement(InstanceDef2, InstanceDefVNs).

removeTypes(Var::_, Var).

removeRefs([], []).
removeRefs([Arg | Args], [ArgNoRefs | ArgsNoRefs]) :-
    (nonvar(Arg), Arg = &Var::Type ->
        ArgNoRefs = Var::(&Type)
        ;
        nonvar(Arg), Arg = *Var::(&Type) ->
            ArgNoRefs = Var::Type
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

checkTypeFlow(Func, Context, Type) :-
    walk(Func, collectTypeVars, [], VarsInFunc),
    checkAssumptionFlow(Context, VarsInFunc, VarsInFuncAndContext),
    walk(Type, checkTypeVarsOnlyIn(VarsInFuncAndContext), [], _).

collectTypeVars(Var::type(Unique), Vars, [Var | Vars]) :-
    atom(Var),
    gensym(unique, Unique).

checkTypeVarsOnlyIn(VarsInFunc, Var::type(_), State, State) :-
    atom(Var),
    \+member(Var, VarsInFunc),
    throw(type_cannot_be_inferred(Var)).

checkAssumptionFlow([], Vars, Vars).
checkAssumptionFlow([T:C | Context], VarsIn, VarsOut) :-
    walk(T, checkTypeVarsOnlyIn(VarsIn), [], _),
    walk(C, collectTypeVars, VarsIn, VarsWithClass),
    checkAssumptionFlow(Context, VarsWithClass, VarsOut).

saturateTypeVars([]).
saturateTypeVars([Var | Vars]) :-
    gensym(unknown_type, Var),
    assert(fake_type(Var)),
    saturateTypeVars(Vars).

assertAssumptions([]).
assertAssumptions([T:C | Assumptions]) :-
    (fake_type(T) ->
        term_variables(C, CVars),
        saturateTypeVars(CVars),
        assert(class_instance(T, C, []))
        ;
        !class_instance(T, C, InstanceAssumptions),
        assertAssumptions(InstanceAssumptions)),
    assertAssumptions(Assumptions).

makeRefTypes([], []).
makeRefTypes([Arg::Type | Args], [Arg::(&Type) | RefArgs]) :-
    makeRefTypes(Args, RefArgs).

makeDerefTypes([], []).
makeDerefTypes([Arg::Type | Args], [Arg::Type | RefArgs]) :-
    makeDerefTypes(Args, RefArgs).
makeDerefTypes([*Arg::(&Type) | Args], [Arg::Type | RefArgs]) :-
    makeDerefTypes(Args, RefArgs).

:- begin_tests(normalizeAssembly).

% destruct() and destruct_ref() are not affected by normalizeAssembly.
test(ignoring_destruct) :-
    !normalizeAssembly(
        [destruct(Foo::footype, foo, [X::int64, Y::int64]),
         destruct_ref(Bar::(&bartype), bar, [Z::(&int64), S::(&string)])], Asm),
    Asm == [destruct(Foo::footype, foo, [X::int64, Y::int64]),
            destruct_ref(Bar::(&bartype), bar, [Z::(&int64), S::(&string)])].

% call() with a non-variable terms as arguments is preceded with construction 
% of the term.
test(call) :-
    !normalizeAssembly(
        [call(+, [2::int64, _::int64], [int64:plus], X::int64),
         destruct(_::footype, foo, [X::int64, _::int64])], Asm),
    Asm =@= [literal(2, Two::int64),
             call(+, [Two::int64, _::int64], [int64:plus], X1::int64),
             destruct(_::footype, foo, [X1::int64, _::int64])].

% For case() we process each branch.
test(case) :-
    !normalizeAssembly(
        [case(_::bartype, [
            [call(+, [2::int64, _::int64], [int64:plus], X::int64),
            destruct(_::footype, foo, [X::int64, _::int64])],
            [call(+, [3::int64, _::int64], [int64:plus], X::int64),
            destruct(_::footype, foo, [X::int64, _::int64])]
        ])], Asm),
    Asm =@= [case(_::bartype, [
                [literal(2, Two::int64),
                 call(+, [Two::int64, _::int64], [int64:plus], X::int64),
                 destruct(_::footype, foo, [X::int64, _::int64])],
                [literal(3, Three::int64),
                 call(+, [Three::int64, _::int64], [int64:plus], X::int64),
                 destruct(_::footype, foo, [X::int64, _::int64])]
            ])].

:- end_tests(normalizeAssembly).

normalizeAssembly([], []).
normalizeAssembly([Cmd | AsmIn], AsmOut) :-
    (Cmd = call(Name, Args, Guard, Ret) ->
        splitTypes(Args, ArgsNoTypes, ArgTypes),
        splitTypes(Vals, _, ArgTypes),
        !assemblies(ArgsNoTypes, Vals,
            [call(Name, Vals, Guard, Ret)], CallAsm),
        !append(CallAsm, AsmMid, AsmOut)
        ;
        Cmd = case(Cond, Branches) ->
            normalizeBranches(Branches, NormBranches),
            AsmOut = [case(Cond, NormBranches) | AsmMid]
            ;
            AsmOut = [Cmd | AsmMid]),
    !normalizeAssembly(AsmIn, AsmMid).

splitTypes([], [], []).
splitTypes([Arg::Type | Args], [Arg | ArgsNoTypes], [Type | ArgTypes]) :-
    splitTypes(Args, ArgsNoTypes, ArgTypes).

normalizeBranches([], []).
normalizeBranches([Branch | Branches], [NormBranch | NormBranches]) :-
    normalizeAssembly(Branch, NormBranch),
    normalizeBranches(Branches, NormBranches).

:- begin_tests(microAsm).

% Micro-assembly is a language similar to our assembly language, in which memory
% operations (construct, destruct and destruct_ref) are broken down into smaller
% pieces, namely allocate() and deallocate() for memory management, and field()
% and read_field() to access individual fields.

% Calls & literals are unaffected by microAsm.
test(unaffected) :-
    microAsm([literal(2, Two::int64),
              call(+, [Two::int64, X::int64], [int64:plus], XPlusTwo::int64)], UAsm),
    UAsm == [literal(2, Two::int64),
             call(+, [Two::int64, X::int64], [int64:plus], XPlusTwo::int64)].

% construct() of a struct with one or more elements is replaced by allocate() with 
% the number of fields, and the fields themselves are bound with field() with the
% respective offset of each field.
test(construct_struct) :-
    microAsm([construct(',', [X::int64, S::string], _::(int64, string)),
              literal(7, X::int64),
              literal("hello", S::string)], UAsm),
    UAsm =@= [allocate(2, Struct::(int64, string)),
              literal(7, field(Struct::(int64, string), 0)::int64),
              literal("hello", field(Struct::(int64, string), 1)::string)].

% construct of a union type with one or more elements is replaced with allocate() with
% the number of fields + 1 (for the option number), and an assignment of field 0 to the
% option number. Fields are then numbered starting at 1.
test(construct_option) :-
    microAsm([construct(just, [S::string], _::maybe(string)),
              literal("hello", S::string)], UAsm),
    UAsm =@= [allocate(2, Struct::maybe(string)),
              literal(0, field(Struct::maybe(string), 0)::int64),
              literal("hello", field(Struct::maybe(string), 1)::string)].

% Constructing an option of arity zero assigns a sentinel with the value of the option.
test(construct_empty_option) :-
    microAsm([construct(none, [], None::maybe(string)),
              call(==, [None::maybe(string), _::maybe(string)], [], _::bool)], UAsm),
    UAsm =@= [assign_sentinel(1, None1::maybe(string)),
              call(==, [None1::maybe(string), _::maybe(string)], [], _::bool)].

% destruct_ref of a struct maps to a sequence of read_field() commands starting with
% offset 0.
test(destruct_ref_struct) :-
    microAsm([destruct_ref(_::(&((int64, string))), ',', [A::(&int64), S::(&string)]),
              call(foo, [A::(&int64), S::(&string)], [], _::bool)], UAsm),
    UAsm =@= [read_field(Tuple::(&((int64,string))), 0, A1::(&int64)),
              read_field(Tuple::(&((int64,string))), 1, S1::(&string)),
              call(foo, [A1::(&int64), S1::(&string)], [], _::bool)].

% destruct_ref of a union option maps to a sequence of read_field() commands starting
% with offset 1 (as offset 0 is reserved for the option indicator).
test(destruct_ref_option) :-
    microAsm([destruct_ref(_::(&maybe(string)), just, [S::(&string)]),
              call(foo, [S::(&string)], [], _::bool)], UAsm),
    UAsm =@= [read_field(_::(&maybe(string)), 1, S1::(&string)),
              call(foo, [S1::(&string)], [], _::bool)].

% destruct of a struct maps to a sequence of read_field() commands starting at 0,
% followed by a deallocate() command with the arity.
test(destruct_struct) :-
    microAsm([destruct(_::(&((int64, string))), ',', [A::(&int64), S::(&string)]),
              call(foo, [A::(&int64), S::(&string)], [], _::bool)], UAsm),
    UAsm =@= [read_field(Tuple::(&((int64,string))), 0, A1::(&int64)),
              read_field(Tuple::(&((int64,string))), 1, S1::(&string)),
              deallocate(Tuple::(&((int64,string))), 2),
              call(foo, [A1::(&int64), S1::(&string)], [], _::bool)].

% destruct of a non-empty union option maps to a sequence of read_field() commands
% starting at 1, followed by a deallocate() with the arity + 1.
test(destruct_option) :-
    microAsm([destruct(_::(&maybe(string)), just, [S::(&string)]),
              call(foo, [S::(&string)], [], _::bool)], UAsm),
    UAsm =@= [read_field(Maybe::(&maybe(string)), 1, S1::(&string)),
              deallocate(Maybe::(&maybe(string)), 2),
              call(foo, [S1::(&string)], [], _::bool)].

% destruct of an empty union option is ignored.
test(destruct_empty_option) :-
    microAsm([destruct(_::(&maybe(string)), none, [])], UAsm),
    UAsm =@= [].

% In case commands each branch is converted separately. assign commands are removed
% and the result variables are bound together.
test(case) :-
    microAsm([case(M::(&maybe(string)),
                [[destruct(M::(&maybe(string)), just, [S::(&string)]),
                  call(foo, [S::(&string)], [], X::bool),
                  assign(X::bool, Result::bool)],
                 [destruct(M::(&maybe(string)), none, []),
                  construct(true, [], True::bool),
                  assign(True::bool, Result::bool)]]),
              literal(3, _::int64)], UAsm),
    UAsm =@= [case(M1::(&maybe(string)),
                [[read_field(M1::(&maybe(string)), 1, S1::(&string)),
                  deallocate(M1::(&maybe(string)), 2),
                  call(foo, [S1::(&string)], [], Result1::bool)],
                 [assign_sentinel(0,Result1::bool)]]),
              literal(3, _::int64)].


:- end_tests(microAsm).

microAsm([], []).
microAsm([Cmd | Asm], UAsm) :-
    (Cmd = construct(Name, Args, Val) ->
        once(constructMicroAsm(Name, Args, Val, ConsUAsm)),
        append(ConsUAsm, RestUAsm, UAsm)
        ;
        Cmd = destruct(Val, Name, Args) ->
            once(destructMicroAsm(Val, Name, Args, DestUAsm)),
            append(DestUAsm, RestUAsm, UAsm)
            ;
            Cmd = destruct_ref(Val, Name, Args) ->
                once(destructRefMicroAsm(Val, Name, Args, DestUAsm)),
                append(DestUAsm, RestUAsm, UAsm)
                ;
                Cmd = case(Val, Branches) ->
                    branchesMicroAssembly(Branches, BranchesUAsm),
                    UAsm = [case(Val, BranchesUAsm) | RestUAsm]
                    ;
                    Cmd = assign(A, A) ->
                        UAsm = RestUAsm
                        ;
                        UAsm = [Cmd | RestUAsm]),
    microAsm(Asm, RestUAsm).

constructMicroAsm(Name, Args, Struct, [allocate(Arity, Struct)]) :-
    length(Args, Arity),
    is_struct(Name/Arity),
    assignFields(Args, Struct, 0).

constructMicroAsm(Name, Args, Struct, [allocate(ArityPlusOne, Struct),
                                       literal(N, field(Struct, 0)::int64)]) :-
    length(Args, Arity),
    is_option(Name/Arity, N),
    Arity > 0,
    ArityPlusOne is Arity + 1,
    assignFields(Args, Struct, 1).

constructMicroAsm(Name, [], Struct, [assign_sentinel(N, Struct)]) :-
    is_option(Name/0, N).

destructMicroAsm(Val, Name, Args, DestUAsm) :-
    length(Args, Arity),
    is_struct(Name/Arity),
    readFieldsMicroAsm(Args, Val, 0, ReadFields),
    append(ReadFields, [deallocate(Val, Arity)], DestUAsm).

destructMicroAsm(Val, Name, Args, DestUAsm) :-
    length(Args, Arity),
    Arity > 0,
    is_option(Name/Arity, _),
    readFieldsMicroAsm(Args, Val, 1, ReadFields),
    ArityPlusOne is Arity + 1,
    append(ReadFields, [deallocate(Val, ArityPlusOne)], DestUAsm).

destructMicroAsm(_, Name, [], []) :-
    is_option(Name/0, _).

destructRefMicroAsm(Val, Name, Args, DestUAsm) :-
    length(Args, Arity),
    is_struct(Name/Arity),
    readFieldsMicroAsm(Args, Val, 0, DestUAsm).

destructRefMicroAsm(Val, Name, Args, DestUAsm) :-
    length(Args, Arity),
    is_option(Name/Arity, _),
    readFieldsMicroAsm(Args, Val, 1, DestUAsm).

assignFields([], _, _).
assignFields([field(Struct, N)::_ | Args], Struct, N) :-
    N1 is N + 1,
    assignFields(Args, Struct, N1).

readFieldsMicroAsm([], _, _, []).
readFieldsMicroAsm([Arg | Args], Val, N, [read_field(Val, N, Arg) | DestUAsm]) :-
    N1 is N + 1,
    readFieldsMicroAsm(Args, Val, N1, DestUAsm).

branchesMicroAssembly([], []).
branchesMicroAssembly([Branch | Branches], [BranchUAsm | BranchesUAsm]) :-
    microAsm(Branch, BranchUAsm),
    branchesMicroAssembly(Branches, BranchesUAsm).

:- begin_tests(reuseSpace).

% The reuseSpace optimization removes deallocations followed by allocations of
% memory of the same size. In shuch cases, the pointer can simply be reused.

% Micro-assembly code that does not include matching allocation and deallocation
% operations is left unchanged.
test(untouched) :-
    reuseSpace([allocate(3, Foo:footype),
                call(bar, [Foo::footype], Bar::bartype),
                deallocate(Foo::footype, 3)], UAsm),
    UAsm == [allocate(3, Foo:footype),
             call(bar, [Foo::footype], Bar::bartype),
             deallocate(Foo::footype, 3)].

% Code that has a deallocation followed (not necessarily directly) by a matching
% allocation of the same size is removed along with the allocation. The variables
% containing both allocations are unified.
test(remove_allocation) :-
    reuseSpace([allocate(3, Foo:footype),
                call(bar, [Foo::footype], Bar::bartype),
                deallocate(Foo::footype, 3),
                literal(3, _::int64),
                allocate(3, Bar::bartype),
                literal("hello", field(Bar::bartype, 2)::string)], UAsm),
    UAsm =@= [allocate(3, Common:footype),
              call(bar, [Common::footype], Common::bartype),
              literal(3, _::int64),
              literal("hello", field(Common::bartype, 2)::string)].


% In a case command, each branch is considered by itself.
test(case) :-
    reuseSpace([case(X::footype, [
                [deallocate(X::footype, 3),
                 read_field(X::footype, 1, _::int64),
                 allocate(3, Bar::bartype),
                 literal("hello", field(Bar::bartype, 2)::string)],
                [deallocate(X::footype, 2),
                 read_field(X::footype, 1, _::int64),
                 allocate(2, Bar::bartype),
                 literal("world", field(Bar::bartype, 1)::string)]]),
                literal(4, _::int64)], UAsm),
    UAsm =@= [case(X1::footype, [
                [read_field(X1::footype, 1, _::int64),
                 literal("hello", field(X1::bartype, 2)::string)],
                [read_field(X1::footype, 1, _::int64),
                 literal("world", field(X1::bartype, 1)::string)]]),
                literal(4, _::int64)].
:- end_tests(reuseSpace).

reuseSpace([], []).
reuseSpace([Cmd | UAsmIn], UAsmOut) :-
    Cmd = deallocate(Alloc, Size) ->
        once(reuseSpace(UAsmIn, Alloc, Size, UAsmOut))
        ;
        Cmd = case(Expr, Branches) ->
            reuseSpaceInBranches(Branches, OptBranches),
            UAsmOut = [case(Expr, OptBranches) | UAsmMid],
            reuseSpace(UAsmIn, UAsmMid)
            ;
            UAsmOut = [Cmd | UAsmMid],
            reuseSpace(UAsmIn, UAsmMid).

reuseSpace([], Alloc, Size, [deallocate(Alloc, Size)]).
reuseSpace([allocate(Size, Var::_) | UAsmIn], Var::_, Size, UAsmOut) :-
    reuseSpace(UAsmIn, UAsmOut).
reuseSpace([Cmd | UAsmIn], Alloc, Size, [Cmd | UAsmOut]) :-
    reuseSpace(UAsmIn, Alloc, Size, UAsmOut).

reuseSpaceInBranches([], []).
reuseSpaceInBranches([Branch | Branches], [OptBranch | OptBranches]) :-
    reuseSpace(Branch, OptBranch),
    reuseSpaceInBranches(Branches, OptBranches).

:- begin_tests(reuseData).

% The reuseData optimization removes read_field() commands which read a field to itself.
% Such commands can be a result of destructing something and immediately afterwards
% constructing the same type of object, with some of the fields having the same values.
% In such cases, the reuseSpace optimization reuses the object and this optimization
% removes the need to copy values to themselves.

% reuseData removes micro-assembly commands which read a field to itself.
test(read_field) :-
    reuseData([literal("hello", S::string),
               read_field(Foo::footype, 1, field(Foo::footype, 1)::int64),
               read_field(Foo::footype, 1, field(_::footype, 1)::int64),
               read_field(Foo::footype, 1, field(Foo::footype, 2)::int64),
               read_field(Foo::footype, 2, field(Foo::footype, 2)::int64)], UAsm),
    UAsm =@= [literal("hello", S::string),
                read_field(Foo::footype, 1, field(_::footype, 1)::int64),
                read_field(Foo::footype, 1, field(Foo::footype, 2)::int64)].

% The case command applies this optimization on each branch.
test(case) :-
    reuseData([case(_::footype, [
               [read_field(Foo::footype, 1, field(Foo::footype, 1)::int64),
                read_field(Foo::footype, 1, field(_::footype, 1)::int64)],
               [read_field(Foo::footype, 1, field(Foo::footype, 2)::int64),
                read_field(Foo::footype, 2, field(Foo::footype, 2)::int64)]]),
               literal("hello", _::string)], UAsm),
    UAsm =@= [case(_::footype, [
               [read_field(Foo1::footype, 1, field(_::footype, 1)::int64)],
               [read_field(Foo1::footype, 1, field(Foo1::footype, 2)::int64)]]),
               literal("hello", _::string)].
:- end_tests(reuseData).

reuseData([], []).
reuseData([Cmd | UAsmIn], UAsmOut) :-
    Cmd = read_field(Obj1, N1, field(Obj2, N2)::_),
    (Obj1, N1) == (Obj2, N2) ->
        reuseData(UAsmIn, UAsmOut)
        ;
        Cmd = case(Expr, Branches) ->
            reuseDataInBranches(Branches, OptBranches),
            UAsmOut = [case(Expr, OptBranches) | UAsmMid],
            reuseData(UAsmIn, UAsmMid)
            ;
            UAsmOut = [Cmd | UAsmMid],
            reuseData(UAsmIn, UAsmMid).

reuseDataInBranches([], []).
reuseDataInBranches([Branch | Branches], [OptBranch | OptBranches]) :-
    reuseData(Branch, OptBranch),
    reuseDataInBranches(Branches, OptBranches).

:- begin_tests(backend).

test(list_sum) :-
    !compileStatement((declare list_sum(list(int64)) -> int64), []),
    !compileStatement((list_sum(L) := case L of {
        [] => 0;
        [N | Ns] => N + list_sum(Ns)
    }), ['L'=L, 'N'=N, 'Ns'=Ns]),
    !function_impl(list_sum, [], [L1::list(int64)], Asm, _::int64),
    !microAsm(Asm, UAsm1),
    !reuseSpace(UAsm1, UAsm2),
    !reuseData(UAsm2, UAsm),
    UAsm =@= [case(L1::list(int64),[
                [literal(0,Res::int64)],
                [read_field(L1::list(int64),1,N1::int64),
                read_field(L1::list(int64),2,Ns1::list(int64)),
                call(list_sum,[Ns1::list(int64)],[],SumOfNs::int64),
                call(+,[N1::int64,SumOfNs::int64],[int64:plus],Res::int64),
                deallocate(L1::list(int64),3)]])].

test(increment_list) :-
    !compileStatement((declare increment_list(list(int64)) -> list(int64)), []),
    !compileStatement((increment_list(L) := case L of {
        [] => [];
        [N | Ns] => [N + 1 | increment_list(Ns)]
    }), ['L'=L, 'N'=N, 'Ns'=Ns]),
    !function_impl(increment_list, [], [L1::list(int64)], Asm, _::list(int64)),
    !microAsm(Asm, UAsm1),
    !reuseSpace(UAsm1, UAsm2),
    !reuseData(UAsm2, UAsm),
    writeln(UAsm),
    UAsm =@= [case(L1::list(int64),[
                [assign_sentinel(0,L1::list(int64))],
                [read_field(L1::list(int64),1,N1::int64),
                 read_field(L1::list(int64),2,Ns1::list(int64)),
                 literal(1,field(L1::list(int64),0)::int64),
                 literal(1,One::int64),
                 call(+,[N1::int64,One::int64],[int64:plus],
                    field(L1::list(int64),1)::int64),
                 call(increment_list,[Ns1::list(int64)],[],
                    field(L1::list(int64),2)::list(int64))]])].

:- end_tests(backend).

% ============= Prelude =============
:- compileStatement((class T : delete where { X del T -> X }),
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
:- compileStatement((class F:(T1->T2) where { F!T1 -> T2 }),
    ['T1'=T1, 'T2'=T2, 'F'=F]).
:- compileStatement((class F:(T1@>T2) where { &F@T1 -> T2 }),
    ['T1'=T1, 'T2'=T2, 'F'=F]).
:- compileStatement((F : (T1 -> T2) => declare let(T1, F) -> T2),
    ['T1'=T1, 'T2'=T2, 'F'=F]).
:- compileStatement((let(V, Fn) := Fn!V), ['V'=V, 'Fn'=Fn]).
