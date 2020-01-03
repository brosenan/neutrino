# DSL Example

This is an example for a DSL defined using [deep embedding](https://alessandrovermeulen.me/2013/07/13/the-difference-between-shallow-and-deep-embedding/). The DSL's abstract syntax is defined as a union type (`expr`). The concrete syntax for this DSL is defined using operator overloading (instantiating of the `plus` and `minus` type classes to provide support for the `+` and `-` operators).

The semantics of this DLS is defined using functions. First, we define the `eval` function, which takes an `expr` value and a list of key-value pairs assigning values to the variables in the expression. `eval` returns the expression's value if all variables had an assignment, or `none` otherwise.

Then, we define the `solve_for` function, which solves the equation `Expr=0` for that variable. If, for example, our expression is based on variables `x`, `y` and `z`, we can refer to the expression as `f(x, y, z)`. Given `f(x, y, z)` as the expression and `x` as the variable name to solve for, `solve_for` will return `g(y, z)` such that `f(g(y, z), y, z) = 0`.

To do so, we make the assumption that the variable we solve for (e.g., `x`), appears only once in the expression.

The following compiles successfully:

```prolog
% This is the abstract syntax of our DSL. An expression can be either a constant,
% a variable, a sum of two expressions, a subtraction of two expressions or zero.
union expr(T) = c(T)
              + v(string)
              + plus(expr(T), expr(T))
              + minus(expr(T), expr(T))
              + zero.

% Look-up a variable in a list of key-value pairs.
declare lookup(&list((string, float64)), &string) -> maybe(float64).

lookup(L, Key) := case L of & {
    [] => none;
    [&((K1, Val)) | Rest] => if(K1 == Key,
				just(*Val),
				lookup(Rest, Key))
}.

% This is done as convenience, to allow adding two values of `maybe(T)`,
% for `T` that can be added.
T : plus, T : delete =>
instance maybe(T) : plus where {
  A+B := case A of {
    just(A1) => case B of {
      just(B1) => just(A1+B1);
      none => none del A1
    };
    none => none del B
  }
}.

% Also for convenience, allowing to subtract two `maybe`s.
T : minus, T : delete =>
instance maybe(T) : minus where {
  A-B := case A of {
    just(A1) => case B of {
      just(B1) => just(A1-B1);
      none => none del A1
    };
    none => none del B
  }
}.

% This function defines the semantics of the DSL. Given an expression
% and a list of variable bindings, this function returns the value, or none
% if one or more variables are not defined in the bindings.
declare eval(&expr(float64), &list((string,float64))) -> maybe(float64).
eval(Expr, Bindings) := case Expr of & {
    c(N) => just(*N);
    v(Var) => lookup(Bindings, Var);
    plus(A, B) => eval(A, Bindings) + eval(B, Bindings);
    minus(C, D) => eval(C, Bindings) - eval(D, Bindings);
    zero => just(0.0)
}.

% Operator overloading. We define expr(T) to be an instance of plus to allow
% applying the + operator on expressions.
instance expr(T) : plus where {
  A+B := plus(A, B)			   
}.

% The same thing, with the - operator.
instance expr(T) : minus where {
  A-B := minus(A, B)			   
}.

% Now we test evaluation in action.
assert let << {
    Expr := v("X") + c(2.0) - v("Y");
    Bindings := [("X", 3.0), ("Y", 4.0)];
    eval(&Expr, &Bindings) == just(1.0) del Expr, Bindings
}.

A\/B := if(A, true, B).
A/\B := if(A, B, false).
\+A := if(A, false, true).

% To be able to solve equations we need to determine if a certain sub-expression
% depends on a given variable. This function does this.
declare depends_on(&expr(T), &string) -> bool.
depends_on(Expr, Var) := case Expr of & {
    c(_) => false;
    v(Var1) => Var == Var1;
    plus(A, B) => depends_on(A, Var) \/ depends_on(B, Var);
    minus(C, D) => depends_on(C, Var) \/ depends_on(D, Var);
    zero => false
}.

assert let << {
    Expr := v("X") + c(2.0) - v("Y");
    Var := "X";
    depends_on(&Expr, &Var) del Var, Expr
}.

assert let << {
    Expr := v("X") + c(2.0) - v("Y");
    Var := "T";
    \+depends_on(&Expr, &Var) del Var, Expr
}.

% Solving equations. This function takes two expressions representing the two sides
% of an equation, and a variable name assumed to be on the left-hand side (Expr1).
% It returns the right-hand side of the equation given that all that is left on the
% left-hand side is the variable we solve for.
declare solve_for(expr(T), expr(T), &string) -> expr(T).
solve_for(Expr1, Expr2, Var) := case Expr1 of {
  c(N) => minus(Expr2, c(N));
  v(V) => if(&V == Var,
             Expr2 del V,
             minus(Expr2, v(V)));
  plus(A, B) => if(depends_on(&A, Var),
                   solve_for(A, minus(Expr2, B), Var),
                   solve_for(B, minus(Expr2, A), Var));
  minus(C, D) => if(depends_on(&C, Var),
                    solve_for(C, plus(Expr2, D), Var),
                    solve_for(D, minus(C, Expr2), Var));
  zero => zero del Expr2 % Well, this cannot really happen.
}.

% This version of solve_for is the one users are expected to use.
% It takes one expression (not two), representing an equation of the form Expr = 0.
declare solve_for(expr(T), &string) -> expr(T).
solve_for(Expr, Var) := solve_for(Expr, zero, Var).

assert let << {
    Var := "x";
    Solved := solve_for(v("x") + v("y") - c(2.0), &Var) del Var;
    Context := [("y", 3.0)];
    eval(&Solved, &Context) == just(-1.0) del Solved, Context
}.
```

While this example is simple, it shows how simple DSL definition is in Neutrino. In that, Neutrino is not much different than other functional programming languages. However, the place where Neutrino really shines is in hiding a DSL's overhead.

The DSL's implementation consists of a case expression matching over all possible values. Doing this at runtime takes time. This is the DSL's implementation overhead. However, Neutrino performs partial evaluation over its code, trying to execute as much as this logic as possible at compile time.

Consider a situation when a constant expression is used, and the only things that change at run-time are the values set to the `Bindings`. In such a situation all the choices in the case expression are known at compile time, and will therefore be evaluated by the compiler. The only instruction left for run-time are the application of the arithmetic operations themselves.

Even if the expression we build is the result of solving an equation, as long as everything leading to the solution is known at compile-time, the equation will be solved by the compiler (the compiler will evaluate `solve_for`), and again, the only thing left to perform at run-time would be applying the arithmetic operators.

This is an example of how Neutrino can allow for programming at a very high-level (e.g., performing symbolic manipulation of expression), while enjoying the efficiency of a low-level language.