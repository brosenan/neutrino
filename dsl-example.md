# DSL Example

The following compiles successfully:

```prolog
union expr(T) = c(T)
              + v(string)
              + plus(expr(T), expr(T))
              + minus(expr(T), expr(T)).
              
declare lookup(&list((string, float64)), &string) -> maybe(float64).

lookup(L, Key) := case L of & {
    [] => none;
    [&((K1, Val)) | Rest] => if(K1 == Key,
				just(*Val),
				lookup(Rest, Key))
}.

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

declare eval(&expr(float64), &list((string,float64))) -> maybe(float64).
eval(Expr, Ctx) := case Expr of & {
    c(N) => just(*N);
    v(Var) => lookup(Ctx, Var);
    plus(A, B) => eval(A, Ctx) + eval(B, Ctx);
    minus(C, D) => eval(C, Ctx) - eval(D, Ctx)
}.

T1 : any, T2 : any, F : (T1 -> T2) =>
declare let(T1, F) -> T2.
let(V, Fn) := Fn!V.

instance expr(T) : plus where {
  A+B := plus(A, B)			   
}.

instance expr(T) : minus where {
  A-B := minus(A, B)			   
}.


assert let << {
    Expr := v("X") + c(2.0) - v("Y");
    Ctx := [("X", 3.0), ("Y", 4.0)];
    eval(&Expr, &Ctx) == just(1.0) del Expr, Ctx
}.

A\/B := if(A, true, B).
A/\B := if(A, B, false).


declare depends_on(&expr(T), &string) -> bool.
depends_on(Expr, Var) := case Expr of & {
    c(_) => false;
    v(Var1) => Var == Var1;
    plus(A, B) => depends_on(A, Var) \/ depends_on(B, Var);
    minus(C, D) => depends_on(C, Var) \/ depends_on(D, Var)
}.

assert let << {
    Expr := v("X") + c(2.0) - v("Y");
    Var := "X";
    depends_on(&Expr, &Var) del Var, Expr
}.

% assert let << {
%     Expr := v("X") + c(2.0) - v("Y");
%     Var := "T";
%     \+depends_on(Expr, &Var) del Var
% }.
```
