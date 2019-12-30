# DSL Example

The following compiles successfully:

```prolog
union expr(T) = c(T)
              + v(string)
              + plus(expr(T), expr(T))
              + minus(expr(T), expr(T)).
              
struct (A=B) = (A=B).

declare lookup(&list(string=float64), &string) -> maybe(float64).

lookup(L, Key) := case L of & {
    [] => none;
    [&(K1=Val) | Rest] => if(K1 == Key,
			     just(*Val),
			     lookup(Rest, Key))
}.

declare eval(&expr(float64), &list(string=float64)) -> maybe(float64).
eval(Expr, Ctx) := case Expr of & {
    c(N) => just(N);
    v(Var) => lookup(Ctx, Var);
    plus(A, B) => eval(A, Ctx) + eval(B, Ctx);
    minus(A, B) => eval(A, Ctx) - eval(B, Ctx)
}.
```
