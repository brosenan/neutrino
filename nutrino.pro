:- op(1050, fx, type).
:- op(1050, fx, struct).
:- op(1050, fx, class).
:- op(1050, fx, instance).
:- op(1090, xfx, =>).
:- op(900, xfx, where).
:- op(700, fx, case).
:- op(600, xfx, of).
:- op(300, xfy, !).

type result(T, E) = ok(T) + err(E).

T : any, Err : error =>
class P : proc(T, Err) where {
    run(P, io) -> (result(T, Err), io)
}.

struct return(T) = T.

T : any =>
instance return(T) : proc(T, _) where {
    run(return(Ret), IO) := (ok(Ret), IO)
}.

T1 : any, T2: any, Err : error, P1 : proc(T1, Err), F : (T1 -> proc(T2, Err)) =>
instance do(P1, F) : proc(T2, Err) where {
    run(do(P1, F), IO) := case run(P1) of {
        ok(V) => run(F!V, IO);
        err(E) => (err(E), IO)
    }
}.

type print(string).
