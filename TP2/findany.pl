findany(Var, Pred, _) :-
    Pred,
    asserta(found(Var)),
    fail.

findany(_, _, Result) :-
    collect_found(Result),
    retractall(found(_)).

collect_found(Results) :-
    collect_found([], Results).

collect_found(Acc, Results) :-
    retract(found(Var)),
    !,
    collect_found([Var|Acc], Results).

collect_found(Results, Results).