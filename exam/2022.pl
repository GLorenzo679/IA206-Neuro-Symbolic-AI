oneWayRoad([lussac, gayac, figeac, trelissac, tourtoirac, dignac, fronsac, agonac, jumillac]).

travel(City1, City2) :-
    oneWayRoad(R),
    path(R, City1, City2).

path([H|R], C1, C2) :-
    H \= C1,
    !,
    path(R, C1, C2).
path(R, _, C2) :-
    member(C2, R).
path([], _, _) :-
    false.