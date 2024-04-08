% EX.1
flatten([X|L], L1) :-
    is_list(X),
    !,
    flatten(X, X1),
    flatten(L, L2),
    append(X1, L2, L1).
flatten([X|L], [X|L1]) :-
    flatten(L, L1).
flatten([], []).

% EX.2
edge(a, b).
edge(b, c).
edge(c, d).
edge(d, a).
edge(d, e).
edge(e, f).
edge(f, g).

path1(X, X, _) :- !.
path1(X, Y, L) :- 
    \+member(X, L),
    append([X], L, L1),
    edge(X, Z),
    path1(Z, Y, L1).

path(X, Y) :- path1(X, Y, []).  