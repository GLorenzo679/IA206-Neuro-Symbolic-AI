my_second([], L2, Acc) :- L2 = Acc.
my_second([H|L1], L2, Acc) :-
    length([H|L1], A),
    C is (A rem 2),
    C \= 0,
    my_second(L1, L2, [H|Acc]).
my_second([_|L1], L2, Acc) :- my_second(L1, L2, Acc).

my_second(L1, L2) :- my_second(L1, L2, []).

second([], []).
second([X], [X]).
second([X, _|R], [X|R1]) :-
    second(R, R1).