duplicate_list([], []).
duplicate_list([H|L], [H, H|DL]) :- 
    duplicate_list(L, DL).

sum_list([], 0).
sum_list([H|L], Sum) :-
    sum_list(L, S1),
    Sum is S1 + H.

reverse_list([], X, X).
reverse_list([H|L], LR, Acc) :-
    reverse_list(L, LR, [H|Acc]).
reverse_list(L, LR) :- reverse_list(L, LR, []).

max_list([X], X).
max_list([H|L], M) :-
    max_list(L, M1),
    M is max(H, M1).

remove_duplicates([], []).
remove_duplicates([X|Xs], [X|WithoutDuplicates]) :-
    delete(Xs, X, NewList),
    remove_duplicates(NewList, WithoutDuplicates).

delete([], _, []).
delete([X|Xs], X, Ys) :-
    delete(Xs, X, Ys).
delete([X|Xs], E, [X|Ys]) :-
    X \= E,
    delete(Xs, E, Ys).

merge_lists([], [], []).
merge_lists([H1|L1], [], [H1|L1]).
merge_lists([], [H2|L2], [H2|L2]).
merge_lists([H1|L1], [H2|L2], [H1|LM]) :-
    H1 =< H2,
    merge_lists(L1, [H2|L2], LM).
merge_lists([H1|L1], [H2|L2], [H2|LM]) :-
    H1 > H2,
    merge_lists([H1|L1], L2, LM).

delete_nth(1, [H|_], H).
delete_nth(N, [_|L], R) :-
    N1 is N-1,
    delete_nth(N1, L, R).

flatten_list([X|L], L1) :-
    is_list(X),
    !,
    flatten_list(X, X1),
    flatten_list(L, L2),
    append(X1, L2, L1).
flatten_list([X|L], [X|L1]) :-
    flatten_list(L, L1).
flatten_list([], []).

average_of_list([], 0, 0).
average_of_list([H|L], Sum, Avg) :-
    average_of_list(L, Sum1, _),
    Sum is Sum1 + H,
    length([H|L], A),
    Avg is Sum / A.
average_of_list(L, Avg) :- average_of_list(L, _, Avg).

remove_all(_, [], []).
remove_all(X, [X|L], R) :-
    remove_all(X, L, R).
remove_all(X, [H|L], [H|R]) :- 
    X \= H,
    remove_all(X, L, R).

insert_nth(1, E, L, [E|L]).
insert_nth(N, E, [H|L], [H|R]) :-
    N1 is N-1,
    insert_nth(N1, E, L, R).

second([], []).
second([X], [X]).
second([X, _|T], [X|R]) :-
    second(T, R).