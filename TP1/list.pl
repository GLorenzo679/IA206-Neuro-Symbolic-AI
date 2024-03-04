% Write last_elt that extracts the last element of a list.
last_elt([_|T], X) :-
    last_elt(T, X).
last_elt([X], X).

% Write attach that joins two lists.
attach([], L, L).
attach([H|T], L, [H|R]) :-
    attach(T, L, R).

% Use attach (previously written) to design assemble that joins three lists
assemble(L1, L2, L3, R) :-
    attach(L1, L2, T),
    attach(T, L3, R).

% Use attach to write sub_list
sub_list(IncludedList, IncludingList) :-
    attach(IncludedList, _, IncludingList).
sub_list(IncludedList, [_|IncludingTail]) :-
    sub_list(IncludedList, IncludingTail).

% Write remove, that removes all occurrences of an element in a list
remove([E|T], E, R) :-
    remove(T, E, R),
    !.
remove([H|T], E, [H|R]) :-
    remove(T, E, R).
remove([], _, []).

% Write duplicate to duplicate each element in a list
duplicate([], []).
duplicate([H|TL], [H,H|TR]) :-
    duplicate(TL, TR).