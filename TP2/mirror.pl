% better solution with accumulator:
mirror2(Left, Right) :-
    invert(Left, [], Right).
invert([X|L1], L2, L3) :-       % the list is 'poured'
    invert(L1, [X|L2], L3).     % into the second argument
invert([], L, L).               % at the deepest level, the result L is merely copied

palindrome(L) :-
    mirror2(L, Reverse),
    L = Reverse.

palindrome2(L) :- palindrome2(L, []).

palindrome2(L, L).      % initial list is even
palindrome2([_|L], L).  % initial list is odd

palindrome2([X|L], Acc) :- 
    append([X], Acc, NewAcc),
    palindrome2(L, NewAcc).
