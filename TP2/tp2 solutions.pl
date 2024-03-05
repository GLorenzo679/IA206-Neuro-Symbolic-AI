%OPTIONAL hannoi
factorial(1, 1) :- !.
factorial(N, F) :-
    N1 is N - 1,
    factorial(N1, F1),
    F is F1 * N.


hanoi :-
	% 1 is the smallest disk and 5 the largest
	move([5,4,3,2,1], a, c, b).

move([ ], _, _, _).
move([D1|Stack], Start, Goal, Interm) :-
	move(Stack, Start, Interm, Goal), % this is a (central) recursive call
	write('move disk '), write(D1), write(' from '), write(Start), write(' to '), write(Goal), nl,
	move(Stack, Interm, Goal, Start). % yet another recursive call
	
	
	
/* Same algorithm with depth-sensitive trace             */

hanoi1 :-
	% 1 is the smallest disk and 5 the largest
	move1(0, [4,3,2,1], a, c, b).

drawDepth(0) :-	!.
drawDepth(Depth) :-
	write('_'),
	Depth1 is Depth -1,
	drawDepth(Depth1).
	
move1(_, [ ], _, _, _).
move1(Depth, [D1|Stack], Start, Goal, Interm) :-
	Depth1 is Depth + 1,
	move1(Depth1, Stack, Start, Interm, Goal), % this is a (central) recursive call
	drawDepth(Depth), 
	write('move1 disk '), write(D1), write(' from '), write(Start), write(' to '), write(Goal), nl,
	move1(Depth1, Stack, Interm, Goal, Start). % yet another recursive call


/****************************************************************/
/* State transition in the Tower of Hanoi puzzle                */
/****************************************************************/

% the program may be called through:
hanoi2 :-
	search2([[1,2,3,4],[ ],[ ]], R), !, print_solution(R).

search2(Node, [Node]) :-
	success(Node).
search2(Node, [Node|Sol1]) :-
	s(Node, Node1),
	write(Node1),nl, get0(_), % for the trace
	search2(Node1, Sol1).	% Note lateral (not central) recursivity


s([Ta,Tb,Tc], [Ta1,Tb1,Tc1]) :-
	permute(K, [Ta,Tb,Tc], [[D1|T1],T2,T3]), % as if source pole were a
	allowed(D1,T2), % checks that the move is legal
	permute(K, [Ta1,Tb1,Tc1], [T1,[D1|T2],T3]). % as if target pole were b - Note that K is the same

allowed(_,[]). % any disk can be put on an empty pole
allowed(D1,[D2|_]) :-
	D1 < D2. % checks that the disk beneath is larger
	
permute(1,[A,B,C],[A,B,C]). % Next time, let's write permute as a genuine prolog programme !
permute(2,[A,B,C],[A,C,B]).
permute(3,[A,B,C],[B,C,A]).
permute(4,[A,B,C],[B,A,C]).
permute(5,[A,B,C],[C,B,A]).
permute(6,[A,B,C],[C,A,B]). % Theory says that 3! = 6.

	% The state to be reached is indicated by:

success([[],[],_]). % all disks in c

/* Same algorithm with cycle detection             */

hanoi3 :-
	search3([[1,2,3,4],[ ],[ ]], R), !, print_solution(R).

search3(Node, Solution) :-
	depth([ ], Node, Solution).

depth(Path, Node, [Node | Path]) :-
	success(Node).

depth(Path, Node, Sol) :-
	s(Node, Node1),
	not(member(Node1, Path)),
	depth([Node | Path], Node1, Sol).


% Avoiding infinite depth paths
% Looking for Paths of increasing length
%--------------------------------------------------

hanoi4 :-
	search4([[1,2,3,4],[ ],[ ]], R), !, print_solution(R).

search4(Start, Solution) :-
	path(Start, Goal, Solution),
	success(Goal).

path(N, N, [N]).
path(First, Last, [Last | Path]) :-
	path(First, Penultimate, Path),
	s(Penultimate, Last),
	not(member(Last, Path)).

% 'search4' is in fact a width-first strategy, though an inefficient one


% Width-first strategy
%--------------------------------------------------

hanoi5 :-
	search5([[1,2,3,4],[ ],[ ]], R), !, print_solution(R).

search5(Start, Solution) :-
	width([[Start]], Solution). % first argument is the list of covered branches

width([[Node | Path] | _], [Node | Path]) :-
	% write(Node),nl, get0(_), % pour la trace
	success(Node).
width([Path | Paths], Solution) :-
	extension( Path, NewPaths ),	% expands first branch by one move, thus creating new branches
	append( Paths, NewPaths, Paths1),
	width(Paths1, Solution).

extension([Node | Path], NewPaths) :-
	bagof([NewNode, Node  | Path],
		(s(Node, NewNode),
			not(member(NewNode, [Node | Path]))),
		NewPaths ),
	!.
	/* bagof puts together into a list (3rd arg.) objects 
	   corresponding to the first argument that make the predicate
	   given as second argument true */
extension(_, [ ]).


%                       useful predicate
print_solution([ ]).
print_solution([E|R]) :-
	print_solution(R),
	write(E), nl.
	


%OPTINAL hannoi


%action(InitialState, Action, ObtainedState)

%state instance
%state(middle, onbox, middle, not_holding)

action(state(middle, onbox, middle, not_holding),
        grab,
        state(middle, onbox, middle,holding)).
action(state(P, floor, P, T),
        climb,
        state(P, onbox, P, T)).
action(state(P1, floor, P1, T),
        push(P1, P2),
        state(P2, floor, P2, T)).
action(state(P1, floor, B, T),
	walk(P1, P2),
	state(P2, floor, B, T)).

	% Definition of the success conditions in the problem of the monkey

%success(state(_, _, _, holding)).
%success(State1) :-
%    action(State1, A, State2),
%    write('Action : '), write(A), nl, write(' --> '), write(State2), nl,
%    success(State2).

%first answer
%it keeps in an infinite loop:
%Using the trace command i can see that it first evaluates the success(State1) predicator since the input does not have the shape success(state(_, _, _, holding)). 
%Then it evaluates action(State1, A, State2),. it goes through the set of actions defined, first, it evaluates 'walk' since we put it first
%Because of the shape of the action walk it defines the State2 as state(_9680, floor, window, not_holding)
%which is pretty much the same to the initial input we defined, just the with the difference of _9680
%So, we're entering again to success(State1) and keeps doing the same thing

%second answer
success(state(_, _, _, holding), []).
success(State1, [A|Plan]) :-
		action(State1, A, State2),
		success(State2, Plan).

%third answer
mirror2(Left, Right) :-
    invert(Left, [ ], Right).
invert([X|L1], L2, L3) :-    % the list is 'poured'
    invert(L1, [X|L2], L3).    % into the second argument
invert([ ], L, L).        % at the deepest level, the result L is merely copied

%%%%%%%%% first task

palindrome(L) :-
	mirror2(L, L).

%%%%%%%%%%%%%%%%%% second task

palindrome2(L) :-
	palindrome2(L, []).

palindrome2(L,L). %this one for the case the word is even
palindrome2([_|L],L). %this one for the case the word is odd

palindrome2([X|XS], Acc) :-
	append([X], Acc, NewAcc),
	palindrome2(XS, NewAcc).

%%%%%%%%%%%%% fourth answer


%myprint :-
%    retract(item(X)), % succeeds as long as there are items
%    write(X), nl,
%    fail.
%myprint.


%empty(Predicate) :-
%    Predicate =.. [Name|Args], % Extract the functor and arguments
%    Fact =.. [Name|Args],      % Create a term to match the fact
%    retract(Fact),
%    fail.
%empty(_).

empty(Predicate) :-
    retract(Predicate), % succeeds as long as there are items
    fail.
empty(_). %when there are no more items, just return true



%%%%%%% fifth answer

parent(marge, lisa).
parent(marge, bart).
parent(marge, maggie).
parent(homer, lisa).
parent(homer, bart).
parent(homer, maggie).
parent(abraham, homer).
parent(abraham, herb).
parent(mona, homer).
parent(jackie, marge).
parent(clancy, marge).
parent(jackie, patty).
parent(clancy, patty).
parent(jackie, selma).
parent(clancy, selma).
parent(selma, ling).

findany(Var, Pred, _) :-
	Pred, %By executing Pred in the first place, the link (that is supposed to exist) betweeen Var and Pred gets instantiated.
	asserta(found(Var)), %The instantiated value of Var can be stored using assert
	fail. %Then, one can force backtracking by inserting fail.

findany(_, _, Result) :-
	collect_found(Result), %another predicate collect_found should gather results into a list.
    retractall(found(_)).   % Clean up the memory of found results

collect_found(Results) :-
    collect_found([], Results).

collect_found(Acc, Results) :-
    retract(found(Var)),        % get a found result
    !,                          % Cut to prevent backtracking
    collect_found([Var|Acc], Results). % Recursively collect more found results

collect_found(Results, Results).  % Once no more found results, unify Results with the accumulated list

%%%%%%%%%%%%%%%%% sixth answer

%--------------------------------
%       semantic networks
%--------------------------------

isa(bird, animal).
isa(albert, albatross).
isa(albatross, bird).
isa(kiwi, bird).
isa(willy, kiwi).
isa(crow, bird).
isa(ostrich, bird).

:- dynamic(locomotion/2).	% for tests

locomotion(bird, fly).
locomotion(kiwi, walk).
locomotion(ostrich, run).
locomotion(X, Loc) :-
	isa(X, SuperX),
	locomotion(SuperX, Loc).

food(albatross,fish).
food(bird,grain).

/* drawback : n particular inheritance rules */
/* solution: one general predicate : "known" */

known(Fact) :- 
	Fact,
	!.
known(Fact) :-
	Fact =.. [Rel, Arg1, Arg2],
	isa(Arg1, SuperArg1),
	SuperFact =.. [Rel, SuperArg1, Arg2],
	known(SuperFact).

%Input:
%known(locomotion(albert, L)).
%Output:
%L = fly.
%
%Input:
%known(locomotion(kiwi, L)).
%Output:
%L = walk.
%This is because we defined locomotion(kiwi, walk). and then, prolog using the first class just keep that result since we use the cut (!) operator.

%%%% seventh answer

animal_can_fly(Animal) :-
	known(locomotion(Animal, M)),
	M = fly.                       %with the 2 sentences above we evaluate if the animal can fly

habitat(Animal, Location) :-
    animal_can_fly(Animal),        %if the animal fly the location must be unknown
	Location = unknown.

habitat(Animal, Location) :-
    not(animal_can_fly(Animal)),    %if the animal doesn't fly the locaiton must be continent
	Location = continent.

%%% eight answer
%aunt ≐ woman ⊓ ∃ sister.parent
