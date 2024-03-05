isa(bird, animal).
isa(albert, albatross).
isa(albatross, bird).
isa(kiwi, bird).
isa(willy, kiwi).
isa(crow, bird).
isa(ostrich, bird).

:- dynamic(locomotion/2).

locomotion(bird, fly).
locomotion(kiwi, walk).
locomotion(X, Loc) :-
	isa(X, SuperX),
	locomotion(SuperX, Loc).

food(albatross,fish).
food(bird,grain).

known(Fact) :- 
	Fact,
	!.

known(Fact) :-
	Fact =.. [Rel, Arg1, Arg2],
	isa(Arg1, SuperArg1),
	SuperFact =.. [Rel, SuperArg1, Arg2],
	known(SuperFact).

can_fly(Animal) :-
    known(locomotion(Animal, M)),
    M = fly.

habitat(Animal, Location) :-
    can_fly(Animal),
    Location = unknown.

habitat(Animal, Location) :-
    not(can_fly(Animal)),
    Location = continent.