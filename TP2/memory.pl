empty(Predicate) :-
    clause(Predicate, _),
    retract(Predicate),
    fail.

empty(_).