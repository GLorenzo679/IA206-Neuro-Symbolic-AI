% Definition of the possible actions with the state associated constraints
action(state(middle, onbox, middle, not_holding),
        grab,
        state(middle, onbox, middle, holding)).
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
success(InitialState, Plan) :- success(InitialState, [], Plan).
success(state(_, _, _, holding), Acc, Acc).
success(State1, Plan, Acc) :-
    action(State1, A, State2),
    append(Plan, [A], NewPlan),
    success(State2, NewPlan, Acc).

success_verbose(state(_, _, _, holding)).
success_verbose(State1) :-
    action(State1, A, State2),
    write('Action : '), write(A), nl, write(' --> '), write(State2), nl,
    success_verbose(State2).
