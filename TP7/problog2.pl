% Write ProbLog code that describes one throw of two unbiased dice. You should write two predicates:
% 1. die(+Id, -Value) where Id can be any integer, and Value is the result of rolling one die
%    (so, an integer from 1 to 6).
% 2. roll(?Outcome) where Outcome is the result of one round of the game above: roll two dice,
% then sum their value. Outcome is an integer.

1/6::die(Id,1); 1/6::die(Id,2); 1/6::die(Id,3); 1/6::die(Id,4); 1/6::die(Id,5); 1/6::die(Id,6).

roll(Outcome) :-
    die(1,Value1),
    die(2,Value2),
    Outcome is Value1+Value2.

% Alice expects that both of Bob’s dice are biased, with the same bias.
% Knowing that t(_)::a. means that a is a probabilistic ProbLog atom with a learnable parameter,
% edit the previous die/2 predicate to have learnable probabilities.

t(_)::die(Id,1); t(_)::die(Id,2); t(_)::die(Id,3); t(_)::die(Id,4); t(_)::die(Id,5); t(_)::die(Id,6).

% Write a probabilistic clause to express that in the wet state, the weather is 'cloudy-rainy' with probability 4/7,
% 'fog' with probability 2/7 and 'foggy-rainy' with probability 1/7.

4/7::emit(Time, 1, "cloudy-rainy"); 2/7::emit(Time, 1, 'fog'); 1/7::emit(Time, 1, 'foggy-rainy').

% Write a ProbLog clause expressing that from state 2, the weather stays in state 2 w.p. .2,
% and goes to state 0 or 1 w.p. .4 for each.

.4::trans(Time, 2, 0); .4::trans(Time, 2, 1); .2::trans(Time, 2, 2).

% Take all the rules about start, trans and emit that we had so far, and 
% change them to turn their probabilities into learnable parameters. 
% Paste in those lines of code into the answer form.

t(_)::start(0); t(_)::start(1).
t(_)::emit(Time, 0, 'cloudy-dry'); t(_)::emit(Time, 0, 'sunshine').
t(_)::emit(Time, 1, 'cloudy-rainy');
t(_)::emit(Time, 1, 'fog'); t(_)::emit(Time, 1, 'foggy-rainy').
t(_)::trans(Time, 0, 0); t(_)::trans(Time, 0, 1);
t(_)::trans(Time, 0, 2).
t(_)::trans(Time, 1, 0); t(_)::trans(Time, 1, 1);
t(_)::trans(Time, 1, 2).
t(_)::trans(Time, 2, 0); t(_)::trans(Time, 2, 1);
t(_)::trans(Time, 2, 2).
emit(Time, 2, 'sunshine+rain'). 
