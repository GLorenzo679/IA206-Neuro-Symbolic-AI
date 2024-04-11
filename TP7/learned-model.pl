state/2.
trans/3.
emit/3.
observe/2.
observe_sequence/1.
observe_sequence_aux/1.
state_sequence/1.
state_sequence_aux/1.
observe(Time,Symbol) :- state(Time,State), emit(Time,State,Symbol).
print([]) :- nl.
print(X) :- write(X), nl.
print(X,Y) :- write(X), write(' - '), write(Y), nl.
observe_sequence_aux(Time,State,[Symbol | Tail]) :- trans(Time,State,State1), Time1 is Time+1, emit(Time1,State1,Symbol), observe_sequence_aux(Time1,State1,Tail).
observe_sequence_aux(_,_,[]).
observe_sequence([First | Rest]) :- start(Start_state), emit(0,Start_state,First), observe_sequence_aux(0,Start_state,Rest).
state_sequence_aux(_,_,[]).
state_sequence_aux(Time,State,[State1 | Rest]) :- trans(Time,State,State1), Time1 is Time+1, state_sequence_aux(Time1,State1,Rest).
state_sequence([Start | Rest]) :- start(Start), state_sequence_aux(0,Start,Rest).
state(0,State) :- start(State).
state(Time,State) :- Time>0, Previous is Time-1, state(Previous,Previous_state), trans(Previous,Previous_state,State).
generate_sequence(L,N) :- length(L,N), observe_sequence(L).
1.0::start(0); 0.0::start(1); 0.0::start(2).
0.118448473026753::emit(Time,0,'cloudy-dry'); 0.130228710579268::emit(Time,0,'fog'); 0.052942696489657::emit(Time,0,'foggy-rainy'); 0.328400245716832::emit(Time,0,'cloudy-rainy'); 0.0::emit(Time,0,'sunshine+rain'); 0.36997987418749::emit(Time,0,'sunshine').
0.0::emit(Time,1,'cloudy-dry'); 0.0::emit(Time,1,'fog'); 0.0::emit(Time,1,'foggy-rainy'); 0.890679228887344::emit(Time,1,'cloudy-rainy'); 0.0::emit(Time,1,'sunshine+rain'); 0.109320771112656::emit(Time,1,'sunshine').
0.114509873999735::emit(Time,2,'cloudy-dry'); 0.130089859538586::emit(Time,2,'fog'); 0.043297623132954::emit(Time,2,'foggy-rainy'); 0.272892556713679::emit(Time,2,'cloudy-rainy'); 0.195656151603264::emit(Time,2,'sunshine+rain'); 0.243553935011781::emit(Time,2,'sunshine').
1.113822155e-06::trans(Time,0,0); 0.0::trans(Time,0,1); 0.999998886177845::trans(Time,0,2).
0.139197813000845::trans(Time,1,0); 0.16974895804124::trans(Time,1,1); 0.691053228957915::trans(Time,1,2).
0.001704959278136::trans(Time,2,0); 0.074984504108946::trans(Time,2,1); 0.923310536612917::trans(Time,2,2).
