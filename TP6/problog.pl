% Write a clause to express "Alice is happy with probability 50%" using the atom alice_is_happy.
.5::alice_is_happy.

% Write a clause to express "There is a school test with probability 5%" using the atom school_test.
.05::school_test.

% Write a clause to express "The class is fun with probability 10%" using the atom fun_class.
.1::fun_class.

% Write a rule to express "Alice is happy if the class was fun, with probability
% 30%" using the atoms defined previously.
.3::alice_is_happy :- fun_class.

% Write a rule to express "Alice is not happy if it’s raining, with probability 70%" using the atoms
% defined previously, including raining, used in the introduction of this section.
.8::raining :- cloudy.
.3::cloudy ; .7::sunshine.
.7::\+alice_is_happy :- raining.

% Write a rule to express "Alice is not happy if there is a test, with probability 90%"
% using the atoms defined previously.
.9::\+alice_is_happy :- school_test.

% Taking all the rules defined until now, what is the probability that Alice is happy,
% knowing that the class was fun?
.2::raining.
.5::alice_is_happy.
.05::school_test.
.1::fun_class.
.3::alice_is_happy :- fun_class.
.7::\+alice_is_happy :- raining.
.9::\+alice_is_happy :- school_test.
% Knowing that the class was fun...
evidence(fun_class).
% ...what is the probability that Alice is happy?
query(alice_is_happy).

% Write a rule to express "A pupil P is happy if one of their friends Q is happy, with probability 80%".
% You may test whether the syntax of your rule is correct by running it through the ProbLog interpreter.
.5::happy(P).
.3::happy(P) :- fun_class.
.8::happy(P) :- friend(P,Q), happy(Q).
.7::\+happy(P) :- raining.
.9::\+happy(P) :- school_test.
.1::fun_class.
.05::school_test.
.2::raining.
friend(alice, charlie). % Alice is friends with Charlie
friend(charlie, bob).
friend(dorothy, emily).
friend(X,Y) :- Y @< X, friend(Y,X). % symmetry of friendship
friend(X,Y) :- X\=Y, friend(X,Z), friend(Z,Y). % transitivity of friendship

.8::happy(P) :- friend(P,Q), happy(Q).