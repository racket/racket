#lang datalog
% Equality test
ancestor(A, B) :-
    parent(A, B).
ancestor(A, B) :-
    parent(A, C),
    D = C,      % Unification required
    ancestor(D, B).
parent(john, douglas).
parent(bob, john).
parent(ebbon, bob).
ancestor(A, B)?
