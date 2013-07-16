#lang datalog

parent(john,douglas).
parent(john,douglas)?
% parent(john, douglas).

parent(john,ebbon)?

parent(bob,john).
parent(ebbon,bob).
parent(A,B)?
% parent(john, douglas).
% parent(bob, john).
% parent(ebbon, bob).

parent(john,B)?
% parent(john, douglas).

parent(A,A)?

ancestor(A,B) :- parent(A,B).
ancestor(A,B) :- parent(A,C), ancestor(C, B).
ancestor(A, B)?
% ancestor(ebbon, bob).
% ancestor(bob, john).
% ancestor(john, douglas).
% ancestor(bob, douglas).
% ancestor(ebbon, john).
% ancestor(ebbon, douglas).

ancestor(X,john)?
% ancestor(bob, john).
% ancestor(ebbon, john).

parent(bob, john)~
parent(A,B)?
% parent(john, douglas).
% parent(ebbon, bob).

ancestor(A,B)?
% ancestor(john, douglas).
% ancestor(ebbon, bob).
