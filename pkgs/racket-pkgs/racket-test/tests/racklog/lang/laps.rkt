#lang racklog
% Laps Test
contains(ca, store, rams_couch, rams).
contains(rams, fetch, rams_couch, will).
contains(ca, fetch, Name, Watcher) :-
    contains(ca, store, Name, Owner),
    contains(Owner, fetch, Name, Watcher).
trusted(ca).
permit(User, Priv, Name) :-
    contains(Auth, Priv, Name, User),
    trusted(Auth).
permit(User, Priv, Name)?
