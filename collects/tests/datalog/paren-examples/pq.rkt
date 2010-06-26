#lang datalog/sexp
; p q test from Chen & Warren
(! (:- (q X)
       (p X)))
(! (q a))
(! (:- (p X)
       (q X)))
(? (q X))
