#lang datalog/sexp
(? (add1 2 :- X))

(! (:- (add2 X Y)
       (add1 X :- Z)
       (add1 Z :- Y)))

(? (add2 1 3))

(? (add1 X :- 1))
