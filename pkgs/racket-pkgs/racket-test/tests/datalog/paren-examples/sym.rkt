#lang datalog/sexp

(! (sym a))
(! (sym b))
(! (sym c))
(! (:- (perm X Y)
       (sym X)
       (sym Y)
       (!= X Y)))

(? (perm X Y))
