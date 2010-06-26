#lang datalog/sexp
(! (parent john douglas))
(? (parent john douglas))

(? (parent john ebbon))

(! (parent bob john))
(! (parent ebbon bob))
(? (parent A B))

(? (parent john B))

(? (parent A A))

(! (:- (ancestor A B)
       (parent A B)))
(! (:- (ancestor A B)
       (parent A C)
       (ancestor C B)))
(? (ancestor A B))

(? (ancestor X john))

(~ (parent bob john))

(? (parent A B))

(? (ancestor A B))
