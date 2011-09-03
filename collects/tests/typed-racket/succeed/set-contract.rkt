#lang typed/racket

(define-predicate string-set? (Setof String))

(string-set? (set 1 2 3))

(string-set? (set "1" "2" "3"))
