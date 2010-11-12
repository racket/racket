#;
(
vector-set-quote.rkt line 9 col 1 - vector-set! - vector
)

#lang typed/scheme
#:optimize

(vector-set! (ann (vector '(1 2)) (Vector Any))
             0
             '(+ 1.0 2.0)) ; we should not optimize under quote
