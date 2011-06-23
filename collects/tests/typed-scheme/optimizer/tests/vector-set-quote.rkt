#;
(
TR opt: vector-set-quote.rkt 9:0 (vector-set! (ann (vector (quote (1 2))) (Vector Any)) 0 (quote (+ 1.0 2.0))) -- vector
)

#lang typed/scheme
#:optimize

(vector-set! (ann (vector '(1 2)) (Vector Any))
             0
             '(+ 1.0 2.0)) ; we should not optimize under quote
