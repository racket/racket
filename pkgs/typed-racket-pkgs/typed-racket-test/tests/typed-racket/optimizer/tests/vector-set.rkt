#;
(
TR opt: vector-set.rkt 9:0 (vector-set! (ann (vector 1 2) (Vector Integer Integer)) 0 1) -- vector
)

#lang typed/scheme
#:optimize

(vector-set! (ann (vector 1 2) (Vector Integer Integer))
             0
             1)
