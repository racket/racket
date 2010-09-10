#;
(
vector-set.rkt line 9 col 1 - vector-set! - vector
)

#lang typed/scheme
#:optimize

(vector-set! (ann (vector 1 2) (Vector Integer Integer))
             0
             1)
