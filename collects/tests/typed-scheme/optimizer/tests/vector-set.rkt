#;
(
vector-set.rkt 9:1 vector-set! -- vector
)

#lang typed/scheme
#:optimize

(vector-set! (ann (vector 1 2) (Vector Integer Integer))
             0
             1)
