#;
(
TR opt: vector-ref2.rkt 10:0 (vector-ref (vector 1 2 3) 0) -- vector
1
)

#lang typed/scheme
#:optimize

(vector-ref (vector 1 2 3) 0)
