#;
(
TR opt: vector-length.rkt 10:0 (vector-length (vector 1 2 3)) -- vector-length
3
)

#lang typed/scheme
#:optimize

(vector-length (vector 1 2 3))
