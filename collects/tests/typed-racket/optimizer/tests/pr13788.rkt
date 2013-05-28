#;
(
TR opt: pr13788.rkt 9:0 (vector-length (vector 1 2 3)) -- known-length vector-length
3
)

#lang typed/racket

(vector-length (vector 1 2 3)) ; should not print the vector
