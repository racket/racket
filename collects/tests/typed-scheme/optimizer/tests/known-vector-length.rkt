#;
(
TR opt: known-vector-length.rkt 11:0 (+ 2 (vector-length (ann (vector 1 2) (Vector Integer Integer)))) -- fixnum bounded expr
TR opt: known-vector-length.rkt 11:5 (vector-length (ann (vector 1 2) (Vector Integer Integer))) -- known-length vector-length
4
)

#lang typed/scheme
#:optimize

(+ 2 (vector-length (ann (vector 1 2) (Vector Integer Integer))))
