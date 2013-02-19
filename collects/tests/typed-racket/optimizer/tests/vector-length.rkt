#;
(
TR opt: vector-length.rkt 14:0 (vector-length (vector 1 2 3)) -- known-length vector-length
TR opt: vector-length.rkt 15:0 (vector-length (ann (vector 4 5 6) (Vectorof Integer))) -- known-length vector-length
'#(1 2 3)
3
'#(4 5 6)
3
)

#lang typed/scheme
#:optimize

(vector-length (vector 1 2 3))
(vector-length (ann (vector 4 5 6) (Vectorof Integer)))
