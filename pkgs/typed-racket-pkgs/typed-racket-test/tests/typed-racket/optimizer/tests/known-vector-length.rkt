#;
#<<END
TR opt: known-vector-length.rkt 12:5 (vector-length (ann (vector 1 2) (Vector Integer Integer))) -- known-length vector-length
TR opt: known-vector-length.rkt 12:0 (+ 2 (vector-length (ann (vector 1 2) (Vector Integer Integer)))) -- fixnum bounded expr
4

END

#lang typed/scheme
#:optimize

(+ 2 (vector-length (ann (vector 1 2) (Vector Integer Integer))))
