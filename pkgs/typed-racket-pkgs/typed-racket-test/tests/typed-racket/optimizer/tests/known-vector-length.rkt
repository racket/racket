#;#;
#<<END
TR opt: known-vector-length.rkt 2:0 (+ 2 (vector-length (ann (vector 1 2) (Vector Integer Integer)))) -- fixnum bounded expr
TR opt: known-vector-length.rkt 2:5 (vector-length (ann (vector 1 2) (Vector Integer Integer))) -- known-length vector-length
END
#<<END
4

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(+ 2 (vector-length (ann (vector 1 2) (Vector Integer Integer))))
