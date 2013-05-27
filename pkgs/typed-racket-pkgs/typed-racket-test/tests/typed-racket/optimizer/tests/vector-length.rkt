#;#;
#<<END
TR opt: vector-length.rkt 2:0 (vector-length (vector 1 2 3)) -- known-length vector-length
TR opt: vector-length.rkt 3:0 (vector-length (ann (vector 4 5 6) (Vectorof Integer))) -- vector-length
END
#<<END
3
3

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(vector-length (vector 1 2 3))
(vector-length (ann (vector 4 5 6) (Vectorof Integer)))
