#;#;
#<<END
TR opt: vector-set2.rkt 2:0 (vector-set! (vector 1 2) 0 2) -- vector
END
""
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(vector-set! (vector 1 2) 0 2) ; type is (Vectorof Integer), length is ot known, can't optimize
