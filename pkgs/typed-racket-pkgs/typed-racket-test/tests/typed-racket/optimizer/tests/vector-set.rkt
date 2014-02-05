#;#;
#<<END
TR opt: vector-set.rkt 2:0 (vector-set! (ann (vector 1 2) (Vector Integer Integer)) 0 1) -- vector
END
""
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(vector-set! (ann (vector 1 2) (Vector Integer Integer))
             0
             1)
