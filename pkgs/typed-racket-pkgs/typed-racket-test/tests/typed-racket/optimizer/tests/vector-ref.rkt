#;#;
#<<END
TR opt: vector-ref.rkt 2:0 (vector-ref (ann (vector 1 2) (Vector Integer Integer)) 0) -- vector
END
#<<END
1

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(vector-ref (ann (vector 1 2) (Vector Integer Integer)) 0)
