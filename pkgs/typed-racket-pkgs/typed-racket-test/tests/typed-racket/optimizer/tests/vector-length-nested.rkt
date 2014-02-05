#;#;
#<<END
TR opt: vector-length-nested.rkt 2:0 (vector-length (vector-ref (ann (vector (vector 1 2) 2 3) (Vector (Vectorof Integer) Integer Integer)) 0)) -- vector-length
TR opt: vector-length-nested.rkt 3:1 (vector-ref (ann (vector (vector 1 2) 2 3) (Vector (Vectorof Integer) Integer Integer)) 0) -- vector
END
#<<END
2

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(vector-length
 (vector-ref
  (ann (vector (vector 1 2) 2 3)
       (Vector (Vectorof Integer) Integer Integer))
  0))
