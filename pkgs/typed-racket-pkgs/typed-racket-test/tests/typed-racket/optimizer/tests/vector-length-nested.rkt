#;#;
#<<END
TR opt: vector-length-nested.rkt 14:0 (vector-length (vector-ref (ann (vector (vector 1 2) 2 3) (Vector (Vectorof Integer) Integer Integer)) 0)) -- vector-length
TR opt: vector-length-nested.rkt 15:1 (vector-ref (ann (vector (vector 1 2) 2 3) (Vector (Vectorof Integer) Integer Integer)) 0) -- vector
END
#<<END
2

END

#lang typed/scheme
#:optimize

(vector-length
 (vector-ref
  (ann (vector (vector 1 2) 2 3)
       (Vector (Vectorof Integer) Integer Integer))
  0))
