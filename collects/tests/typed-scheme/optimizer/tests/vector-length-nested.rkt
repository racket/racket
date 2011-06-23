#;
(
TR opt: vector-length-nested.rkt 11:0 (vector-length (vector-ref (ann (vector (vector 1 2) 2 3) (Vector (Vectorof Integer) Integer Integer)) 0)) -- vector-length
TR opt: vector-length-nested.rkt 12:1 (vector-ref (ann (vector (vector 1 2) 2 3) (Vector (Vectorof Integer) Integer Integer)) 0) -- vector
2
)

#lang typed/scheme
#:optimize

(vector-length
 (vector-ref
  (ann (vector (vector 1 2) 2 3)
       (Vector (Vectorof Integer) Integer Integer))
  0))
