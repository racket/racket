#;
(
vector-length-nested.rkt line 11 col 1 - vector-length - vector-length
vector-length-nested.rkt line 12 col 2 - vector-ref - vector
2
)

#lang typed/scheme
#:optimize

(vector-length
 (vector-ref
  (ann (vector (vector 1 2) 2 3)
       (Vector (Vectorof Integer) Integer Integer))
  0))
