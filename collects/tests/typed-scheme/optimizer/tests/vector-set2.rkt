#;
(
vector-set2.rkt line 9 col 1 - vector-set! - vector
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(vector-set! (vector 1 2) 0 2) ; type is (Vectorof Integer), length is ot known, can't optimize
