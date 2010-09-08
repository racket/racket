#;
(
vector-length.rkt line 10 col 1 - vector-length - vector-length
3
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(vector-length (vector 1 2 3))
