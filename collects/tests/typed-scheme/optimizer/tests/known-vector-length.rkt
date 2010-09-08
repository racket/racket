#;
(
known-vector-length.rkt line 11 col 6 - vector-length - known-length vector-length
known-vector-length.rkt line 11 col 6 - vector-length - known-length vector-length
4
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(+ 2 (vector-length (ann (vector 1 2) (Vector Integer Integer))))
