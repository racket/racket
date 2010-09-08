#;
(
binary-nonzero-fixnum.rkt line 11 col 9 - vector-length - vector-length
binary-nonzero-fixnum.rkt line 11 col 1 - modulo - binary nonzero fixnum
1
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(modulo (vector-length '#(1 2 3)) 2)
