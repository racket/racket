#;
(
flvector-length.rkt line 10 col 1 - flvector-length - flvector-length
2
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops racket/flonum)
(flvector-length (flvector 0.0 1.2))
