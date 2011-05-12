#;
(
flvector-length.rkt 10:1 flvector-length -- flvector-length
2
)

#lang typed/scheme
#:optimize
(require racket/flonum)
(flvector-length (flvector 0.0 1.2))
