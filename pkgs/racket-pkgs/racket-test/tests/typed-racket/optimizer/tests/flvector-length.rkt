#;
(
TR opt: flvector-length.rkt 10:0 (flvector-length (flvector 0.0 1.2)) -- flvector-length
2
)

#lang typed/scheme
#:optimize
(require racket/flonum)
(flvector-length (flvector 0.0 1.2))
