#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(modulo (vector-length '#(1 2 3)) 2)
