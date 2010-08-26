#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(quotient (vector-length '#(1 2 3)) 2)
