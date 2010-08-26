#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(vector-ref (vector 1 2 3) 0)
