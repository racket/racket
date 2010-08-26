#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(vector-ref (ann (vector 1 2) (Vector Integer Integer)) 0)
