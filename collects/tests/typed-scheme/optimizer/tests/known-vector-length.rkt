#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(+ 2 (vector-length (ann (vector 1 2) (Vector Integer Integer))))
