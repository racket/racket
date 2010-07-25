#lang typed/racket #:optimize
(require racket/unsafe/ops)
(apply + (map add1 (list 1 2 3)))
(apply * (map add1 (list 1 2 3)))
