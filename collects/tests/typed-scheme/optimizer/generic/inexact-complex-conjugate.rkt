#lang typed/scheme #:optimize
(require racket/unsafe/ops)
(+ (conjugate 1.0+2.0i) (conjugate 2.0+4.0i))
