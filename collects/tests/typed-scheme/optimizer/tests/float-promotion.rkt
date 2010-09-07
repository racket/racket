#lang typed/scheme
#:optimize
(require racket/unsafe/ops racket/flonum)
(+ (modulo 1 1) 2.0)
(+ (expt 100 100) 2.0)
