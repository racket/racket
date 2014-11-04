#lang typed/racket/base

(: f : Real → Real)
(define (f x)
  (+ 1 (expt 1.0 x)))
(f 0)
