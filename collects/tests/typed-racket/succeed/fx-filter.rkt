#lang typed/racket

;; test filters on fx primitives that tell us that if the function
;; returns at all, its arguments were fixnums
;; currently only works if the fx operation is used in test position,
;; due to the way filters are used. this should be improved in the
;; future

(require racket/fixnum)

(: f : Integer -> Fixnum)
(define (f x)
  (if (fx+ x 4) x x))
(: g : Integer -> Fixnum)
(define (g x)
  (if (fxnot x) x x))
