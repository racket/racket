#lang racket/base

(provide nonpositive-integer/c
         negative-integer/c
         nonnegative-integer/c
         positive-integer/c)

(require "misc.rkt"
         "and.rkt")

(define nonpositive-integer/c
  (and/c integer? (>=/c 0)))

(define negative-integer/c
  (and/c integer? (</c 0)))

(define nonnegative-integer/c
  (and/c integer? (>=/c 0)))

(define positive-integer/c
  (and/c integer? (>/c 0)))