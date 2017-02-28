#lang racket/base

(provide nonpositive-integer?
         negative-integer?
         nonnegative-integer?
         positive-integer?)

(require "misc.rkt"
         "and.rkt")

(define nonpositive-integer?
  (and/c integer? (>=/c 0)))

(define negative-integer?
  (and/c integer? (</c 0)))

(define nonnegative-integer?
  (and/c integer? (>=/c 0)))

(define positive-integer?
  (and/c integer? (>/c 0)))