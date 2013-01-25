#lang web-server/base

(define (f a) a)

(module test racket/base
  (require rackunit)
  (check-equal? 1 1))
