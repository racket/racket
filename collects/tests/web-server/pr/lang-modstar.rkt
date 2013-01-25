#lang web-server/base

(define f 1)

(module+ test
  (define g f))
