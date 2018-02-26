#lang racket/base
(require "check.rkt"
         "path.rkt")

(provide check-path-argument)

(define (check-path-argument who p)
  (check who (lambda (p) (or (path-string? p) (path-for-some-system? p)))
         #:contract "(or/c path-string? path-for-some-system?)"
         p))
