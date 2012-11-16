#lang typed/racket

(provide raise-length-error)

(: raise-length-error (Symbol String Any Index -> Nothing))
(define (raise-length-error name type-name xs n)
  (raise-argument-error name (format "~a of length ~a" type-name n) xs))
