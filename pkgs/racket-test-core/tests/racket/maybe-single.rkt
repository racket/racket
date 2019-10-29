#lang racket/base

(provide (rename-out
          [sf:read read]
          [sf:read-syntax read-syntax]))

(define (sf:read in mod line col pos)
  (parameterize ([read-single-flonum (single-flonum-available?)])
    (read in)))

(define (sf:read-syntax in src mod line col pos)
  (parameterize ([read-single-flonum (single-flonum-available?)])
    (read-syntax in src)))
