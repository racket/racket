#lang racket/base
(#%declare #:unsafe-cross-phase-persistent)

(provide (protect-out
          prop:convertible convertible? convertible-ref
          convert))

(define-values (prop:convertible convertible? convertible-ref)
  (make-struct-type-property 'convertible))

(define (convert v request [default #f])
  ((convertible-ref v) v request default))
