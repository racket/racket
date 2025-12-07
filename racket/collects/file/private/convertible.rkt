#lang racket/base

(provide (protect-out
          prop:convertible convertible? convertible-ref
          convert))

(define-values (prop:convertible convertible? convertible-ref)
  (make-struct-type-property 'convertible))

(define (convert v request [default #f])
  ((convertible-ref v) v request default))
