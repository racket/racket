#lang racket/base

(provide prop:convertible convertible? convert)

(define-values (prop:convertible convertible? convertible-ref)
  (make-struct-type-property 'convertible))

(define (convert v target [default #f])
  (unless (convertible? v)
    (raise-type-error 'convert "convertible" 0 v target))
  (unless (symbol? target)
    (raise-type-error 'convert "symbol" 1 v target))
  ((convertible-ref v) v target default))
