#lang racket/base
(provide prop:convertible prop:convertible? convertible? convert)

(define-values (prop:convertible -convertible? convertible-ref)
  (make-struct-type-property 'convertible))

(define-values (prop:convertible? convertible?? convertible?-ref)
  (make-struct-type-property 'convertible?))

(define (convertible? x)
  (and (-convertible? x)
       (if (convertible?? x)
           ((convertible?-ref x) x)
           #t)))

(define (convert v)
  (unless (convertible? v)
    (raise-type-error 'convert "convertible" v))
  ((convertible-ref v) v))
