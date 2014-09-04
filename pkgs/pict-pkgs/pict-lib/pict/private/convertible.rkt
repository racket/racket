#lang racket/base
(provide prop:pict-convertible prop:pict-convertible? pict-convertible? pict-convert)

(define-values (prop:pict-convertible -pict-convertible? pict-convertible-ref)
  (make-struct-type-property 'pict-convertible))

(define-values (prop:pict-convertible? pict-convertible?? pict-convertible?-ref)
  (make-struct-type-property 'pict-convertible?))

(define (pict-convertible? x)
  (and (-pict-convertible? x)
       (if (pict-convertible?? x)
           ((pict-convertible?-ref x) x)
           #t)))

(define (pict-convert v)
  (unless (pict-convertible? v)
    (raise-type-error 'pict-convert "pict-convertible" v))
  ((pict-convertible-ref v) v))
