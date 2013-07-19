#lang racket/base

(provide prop:place
         prop:place?
         prop:place-ref)

(define-values (prop:place prop:place? prop:place-ref)
  (make-struct-type-property 'place))
