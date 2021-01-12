#lang s-exp racket/kernel

(#%provide prop:place
           prop:place?
           prop:place-ref)

(define-values (prop:place prop:place? prop:place-ref)
  (make-struct-type-property 'place))
