#lang racket/base

(require (only-in racket/contract/private/box box-immutable/c)
         racket/contract/private/blame
         racket/contract/private/guts)

(provide box/c box-immutable/c)

(define/subexpression-pos-prop (box/c ctc)
  (let ([ctc (coerce-flat-contract 'box/c ctc)])
    (make-flat-contract
     #:name (build-compound-type-name 'box/c ctc)
     #:first-order
     (λ (val)
       (and (box? val)
            (contract-first-order-passes? ctc (unbox val))))
     #:projection
     (λ (blame)
       (λ (val)
         (let ([proj ((contract-projection ctc) blame)])
           (unless (box? val)
             (raise-blame-error blame val "not a box"))
           (proj (unbox val))
           val))))))
