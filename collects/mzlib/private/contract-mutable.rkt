#lang racket/base

(require (only-in racket/contract/private/box box-immutable/c)
         (only-in racket/contract/private/vector
                  vector/c vector-immutableof vector-immutable/c)
         racket/contract/private/blame
         racket/contract/private/guts)

(provide box/c box-immutable/c
         vector/c vectorof vector-immutableof vector-immutable/c)

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

(define/subexpression-pos-prop (vectorof ctc)
  (let ([ctc (coerce-flat-contract 'vectorof ctc)])
    (make-flat-contract
     #:name (build-compound-type-name 'vectorof ctc)
     #:first-order
     (λ (val)
       (and (vector? val)
            (for/and ([v (in-vector val)])
              (contract-first-order-passes? ctc v))))
     #:projection
     (λ (blame)
       (λ (val)
         (let ([proj ((contract-projection ctc) blame)])
           (unless (vector? val)
             (raise-blame-error blame val "not a vector"))
           (for ([v (in-vector val)])
             (proj v))
           val))))))
