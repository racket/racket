#lang racket/base
(require "test-util.rkt")
(parameterize ([current-contract-namespace
                (make-basic-contract-namespace)])
  
  (test/no-error
   '(let ([v (chaperone-vector (vector-immutable 1)
                               (λ (vec i v) v)
                               (λ (vec i v) v))])
      (contract (vectorof any/c) v 'pos 'neg)))
  
  (test/no-error
   '(let ([v (chaperone-vector (vector-immutable 1)
                               (λ (vec i v) v)
                               (λ (vec i v) v))])
      (contract (vector/c any/c) v 'pos 'neg))))
