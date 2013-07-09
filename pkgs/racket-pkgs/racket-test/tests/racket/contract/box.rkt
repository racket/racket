#lang racket/base
(require "test-util.rkt")
(parameterize ([current-contract-namespace (make-basic-contract-namespace)])
  (test/no-error
   '(let ([v (chaperone-box (box-immutable 1)
                            (λ (box v) v)
                            (λ (box v) v))])
      (contract (box/c any/c) v 'pos 'neg))))
