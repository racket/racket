#lang racket/base

(require "arrow.rkt"
         "guts.rkt")
(provide
 build-env)

(define (make-env)
  (list))

(define (extend-env ctc exp env)
  (cons (list ctc exp)
        env))

(define (build-env f-l)
  (let ([value-contract (dynamic-require 'racket/contract 'value-contract)]
        ;        [->-rngs/c (dynamic-require 'racket/contract/private/arrow '->-rngs/c)]
        ;        [contract-stronger? (dynamic-require 'racket/contract 'contract-stronger?)]
        [env-item (dynamic-require 'racket/contract/private/generator-base 'env-item)]
        ;        [->-rngs/c (dynamic-require 'racket/contract/private/arrow '->-rngs/c)]
        )
    (map (λ (f) 
           (env-item (value-contract f)
                     f))
         f-l))
  )
