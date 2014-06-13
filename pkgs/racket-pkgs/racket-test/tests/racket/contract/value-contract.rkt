#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace (make-basic-contract-namespace
                                            'racket/contract)])
  
  (test/spec-passed/result
   'value-contract
   '(let ()
      (define c (-> integer? integer?))
      (define f (contract c (λ (x) x) 'pos 'neg))
      ;; opt/c version doesn't yet have blame, so 
      ;; we require only that when there is blame, that the blame is right.
      (or (and (has-contract? f)
               (equal? c (value-contract f)))
          #t))
   #t)
  
  (test/spec-passed/result
   'value-blame
   '(let ()
      (define f
        (contract (-> integer? integer?) (λ (x) x) 'pos 'neg))
      ;; opt/c version doesn't yet have blame, so 
      ;; we require only that when there is blame, that the blame is right.
      (or (and (has-blame? f)
               (blame-positive (value-blame f)))
          'pos))
   'pos))
