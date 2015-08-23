#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 
                 'racket/contract)])

  (contract-eval
   '(module prof-fun racket/base
      (require (only-in racket/contract/private/guts
                        contract-continuation-mark-key)
               (only-in racket/contract/private/blame
                        blame-positive
                        blame-negative
                        blame?))
      (provide pos-blame? neg-blame? named-blame?)
      (define (named-blame? who)
        (define mark-info
          (continuation-mark-set-first
           (current-continuation-marks)
           contract-continuation-mark-key))
        (define (get-party selector)
          (and mark-info
               (or (selector (car mark-info))
                   (cdr mark-info))))
        (and mark-info
             (let ([pos (get-party blame-positive)]
                   [neg (get-party blame-negative)])
               (or (equal? pos who)
                   (equal? neg who)))))
      (define (pos-blame? _) (named-blame? 'pos))
      (define (neg-blame? _) (named-blame? 'neg))))

  (contract-eval '(require 'prof-fun))

  (test/spec-passed
   'provide/contract1
   '((contract (-> neg-blame? any/c) (λ (x) x) 'pos 'neg) 1))

  (test/spec-passed
   'provide/contract2
   '((contract (-> any/c pos-blame?) (λ (x) x) 'pos 'neg) 1))

  (test/spec-passed
   'provide/contract3
   '(contract (vector/c pos-blame?) (vector 1) 'pos 'neg))

  (test/spec-passed
   'provide/contract4
   '((contract (parameter/c pos-blame?) (make-parameter #f) 'pos 'neg)))

  (test/spec-passed
   'provide/contract5
   '(contract (unconstrained-domain-> pos-blame?) (λ () 1) 'pos 'neg))

  (test/spec-passed
   'provide/contract6
   '(contract (->* () #:pre neg-blame? any) (λ () 1) 'pos 'neg))

  (test/spec-passed
   'provide/contract7
   '(contract (->* () any/c #:post pos-blame?) (λ () 1) 'pos 'neg))

  (test/spec-passed/result
   'provide/contract8
   '(let ()
      (eval '(module prof1 racket/base
               (require racket/contract 'prof-fun)
               (define (f x) x)
               (define a-contract (-> (λ _ (named-blame? 'prof1)) any/c))
               (provide
                (contract-out
                 [f a-contract]))))
      (eval '(require 'prof1))
      (eval '(f 11)))
   11)

  (test/spec-passed/result
   'provide/contract9
   '(let ()
      (eval '(module prof2 racket/base
               (require racket/contract 'prof-fun)
               (define (f x) x)
               (provide
                (contract-out
                 [f (-> (λ _ (named-blame? 'prof2)) any/c)]))))
      (eval '(require 'prof2))
      (eval '(f 11)))
   11)

  (test/spec-passed/result
   'provide/contract10
   '(let ()
      (eval '(module prof3 racket/base
               (require racket/contract 'prof-fun)
               (define (f #:x x) x)
               (provide
                (contract-out
                 [f (-> #:x (λ _ (named-blame? 'prof3)) any/c)]))))
      (eval '(require 'prof3))
      (eval '(f #:x 11)))
   11))
