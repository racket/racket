#lang racket/base

(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace)])
  (test/no-error '(unconstrained-domain-> number?))
  (test/no-error '(unconstrained-domain-> (flat-contract number?)))
  
  
  (test/spec-passed
   'unconstrained-domain->1
   '(contract (unconstrained-domain-> number?) (λ (x) x) 'pos 'neg))
  (test/pos-blame
   'unconstrained-domain->2
   '(contract (unconstrained-domain-> number?) 1 'pos 'neg))
  (test/spec-passed
   'unconstrained-domain->3
   '((contract (unconstrained-domain-> number?) (λ (x) x) 'pos 'neg) 1))
  (test/pos-blame
   'unconstrained-domain->4
   '((contract (unconstrained-domain-> number?) (λ (x) x) 'pos 'neg) #f))
  
  (test/spec-passed/result
   'unconstrained-domain->5
   '((contract (->d ([size natural-number/c]
                     [proc (and/c (unconstrained-domain-> number?)
                                  (λ (p) (procedure-arity-includes? p size)))])
                    ()
                    [range number?])
               (λ (i f) (apply f (build-list i add1)))
               'pos
               'neg)
     10 +)
   55)
  
  (test/spec-passed/result
   'unconstrained-domain->6
   '((contract (unconstrained-domain-> any/c)
               (λ (#:key k) k)
               'pos
               'neg)
     #:key 1)
   1)
  
  (test/pos-blame
   'unconstrained-domain->7
   '((contract (unconstrained-domain-> number?) (λ (#:x x) x) 'pos 'neg) #:x #f)))