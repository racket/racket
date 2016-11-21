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
   '((contract (unconstrained-domain-> number?) (λ (#:x x) x) 'pos 'neg) #:x #f))

  (test/spec-passed/result
   'unconstrained-domain->8
   '(let ([f (λ (x) 0)])
      (eq? (contract (unconstrained-domain-> any/c)
                     f
                     'pos
                     'neg)
           f))
   #t)

  (test/pos-blame
   'unconstrained-domain->9
   '((contract (unconstrained-domain-> number? number?) (λ () (values #f 0)) 'pos 'neg)))

  (test/pos-blame
   'unconstrained-domain->10
   '((contract (unconstrained-domain-> number? number?) (λ () (values 0 #f)) 'pos 'neg)))

  (test/pos-blame
   'unconstrained-domain->11
   '((contract (unconstrained-domain-> number? number?) (λ () 1) 'pos 'neg)))

  (test/spec-passed/result
   'unconstrained-domain->12
   '(and (value-contract (contract (unconstrained-domain-> number?)
                                   (lambda (x) 1)
                                   'pos
                                   'neg))
         #t)
   #t)
  (test/spec-passed/result
   'unconstrained-domain->13
   '(and (value-blame (contract (unconstrained-domain-> number?)
                                (lambda (x) 1)
                                'pos
                                'neg))
         #t)
   #t)
  )
