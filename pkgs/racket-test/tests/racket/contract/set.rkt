#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/set)])

  (test/spec-passed/result
   'set/c1
   '(contract (set/c integer?)
              (set 0)
              'pos 'neg)
   (contract-eval '(set 0)))

  (test/pos-blame
   'set/c2
   '(contract (set/c integer?)
              (set #t)
              'pos 'neg))

  (test/pos-blame
   'set/c3
   '(contract (set/c integer? #:cmp 'eq)
              (set 0)
              'pos 'neg))

  (test/pos-blame
   'set/c4
   '(contract (set/c integer? #:cmp 'eqv)
              (set 0)
              'pos 'neg))

  (test/pos-blame
   'set/c5
   '(contract (set/c integer? #:cmp 'equal)
              (seteq 0)
              'pos 'neg))

  (test/spec-passed/result
   'set/c6
   '(set-map (contract (set/c integer?)
                       (set 0)
                       'pos 'neg)
             values)
   (list 0))

  (test/neg-blame
   'set/c7
   '(let ([s (set-map (contract (set/c (-> integer? integer?))
                                (set (λ (x) #f))
                                'pos 'neg)
                      values)])
      ((car s) #f)))

  (test/pos-blame
   'set/c8
   '(let ([s (set-map (contract (set/c (-> integer? integer?))
                                (set (λ (x) #f))
                                'pos 'neg)
                      values)])
      ((car s) 1)))

  (test/pos-blame
   'set/c9
   '(contract (set/c integer?)
              (list 0)
              'pos 'neg))

  (test/pos-blame
   'set/c10
   '(contract (set/c integer?)
              (mutable-set 0)
              'pos 'neg))

  (test/spec-passed/result
   'set/c11
   '(contract (set/c integer? #:kind 'dont-care)
              (list 0)
              'pos 'neg)
   (list 0))

  (test/spec-passed/result
   'set/c12
   '(contract (set/c integer? #:kind 'dont-care)
              (set 0)
              'pos 'neg)
   (contract-eval '(set 0)))

  (test/spec-passed/result
   'set/c13
   '(contract (set/c integer? #:kind 'dont-care)
              (mutable-set 0)
              'pos 'neg)
   (contract-eval '(mutable-set 0)))

  (test/pos-blame
   'set/c14
   '(contract (set/c integer? #:kind 'mutable)
              (list 0)
              'pos 'neg))

  (test/pos-blame
   'set/c15
   '(contract (set/c integer? #:kind 'mutable)
              (set 0)
              'pos 'neg))

  (test/spec-passed/result
   'set/c16
   '(contract (set/c integer? #:kind 'mutable)
              (mutable-set 0)
              'pos 'neg)
   (contract-eval '(mutable-set 0))))
