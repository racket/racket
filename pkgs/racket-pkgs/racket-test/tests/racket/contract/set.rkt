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
   '(set-first (contract (set/c integer?)
                         (set #t)
                         'pos 'neg)))

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
      ((car s) 1))))
