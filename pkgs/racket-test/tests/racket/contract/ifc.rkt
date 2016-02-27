#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace)])

  (test/spec-passed/result
   'if/c1
   '(contract (if/c integer? even? (listof number?))
              2
              'pos 'neg)
   2)

  (test/spec-passed/result
   'if/c2
   '(contract (if/c integer? even? (listof number?))
              '()
              'pos 'neg)
   '())

  (test/pos-blame
   'if/c3
   '(contract (if/c integer? even? (listof number?))
              3
              'pos 'neg))

  (test/pos-blame
   'if/c4
   '(contract (if/c integer? even? (listof number?))
              '(#f)
              'pos 'neg))

  (test/pos-blame
   'if/c5
   '(contract (if/c integer? even? (listof number?))
              #f
              'pos 'neg))

  (test/pos-blame
   'if/c6
   '(contract (if/c (λ (x) (and (procedure? x) (procedure-arity-includes? x 1)))
                    (-> integer? integer?)
                    number?)
              #f
              'pos 'neg))

  (test/neg-blame
   'if/c7
   '((contract (if/c (λ (x) (and (procedure? x) (procedure-arity-includes? x 1)))
                     (-> integer? integer?)
                     number?)
               (λ (x) x)
               'pos 'neg)
     #f))

  (test/spec-passed/result
   'if/c8
   '(contract (if/c (λ (x) (and (procedure? x) (procedure-arity-includes? x 1)))
                    (-> integer? integer?)
                    number?)
              1
              'pos 'neg)
   1)

  (test/spec-passed/result
   'if/c9
   '(let ([f (λ (x) x)])
      (chaperone-of?
       (contract (if/c (λ (x) (and (procedure? x) (procedure-arity-includes? x 1)))
                       (-> integer? integer?)
                       number?)
                 f
                 'pos 'neg)
       f))
   #t)

  (test/spec-passed/result
   'if/c10
   '(chaperone-contract?
     (if/c (λ (x) (and (procedure? x) (procedure-arity-includes? x 1)))
           (-> integer? integer?)
           number?))
   #t))


