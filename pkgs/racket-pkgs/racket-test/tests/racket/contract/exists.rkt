#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract)])

  (test/spec-passed
   '∃1
   '(contract (new-∃/c 'pair)
              1
              'pos
              'neg))

  (test/neg-blame
   '∃2
   '((contract (-> (new-∃/c 'pair) any/c)
               (λ (x) x)
               'pos
               'neg)
     1))

  (test/spec-passed/result
   '∃3
   '(let ([pair (new-∃/c 'pair)])
      ((contract (-> (-> pair pair) any/c)
                 (λ (f) (f 11))
                 'pos
                 'neg)
       (λ (x) x)))
   11)

  (test/pos-blame
   '∀1
   '(contract (new-∀/c 'pair)
              1
              'pos
              'neg))

  (test/spec-passed
   '∀2
   '((contract (-> (new-∀/c 'pair) any/c)
               (λ (x) x)
               'pos
               'neg)
     1))

  (test/spec-passed/result
   '∀3
   '(let ([pair (new-∀/c 'pair)])
      ((contract (-> pair pair)
                 (λ (x) x)
                 'pos
                 'neg)
       11))
   11))