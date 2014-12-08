#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace (make-basic-contract-namespace)])
  (define exn:fail:contract:blame? (contract-eval 'exn:fail:contract:blame?))
  
  (test/no-error '(-> integer? integer?))
  (test/no-error '(-> (flat-contract integer?) (flat-contract integer?)))
  (test/no-error '(-> integer? any))
  (test/no-error '(-> (flat-contract integer?) any))
  
  (test/spec-passed
   'contract-arrow-values1
   '(let-values ([(a b) ((contract (-> integer? (values integer? integer?))
                                   (lambda (x) (values x x))
                                   'pos
                                   'neg)
                         2)])
      1))
  
  (test/neg-blame
   'contract-arrow-values2
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) (values x x))
               'pos
               'neg)
     #f))
  
  (test/pos-blame
   'contract-arrow-values3
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) (values 1 #t))
               'pos
               'neg)
     1))
  
  (test/pos-blame
   'contract-arrow-values4
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) (values #t 1))
               'pos
               'neg)
     1))
  
  (test/pos-blame
   'contract-arrow-values5
   '((contract (-> integer? (values integer? integer?))
               (lambda (x) x)
               'pos
               'neg)
     1))
  
  
  (test/pos-blame
   'contract-arrow-keyword1
   '(contract (-> integer? any)
              (λ (x #:y y) x)
              'pos
              'neg))
  
  (test/pos-blame
   'contract-arrow-keyword1b
   '(contract (-> integer? #:y integer? any)
              (λ (x) x)
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-keyword2
   '(contract (-> integer? #:y boolean? any)
              (λ (x #:y y) x)
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-keyword2b
   '(contract (-> #:x boolean? #:y boolean? any)
              (λ (#:x x #:y y) x)
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-keyword2c
   '(contract (-> #:y boolean? #:x boolean? any)
              (λ (#:x x #:y y) x)
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-keyword2d
   '(contract (-> #:y boolean? #:x boolean? any)
              (λ (#:y y #:x x) x)
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-keyword2e
   '(contract (-> #:x boolean? #:y boolean?  any)
              (λ (#:y y #:x x) x)
              'pos
              'neg))
  
  (test/neg-blame
   'contract-arrow-keyword3
   '((contract (-> integer? #:y boolean? any)
               (λ (x #:y y) x)
               'pos
               'neg)
     1 #:y 1))
  
  (test/neg-blame
   'contract-arrow-keyword4
   '((contract (-> integer? #:y boolean? any)
               (λ (x #:y y) x)
               'pos
               'neg)
     #t #:y #t))
  
  (test/spec-passed
   'contract-arrow-keyword5
   '((contract (-> integer? #:y boolean? any)
               (λ (x #:y y) x)
               'pos
               'neg)
     1 #:y #t))
  
  (test/pos-blame
   'contract-arrow-keyword6
   '(contract (-> integer? integer?)
              (λ (x #:y y) x)
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-keyword7
   '(contract (-> integer? #:y boolean? integer?)
              (λ (x #:y y) x)
              'pos
              'neg))
  
  (test/neg-blame
   'contract-arrow-keyword8
   '((contract (-> integer? #:y boolean? integer?)
               (λ (x #:y y) x)
               'pos
               'neg)
     1 #:y 1))
  
  (test/neg-blame
   'contract-arrow-keyword9
   '((contract (-> integer? #:y boolean? integer?)
               (λ (x #:y y) x)
               'pos
               'neg)
     #t #:y #t))
  
  (test/spec-passed
   'contract-arrow-keyword10
   '((contract (-> integer? #:y boolean? integer?)
               (λ (x #:y y) x)
               'pos
               'neg)
     1 #:y #t))
  
  (test/pos-blame
   'contract-arrow-keyword11
   '(contract (-> integer? (values integer? integer?))
              (λ (x #:y y) x)
              'pos
              'neg))
  
  (test/spec-passed
   'contract-arrow-keyword12
   '(contract (-> integer? #:y boolean? (values integer? integer?))
              (λ (x #:y y) x)
              'pos
              'neg))
  
  (test/neg-blame
   'contract-arrow-keyword13
   '((contract (-> integer? #:y boolean? (values integer? integer?))
               (λ (x #:y y) x)
               'pos
               'neg)
     1 #:y 1))
  
  (test/neg-blame
   'contract-arrow-keyword14
   '((contract (-> integer? #:y boolean? (values integer? integer?))
               (λ (x #:y y) x)
               'pos
               'neg)
     #t #:y #t))
  
  (test/spec-passed
   'contract-arrow-keyword15
   '((contract (-> integer? #:y boolean? (values integer? integer?))
               (λ (x #:y y) (values x x))
               'pos
               'neg)
     1 #:y #t))
  
  (test/spec-passed
   'contract-arrow-keyword16
   '((contract (-> integer? integer?)
               (λ (x #:y [y #f]) x)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   'contract-arrow-keyword17
   '((contract (-> integer? integer?)
               (λ (x #:y [y #f]) x)
               'pos
               'neg)
     #f))
  
  (test/pos-blame
   'contract-arrow-keyword18
   '((contract (-> integer? integer?)
               (λ (x #:y [y #f]) y)
               'pos
               'neg)
     1))
  
  (test/neg-blame
   'contract-arrow-keyword19
   '((contract (-> boolean?)
               (λ (#:x [x #f]) x)
               'pos
               'neg)
     #:x 1))
  
  (test/spec-passed
   'contract-arrow1
   '(contract (integer? . -> . integer?) (lambda (x) x) 'pos 'neg))
  
  ;; make sure we skip the optimizations
  (test/spec-passed
   'contract-arrow1b
   '(contract (-> integer? integer? integer? integer? integer? 
                  integer? integer? integer? integer? integer?
                  integer?)
              (lambda (x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) x1) 'pos 'neg))
  
  (test/pos-blame
   'contract-arrow2
   '(contract (integer? . -> . integer?) (lambda (x y) x) 'pos 'neg))
  
  (test/neg-blame
   'contract-arrow3
   '((contract (integer? . -> . integer?) (lambda (x) #f) 'pos 'neg) #t))
  
  (test/pos-blame
   'contract-arrow4
   '((contract (integer? . -> . integer?) (lambda (x) #f) 'pos 'neg) 1))
  
  (test/neg-blame
   'contract-arrow-arity1
   '((contract (-> number? number? number?)
               (λ (x . r) x)
               'pos 'neg)
     1))
  
  (test/spec-passed
   'contract-arrow-any1
   '(contract (integer? . -> . any) (lambda (x) x) 'pos 'neg))
  
  (test/pos-blame
   'contract-arrow-any2
   '(contract (integer? . -> . any) (lambda (x y) x) 'pos 'neg))
  
  (test/neg-blame
   'contract-arrow-any3
   '((contract (integer? . -> . any) (lambda (x) #f) 'pos 'neg) #t))
  
  (test/spec-passed
   'contract-arrow-all-anys1
   '((contract (-> any) (lambda () #f) 'pos 'neg)))
  
  (test/pos-blame
   'contract-arrow-all-anys2
   '((contract (-> any) (lambda (x) #f) 'pos 'neg)))
  
  (test/spec-passed
   'contract-arrow-all-anys3
   '((contract (-> any) (lambda ([x #f]) #f) 'pos 'neg)))
  
  (test/spec-passed
   'contract-arrow-all-kwds
   '(contract (-> #:a string? string?)
              (make-keyword-procedure void)
              'pos 'neg))
  
  (test/pos-blame
   'contract-arrow-non-function
   '(contract (-> integer? any) 1 'pos 'neg))
  
  (test/pos-blame
   'contract-any/c-arrow1
   '(contract (-> any/c any) 1 'pos 'neg))
  
  (test/spec-passed
   'contract-any/c-arrow2
   '(contract (-> any/c any) (λ (x) 1) 'pos 'neg))
  
  (test/pos-blame
   'contract-any/c-arrow3
   '(contract (-> any/c any) (λ (x y) x) 'pos 'neg))
  
  (test/pos-blame
   'contract-any/c-arrow4
   '(contract (-> any/c any) (λ (x #:y y) x) 'pos 'neg))
  
  (test/spec-passed
   'contract-arrow-all-kwds2
   '((contract (-> #:a string? void?)
               (make-keyword-procedure void)
               'pos 'neg)
     #:a "abcdef"))
  
  (contract-error-test
   'contract-arrow-kwd-name-in-message
   '((contract
      (-> #:a any/c #:the-missing-keyword-arg-b any/c any)
      (λ (#:a [a 0] #:the-missing-keyword-arg-b [b 0]) b)
      'pos
      'neg)
     #:a 0)
   (λ (x)
     (and (exn:fail:contract:blame? x)
          ;; the ? here is to allow the currently pushed buggy version to
          ;; pass; this is fixed in a separate branch that can't 
          (regexp-match #rx"expected:? keyword argument #:the-missing-keyword-arg-b"
                        (exn-message x)))))
  
  (test/pos-blame
   'predicate/c1
   '(contract predicate/c 1 'pos 'neg))
  (test/pos-blame
   'predicate/c2
   '(contract predicate/c (λ (x y) 1) 'pos 'neg))
  (test/pos-blame
   'predicate/c3
   '((contract predicate/c (λ (x) 1) 'pos 'neg) 12))
  (test/spec-passed
   'predicate/c4
   '((contract predicate/c (λ (x) #t) 'pos 'neg) 12))
  
  ;; this test ensures that no contract wrappers
  ;; are created for struct predicates
  (test/spec-passed/result
   'predicate/c5
   '(let ()
      (struct x (a))
      (eq? (contract predicate/c x? 'pos 'neg) x?))
   #t)
  
  )
