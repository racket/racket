#lang racket/base

(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract)])
  
  (test/spec-passed/result
   'parametric->/c1
   '((contract (parametric->/c (A) (-> A A))
               (λ (x) x)
               'pos 'neg)
     1)
   1)
  
  (test/pos-blame
   'parametric->/c2
   '((contract (parametric->/c (A) (-> A A))
               (λ (x) 1)
               'pos 'neg)
     1))
  
  (test/pos-blame
   'parametric->/c3
   '((contract (parametric->/c (A B) (-> A B))
               (λ (x) x)
               'pos 'neg)
     1))
  
  ;; this should (probably) not require the
  ;; application to 1, but parametric->/c
  ;; currently checks only that its argument
  ;; is a procedure and otherwise delays any
  ;; checking until the first application of
  ;; the wrapper
  (test/pos-blame
   'parametric->/c4
   '((contract (parametric->/c (A) (-> A A))
               (λ (x y) x)
               'pos 'neg)
     1))
  
  (test/pos-blame
   'parametric->/c5
   '(contract (parametric->/c (A) (-> A A))
              5
              'pos 'neg))
  
  (test/spec-passed/result
   'parametric->/c6
   '((contract (parametric->/c (A B) (-> A B (or/c A B)))
               (λ (x y) x)
               'pos 'neg)
     1 "foo")
   1)
  
  (test/pos-blame
   'parametric->/c7
   '(let* ([c #f]
           [f (contract
               (parametric->/c (x) (-> x x))
               (λ (x) (unless c (set! c x)) c)
               'pos 'neg)])
      (f 1)
      (f 2))))
