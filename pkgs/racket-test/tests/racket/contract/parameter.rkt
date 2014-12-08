#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace)])
  
  (test/neg-blame
   'parameter/c1
   '((contract (parameter/c integer?)
               (make-parameter 1)
               'pos 'neg)
     #f))
  
  (test/pos-blame
   'parameter/c2
   '((contract (parameter/c integer?)
               (make-parameter 'not-an-int)
               'pos 'neg)))
  
  (test/pos-blame
   'parameter/c3
   '((contract (parameter/c integer? string?)
               (make-parameter 'not-an-int number->string)
               'pos 'neg)))
  
  (test/neg-blame
   'parameter/c4
   '((contract (parameter/c integer? string?)
               (make-parameter 5 number->string)
               'pos 'neg)
     'not-an-int))
  
  (test/spec-passed
   'parameter/c5
   '((contract (parameter/c integer? string?)
               (make-parameter "foo" number->string)
               'pos 'neg)))
  
  (test/spec-passed
   'parameter/c6
   '((contract (parameter/c integer? string?)
               (make-parameter "foo" number->string)
               'pos 'neg)
     5))
  
  (test/pos-blame
   'parameter/c7
   '((contract (parameter/c integer? string?)
               (make-parameter 5 values)
               'pos 'neg))))