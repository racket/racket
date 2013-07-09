#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace (make-basic-contract-namespace)])
  (test/no-error '(listof any/c))
  (test/no-error '(listof (lambda (x) #t)))
  (test/no-error '(((lambda (x) x) listof) #t))
  (test/no-error '(non-empty-listof any/c))
  (test/no-error '(non-empty-listof (lambda (x) #t)))
  
  (test/no-error '(list/c 'x "x" #t #f #\c #rx"a" #rx#"b"))
  
  (test/pos-blame 'list1 '(contract (list/c 1 2) (list 1 3) 'pos 'neg))
  (test/no-error '(contract (list/c 1 2) (list 1 2) 'pos 'neg)))