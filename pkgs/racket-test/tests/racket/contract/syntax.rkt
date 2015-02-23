#lang racket/base

(require "test-util.rkt")
(parameterize ([current-contract-namespace
                (make-basic-contract-namespace)])
  
  (test/pos-blame
   'syntax/c1
   '(contract (syntax/c boolean?)
              #'x
              'pos
              'neg))
  
  (test/spec-passed
   'syntax/c2
   '(contract (syntax/c symbol?)
              #'x
              'pos
              'neg))
  
  (test/no-error '(syntax/c (list/c #f)))
  (contract-error-test 'syntax/c-non-flat '(syntax/c (vector/c #f))
                       (λ (x) (regexp-match? #rx"flat-contract[?]" (exn-message x)))))
