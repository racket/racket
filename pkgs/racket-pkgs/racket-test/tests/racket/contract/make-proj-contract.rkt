#lang racket/base

(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract)])

  (contract-eval
   '(define proj:add1->sub1
      (make-proj-contract
       'proj:add1->sub1
       (lambda (pos neg src name blame)
         (lambda (f)
           (unless (and (procedure? f) (procedure-arity-includes? f 1))
             (raise-contract-error f src pos name
                                   "expected a unary function, got: ~e"
                                   f))
           (lambda (x)
             (unless (and (integer? x) (exact? x))
               (raise-contract-error x src neg name
                                     "expected an integer, got: ~e"
                                     x))
             (let* ([y (f (add1 x))])
               (unless (and (integer? y) (exact? y))
                 (raise-contract-error y src pos name
                                       "expected an integer, got: ~e"
                                       y))
               (sub1 y)))))
       (lambda (f)
         (and (procedure? f) (procedure-arity-includes? f 1))))))

  (test/spec-passed/result
   'make-proj-contract-1
   '((contract proj:add1->sub1 sqrt 'pos 'neg) 15)
   3)

  (test/pos-blame
   'make-proj-contract-2
   '(contract proj:add1->sub1 'dummy 'pos 'neg))

  (test/pos-blame
   'make-proj-contract-3
   '((contract proj:add1->sub1 (lambda (x) 'dummy) 'pos 'neg) 2))

  (test/neg-blame
   'make-proj-contract-4
   '((contract proj:add1->sub1 sqrt 'pos 'neg) 'dummy)))
