#lang racket/base
(require "test-util.rkt"
         (for-syntax racket/base))

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract 'racket/list)])

  (test/spec-failed
   'contract-in1
   '(let ()
      (eval '(module contract-in-test-suite1 racket/base
               (require racket/contract
                        (contract-in racket/list
                                     [first (-> boolean? any)]))
               (first "wrong")))
      (eval '(require 'contract-in-test-suite1)))
   "contract-in-test-suite1")

  (test/spec-failed
   'contract-in2
   '(let ()
      (eval '(module contract-in-test-suite2 racket/base
               (require racket/contract
                        (contract-in racket/list
                                     [first (-> any/c boolean?)]))
               (first (list 1))))
      (eval '(require 'contract-in-test-suite2)))
   "racket/list")

  (void))
