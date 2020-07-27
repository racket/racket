;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(library (tests compiler-test)
  (export test-one test-all run-main-tests run-final-tests run-all-tests)
  (import (rnrs)
          (tests compiler)
          (tests test-driver)
          (tests alltests))

  (define run-final-tests
    (case-lambda 
      [() (run-final-tests #t)]
      [(emit?) (run-final-tests emit? #f)]
      [(emit? noisy?) (tests final-tests) (test-all emit? noisy?)]))

  (define run-main-tests
    (case-lambda
      [() (run-main-tests #t)]
      [(emit?) (run-main-tests emit? #f)]
      [(emit? noisy?) (tests main-tests) (test-all emit? noisy?)]))

  (define run-all-tests
    (case-lambda
      [() (run-all-tests #t #f)]
      [(emit?) (run-all-tests emit? #f)]
      [(emit? noisy?) (run-main-tests emit? noisy?)
       (run-final-tests emit? noisy?)])) 

  (passes
    (define-passes 
      rename-vars/verify-legal
      remove-implicit-begin
      remove-unquoted-constant
      remove-one-armed-if
      uncover-settable
      remove-impure-letrec
      remove-set!
      sanitize-binding
      remove-anonymous-lambda
      uncover-free
      convert-closure
      lift-letrec
      explicit-closure
      normalize-context
      remove-complex-opera*
      remove-anonymous-call
      introduce-dummy-rp
      remove-nonunary-let
      return-of-set!
      explicit-labels
      ;unparse-l18
      ;introduce-registers
      ;uncover-live
      ;uncover-conflict
      ;uncover-move
      ;assign-register
      ;rename-register
      ;assign-frame
      ;rename-frame
      ;flatten-program
      ;generate-code
      )))
