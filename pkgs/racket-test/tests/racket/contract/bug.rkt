#lang racket/base
(require "test-util.rkt"
         (for-syntax racket/base))

#|

This file contains tests that test for unwanted
behavior. That is, if you were to improve
the contract library in some way and find that
tests in here were failing, that would probably
be a Good Thing.

Each test comes with a commented out explanation
of the desired behavior.

|#

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract)])
  
  (define exn:fail:contract:blame?
    (contract-eval 'exn:fail:contract:blame?))
  
  (contract-error-test
   'bad1
   '(begin
      (eval '(module bad1-server racket
               (provide (contract-out [bad1-boo (-> number? number?)]))
               (define (bad1-boo x) x)))
      
      (eval '(module bad1-client racket
               (require 'bad1-server)
               (provide bad1-boo)))
      
      (eval '(require 'bad1-client))
      (eval '(bad1-boo 'wrong)))
   (λ (x)
     (and (exn:fail:contract:blame? x)
          (regexp-match? #rx"blaming: top-level"
                         ;; the regexp should be #rx"blaming: bad1-client"
                         (exn-message x)))))
  
  ;; this is a case where recursive-contract cannot easily tell 
  ;; that it has found a cycle. It's currently supposed to detect
  ;; cycles and signal errors, but it doesn't detect this one.
  (test/spec-passed
   'recursive-contract14
   '(let ()
      (struct s (x) #:mutable)
      (define an-s (s #f))
      (set-s-x! an-s an-s)
      
      (define c
        (recursive-contract
         (struct/c s c)))
      
      (s-x (s-x (contract c an-s 'pos 'neg))))))
