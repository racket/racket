#lang scheme/base

(require "test-utils.rkt"
         (for-syntax scheme/base)
         (for-template scheme/base)
         (private type-contract)
         (types abbrev numeric-tower)
         rackunit)

(define-syntax-rule (t e)
  (test-not-exn (format "~a" e) (lambda () (type->contract e (lambda _ (error "type could not be converted to contract"))))))

(define-syntax-rule (t/fail e)
  (test-not-exn (format "~a" e) (lambda () 
                                  (let/ec exit
                                    (type->contract e (lambda _ (exit #t)))
                                    (error "type could be converted to contract")))))

(define (contract-tests)
  (test-suite "Contract Tests"
              (t (-Number . -> . -Number))
              (t (-Promise -Number))
              (t (-set Univ)) 
              ))

(define-go contract-tests)
(provide contract-tests)
