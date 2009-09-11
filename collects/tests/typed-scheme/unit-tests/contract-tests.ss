#lang scheme/base

(require "test-utils.ss" "planet-requires.ss"
         (for-syntax scheme/base)
         (for-template scheme/base)
         (private type-contract)	 
	 (rep type-rep filter-rep object-rep)
         (types utils union convenience)
         (utils tc-utils mutated-vars)
         (schemeunit))

(define-syntax-rule (t e)
  (test-not-exn (format "~a" e) (lambda () (type->contract e (lambda _ (error "type could not be converted to contract"))))))

(define (contract-tests)
  (test-suite "Contract Tests"
              (t (-Number . -> . -Number))))

(define-go contract-tests)
(provide contract-tests)
