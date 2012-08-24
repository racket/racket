#lang scheme/base

(require "test-utils.rkt"
         (for-syntax scheme/base)
         (for-template scheme/base)
         (private type-contract)
         (types abbrev numeric-tower)
         rackunit)

(define-syntax-rule (t e)
  (test-not-exn (format "~a" e) (lambda () (type->contract e (lambda _ (error "type could not be converted to contract"))))))

(define (contract-tests)
  (test-suite "Contract Tests"
              (t (-Number . -> . -Number))
              (t (-Promise -Number))))

(define-go contract-tests)
(provide contract-tests)
