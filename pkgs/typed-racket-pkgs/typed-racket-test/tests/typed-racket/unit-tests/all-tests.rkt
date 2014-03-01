#lang racket/base

(require
 rackunit
 (for-syntax racket/base syntax/parse racket/syntax))

(provide unit-tests)

(define-syntax define-tests
  (syntax-parser
    [(_ test-name:id files:expr ...)
     (define/with-syntax (new-names ...)
       (generate-temporaries #'(files ...)))
     #'(begin
         (require (only-in files [tests new-names]) ...)
         (define test-name
           (make-test-suite
             "Unit Tests"
             (list new-names ...))))]))

(define-tests unit-tests
  "typecheck-tests.rkt"
  "subtype-tests.rkt"
  "type-equal-tests.rkt"
  "remove-intersect-tests.rkt"
  "static-contract-conversion-tests.rkt"
  "static-contract-optimizer-tests.rkt"
  "parse-type-tests.rkt"
  "subst-tests.rkt"
  "infer-tests.rkt"
  "type-annotation-test.rkt"
  "keyword-expansion-test.rkt"
  "special-env-typecheck-tests.rkt"
  "contract-tests.rkt"
  "interactive-tests.rkt"
  "class-tests.rkt"
  "class-util-tests.rkt"
  "check-below-tests.rkt"
  "rep-tests.rkt")
