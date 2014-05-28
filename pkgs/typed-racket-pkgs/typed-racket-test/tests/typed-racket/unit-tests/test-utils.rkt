#lang racket/base

(require racket/require-syntax
         racket/match
         racket/gui/dynamic
         typed-racket/utils/utils
         (for-syntax racket/base syntax/parse)
         (types utils)
         (rep type-rep)
         rackunit rackunit/text-ui)

(provide private typecheck (rename-out [infer r:infer]) utils env rep types base-env static-contracts
         (all-defined-out))

;; FIXME - do something more intelligent
(define (tc-result-equal/test? a b)
  (equal? a b))

(define-syntax (check-type-equal? stx)
  (syntax-case stx ()
    [(_ nm a b)
     (syntax/loc stx (test-check nm type-equal? a b))]))

(define-syntax gen-test-main
  (syntax-parser
    [(stx:id)
     #`(module* main #f
         (require rackunit/text-ui)
         (void (run-tests #,(datum->syntax #'stx 'tests))))]))
