#lang scheme/base

(require scheme/require-syntax
         scheme/match
         scheme/gui/dynamic
         typed-racket/utils/utils
         (for-syntax scheme/base)
         (types utils)
         (rep type-rep)
         rackunit rackunit/text-ui)

(provide private typecheck (rename-out [infer r:infer]) utils env rep types base-env
         (all-defined-out))

;; FIXME - do something more intelligent
(define (tc-result-equal/test? a b)
  (equal? a b))

(define-syntax (check-type-equal? stx)
  (syntax-case stx ()
    [(_ nm a b)
     (syntax/loc stx (test-check nm type-equal? a b))]))
(define-binary-check (check-tc-result-equal?* tc-result-equal/test? a b))
(define-syntax (check-tc-result-equal? stx)
  (syntax-case stx ()
    [(_ nm a b)
     (syntax/loc stx (test-case nm (check-tc-result-equal?* a b)))]))
