#lang racket/base

(require (for-syntax racket/base))

(provide ;; for use in ~literal clauses
         class-internal
         :-augment)

;; give it a binding, but it shouldn't be used directly
(define-syntax (class-internal stx)
  (raise-syntax-error 'class "should only be used internally"))

(define-syntax (:-augment stx)
  (raise-syntax-error 'class "should only be used internally"))
