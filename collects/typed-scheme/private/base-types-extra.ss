#lang scheme/base

(require (for-syntax scheme/base))

(define-syntax (define-other-types stx)
  (syntax-case stx ()
    [(_ nm ...)
     #'(begin (define-syntax nm (lambda (stx) (raise-syntax-error 'type-check "type name used out of context" stx))) ...
              (provide nm) ...)]))

;; special types names that are not bound to particular types

(define-other-types
  -> U mu Un All Opaque Vectorof
  Parameter Tuple Class Values)

(provide (rename-out [All ∀]
                     [mu Rec]))

