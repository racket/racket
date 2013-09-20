#lang racket/base

(require (for-syntax racket/base syntax/stx))

(define-syntax (define-other-types stx)
  (syntax-case stx ()
    [(_ nm ...)
     #'(begin (define-syntax nm
                (lambda (stx)
                  (raise-syntax-error 'type-check "type name used out of context"
                                      stx
                                      (and (stx-pair? stx) (stx-car stx)))))
              ...
              (provide nm) ...)]))

;; special type names that are not bound to particular types
(define-other-types
  -> case-> U Rec All Opaque Vector
  Parameterof List List* Class Values Instance Refinement
  pred Struct)

(provide (rename-out [All ∀]
                     [U Un]
                     [-> →]
                     [case-> case→]
                     [List Tuple]
                     [Rec mu]
                     [Parameterof Parameter]))

