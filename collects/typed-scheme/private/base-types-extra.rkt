#lang scheme/base

(require (for-syntax scheme/base))

(define-syntax (define-other-types stx)
  (syntax-case stx ()
    [(_ nm ...)
     #'(begin (define-syntax nm 
                (lambda (stx) 
                  (raise-syntax-error 'type-check "type name used out of context" stx))) ...
              (provide nm) ...)]))

;; special type names that are not bound to particular types
(define-other-types
  -> U Rec All Opaque
  Parameterof List Class Values Instance Refinement
  pred)

(provide (rename-out [All ∀]
                     [U Un]
                     [List Tuple]
                     [Rec mu]
                     [Parameterof Parameter]))

