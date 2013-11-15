#lang racket/base
(require syntax/parse (for-syntax racket/base))
(provide (for-syntax internal))

(define-syntax-rule (internal-forms set-name nms ...)
  (begin
    (provide nms ... set-name)
    (define-literal-set set-name (nms ...))
    (define-syntax (nms stx) (raise-syntax-error 'typecheck "Internal typechecker form used out of context" stx)) ...))

(internal-forms internal-literals
 require/typed-internal
 define-type-alias-internal
 define-type-internal
 define-typed-struct-internal
 define-typed-struct/exec-internal
 assert-predicate-internal
 declare-refinement-internal
 :-internal
 typecheck-fail-internal)

;;; Internal form creation
(begin-for-syntax
  (define (internal stx)
    (quasisyntax/loc stx
      (define-values ()
        (begin
          (quote-syntax #,stx)
          (#%plain-app values))))))
