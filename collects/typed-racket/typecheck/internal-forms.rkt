#lang racket/base

(require (for-syntax racket/base)
	 syntax/parse)

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

