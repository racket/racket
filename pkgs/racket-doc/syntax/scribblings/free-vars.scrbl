#lang scribble/doc
@(require "common.rkt" (for-label syntax/free-vars) scribble/example)

@title[#:tag "free-vars"]{Computing the Free Variables of an Expression}

@defmodule[syntax/free-vars]

@defproc[(free-vars [expr-stx syntax?]
                    [insp inspector? _mod-decl-insp]
                    [#:module-bound? module-bound? any/c #f])
         (listof identifier?)]{

Returns a list of free @racket[lambda]- and @racket[let]-bound
identifiers in @racket[expr-stx] in the order in which each
identifier first appears within @racket[expr-stx]. The expression must be fully
expanded (see @secref[#:doc refman "fully-expanded"] and
@racket[expand]).

The inspector @racket[insp] is used to disarm @racket[expr-stx] and
sub-expressions before extracting identifiers. The default
@racket[insp] is the declaration-time inspector of the
@racketmodname[syntax/free-vars] module.}

If @racket[module-bound?] is non-false, the list of free variables also
includes free module-bound identifiers.

@examples[
(require (for-syntax racket/base syntax/parse syntax/free-vars))
(define-syntax (print-body-free-vars stx)
  (syntax-parse stx
    #:literals (lambda)
    [(_ (~and lam (lambda (a ...) b ...)))
     (define expanded-body (local-expand #'lam 'expression '()))
     (syntax-parse expanded-body
       #:literals (#%plain-lambda)
       [(#%plain-lambda (arg ...) body)
        (displayln (free-vars #'body))
        expanded-body])]))

(lambda (x) (print-body-free-vars (lambda (y) x)))
]
