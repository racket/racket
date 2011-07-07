#lang scribble/doc
@(require "common.rkt" "std-grammar.rkt" "prim-ops.rkt"
          (for-label lang/htdp-intermediate))


@title[#:style 'toc #:tag "intermediate"]{Intermediate Student}

@declare-exporting[lang/htdp-intermediate]

@racketgrammar*+qq[
#:literals (define define-struct lambda cond else if and or require lib planet
            local let let* letrec time check-expect check-within check-error)
(check-expect check-within check-member-of check-range check-error require)
[program (code:line def-or-expr ...)]
[def-or-expr definition
             expression
             test-case
             library-require]
[definition (define (name variable variable ...) expression)
            (define name expression)
            (define name (lambda (variable variable ...) expression))
            (define-struct name (name ...))]
[expression (local [definition ...] expression)
      (letrec ([name expr-for-let] ...) expression)
      (let ([name expr-for-let] ...) expression)
      (let* ([name expr-for-let] ...) expression)
      (code:line (name expression expression ...) )
      (cond [expression expression] ... [expression expression])
      (cond [expression expression] ... [else expression])
      (if expression expression expression)
      (and expression expression expression ...)
      (or expression expression expression ...)
      (time expression)
      (code:line name)
      (code:line @#,elem{@racketvalfont{'}@racket[_quoted]})
      (code:line @#,elem{@racketvalfont{`}@racket[_quasiquoted]})
      number
      string
      character]
[expr-for-let (lambda (variable variable ...) expression)
              expression]
]

@prim-nonterms[("intermediate") define define-struct]

@prim-variables[("intermediate") empty true false]


@; ----------------------------------------------------------------------

@section[#:tag "intermediate-syntax"]{Syntax for Intermediate}

@(intermediate-forms lambda
                     quote
                     quasiquote
                     unquote
                     unquote-splicing
                     local
                     letrec
                     let*
                     let
                     time)

@; ----------------------------------------------------------------------

@section[#:tag "intermediate-common-syntax"]{Common Syntax}

@(define-forms/normal define)
@(define-form/explicit-lambda define lambda)

@(prim-forms 
                     ("intermediate")
                     define 
                     lambda
                     define-struct []
                     define-wish
                     cond
                     else
                     if
                     and 
                     or
                     check-expect
                     check-within
                     check-error
                     check-member-of
                     check-range
                     require
                     true false)




@section[#:tag "intermediate-pre-defined"]{Pre-defined Functions}

@prim-op-defns['(lib "htdp-intermediate.rkt" "lang") #'here '()]

