#lang scribble/doc
@(require "common.rkt" "std-grammar.rkt" "prim-ops.rkt"
          (for-label lang/htdp-beginner))


@title[#:style 'toc #:tag "beginner"]{Beginning Student}

@declare-exporting[lang/htdp-beginner #:use-sources (lang/htdp-beginner lang/private/teachprims)]

@racketgrammar*+library[
#:literals (define define-struct lambda cond else if and or require lib planet
            check-expect check-within check-error)
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
[expression (code:line (name expression expression ...))
      (cond [expression expression] ... [expression expression])
      (cond [expression expression] ... [else expression])
      (if expression expression expression)
      (and expression expression expression ...)
      (or expression expression expression ...)
      name
      (code:line @#,elem{@racketvalfont{'}@racket[name]})
      number
      string
      character]
]

@prim-nonterms[("beginner") define define-struct]

@prim-variables[("beginner") empty true false]

@; --------------------------------------------------

@section[#:tag "beginner-syntax"]{Syntax}

@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont{'}@racket[name]})]
@defform[(quote name)]
)]{

A quoted @racket[name] is a symbol. A symbol is a value, just like
@racket[0] or @racket[empty].}

@(define-forms/normal define)
@(define-form/explicit-lambda define lambda)

@(prim-forms ("beginner")
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

@; --------------------------------------------------
             
@section[#:tag "beginner-pre-defined"]{Pre-defined Functions}

@prim-op-defns['(lib "htdp-beginner.rkt" "lang") #'here '()]
