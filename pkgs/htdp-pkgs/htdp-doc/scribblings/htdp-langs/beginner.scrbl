#lang scribble/doc
@(require "common.rkt" "std-grammar.rkt" "prim-ops.rkt"
          (for-label lang/htdp-beginner))


@title[#:tag "beginner"]{Beginning Student}

@declare-exporting[lang/htdp-beginner #:use-sources (lang/htdp-beginner lang/private/teachprims)]

@racketgrammar*+library[
#:literals (define define-struct lambda cond else if and or require lib planet
            check-expect check-random check-satisfied check-within check-error)
(check-expect check-random check-satisfied check-within check-member-of check-range check-error require)
[program (code:line def-or-expr ...)]
[def-or-expr definition
             expr
             test-case             
             library-require]
[definition (define (name variable variable ...) expr)
            (define name expr)
            (define name (lambda (variable variable ...) expr))
            (define-struct name (name ...))]
[expr (code:line (name expr expr ...))
      (cond [expr expr] ... [expr expr])
      (cond [expr expr] ... [else expr])
      (if expr expr expr)
      (and expr expr expr ...)
      (or expr expr expr ...)
      name
      (code:line @#,elem{@racketvalfont{'}@racket[name]})
      number
      boolean 
      string
      character]
]

@prim-nonterms[("beginner") define define-struct]

@prim-variables[("beginner") empty true false .. ... .... ..... ......]

@; --------------------------------------------------

@section[#:tag "beginner-syntax"]{Syntax}

@(define-forms/normal define)
@(define-form/explicit-lambda define lambda)

@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont{'}@racket[name]})]
@defform[(quote name)]
)]{

A quoted @racket[name] is a symbol. A symbol is a value, just like
@racket[0] or @racket[empty].}

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
             check-random
             check-satisfied
             check-within
             check-error
             check-member-of
             check-range
             require
             true false
             #:with-beginner-function-call #t
             )

@; --------------------------------------------------
             
@section[#:tag "beginner-pre-defined"]{Pre-defined Functions}

@(require (submod lang/htdp-beginner procedures))
@(render-sections (docs) #'here "htdp-beginner")

@;prim-op-defns[ #'here '()]
