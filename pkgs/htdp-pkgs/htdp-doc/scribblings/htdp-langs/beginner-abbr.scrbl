#lang scribble/doc

@(require "common.rkt" "std-grammar.rkt" "prim-ops.rkt" (for-label lang/htdp-beginner-abbr))

@title[#:tag "beginner-abbr"]{Beginning Student with List Abbreviations}

@declare-exporting[lang/htdp-beginner-abbr]

@racketgrammar*+qq[
#:literals (define define-struct lambda cond else if and or require lib planet
            check-expect check-within check-error)
(check-expect check-within check-member-of check-range check-error require)
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
      (code:line (prim-op expr ...))
      (cond [expr expr] ... [expr expr])
      (cond [expr expr] ... [else expr])
      (if expr expr expr)
      (and expr expr expr ...)
      (or expr expr expr ...)
      name
      (code:line @#,elem{@racketvalfont{'}@racket[_quoted]})
      (code:line @#,elem{@racketvalfont{`}@racket[_quasiquoted]})
      number
      boolean
      string
      character]
]

@prim-nonterms[("beginner-abbr") define define-struct]

@prim-variables[("beginner-abbr") empty true false .. ... .... ..... ......]

@; ----------------------------------------

@section[#:tag "beginner-abbr-syntax"]{Syntaxes for Beginning Student with List Abbreviations}

@(beginner-abbr-forms quote quasiquote unquote unquote-splicing)



@; ----------------------------------------------------------------------
@section[#:tag "beginner-abbr-common-syntax"]{Common Syntaxes}

The following syntaxes behave the same in the @emph{Beginner with List
Abbreviations} level as they did in the @secref["beginner"] level.

@(define-forms/normal define)
@(define-form/explicit-lambda define lambda)


@prim-forms[("beginner-abbr")
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
            true false
             #:with-beginner-function-call #t]


@; ----------------------------------------

@section[#:tag "beginner-abbr-pre-defined"]{Pre-defined Functions}

@(require (submod lang/htdp-beginner-abbr procedures))
@(render-sections (docs) #'here "htdp-beginner-abbr")

@;prim-op-defns['(lib "htdp-beginner-abbr.rkt" "lang") #'here '()]
