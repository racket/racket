#lang scribble/doc
@(require "common.rkt" "std-grammar.rkt" "prim-ops.rkt"
          (for-label lang/htdp-intermediate))


@title[#:tag "intermediate"]{Intermediate Student}

@declare-exporting[lang/htdp-intermediate]

@racketgrammar*+qq[
#:literals (define define-struct lambda cond else if and or require lib planet
            local let let* letrec time check-expect check-random check-satisfied check-within check-error)
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
[expr (local [definition ...] expr)
      (letrec ([name expr-for-let] ...) expr)
      (let ([name expr-for-let] ...) expr)
      (let* ([name expr-for-let] ...) expr)
      (code:line (name expr expr ...) )
      (cond [expr expr] ... [expr expr])
      (cond [expr expr] ... [else expr])
      (if expr expr expr)
      (and expr expr expr ...)
      (or expr expr expr ...)
      (time expr)
      (code:line name)
      (code:line @#,elem{@racketvalfont{'}@racket[_quoted]})
      (code:line @#,elem{@racketvalfont{`}@racket[_quasiquoted]})
      number
      boolean 
      string
      character]
[expr-for-let (lambda (variable variable ...) expr)
              expr]
]

@prim-nonterms[("intermediate") define define-struct]

@prim-variables[("intermediate") empty true false .. ... .... ..... ......]

@; ----------------------------------------------------------------------

@section[#:tag "intermediate-syntax"]{Syntax for Intermediate}


@(intermediate-forms lambda
                     local
                     letrec
                     let*
                     let
                     time
                     define
                     define-struct)

@; ----------------------------------------------------------------------

@section[#:tag "intermediate-common-syntax"]{Common Syntaxes}

The following syntaxes behave the same in the @emph{Intermediate} level as they
did in the @secref["beginner-abbr"] level.

@(beginner-abbr-forms quote quasiquote unquote unquote-splicing)

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
                     check-random
                     check-satisfied
                     check-within
                     check-error
                     check-member-of
                     check-range
                     require
                     true false
                     #:with-beginner-function-call #t)




@section[#:tag "intermediate-pre-defined" ]{Pre-defined Functions}

@(require (submod lang/htdp-intermediate procedures))
@(render-sections (docs) #'here "htdp-intermediate")

@;prim-op-defns['(lib "htdp-intermediate.rkt" "lang") #'here '()]

