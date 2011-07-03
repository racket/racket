#lang scribble/doc
@(require "common.rkt" "std-grammar.rkt" "prim-ops.rkt"
          (for-label lang/htdp-beginner-abbr))



@title[#:style 'toc #:tag "beginner-abbr"]{Beginning Student with List Abbreviations}

@declare-exporting[lang/htdp-beginner-abbr]

@racketgrammar*+qq[
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
      (code:line (prim-op expression ...))
      (cond [expression expression] ... [expression expression])
      (cond [expression expression] ... [else expression])
      (if expression expression expression)
      (and expression expression expression ...)
      (or expression expression expression ...)
      name
      (code:line @#,elem{@racketvalfont{'}@racket[_quoted]})
      (code:line @#,elem{@racketvalfont{`}@racket[_quasiquoted]})
      number
      string
      character]
]

@prim-nonterms[("beginner-abbr") define define-struct]

@prim-variables[("beginner-abbr") empty true false]

@; ----------------------------------------

@section[#:tag "beginner-abbr-syntax"]{Syntax for Abbreviations}


@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont{'}@racket[name]})]
@defform/none[(unsyntax @elem{@racketvalfont{'}@racket[part]})]
@defform[(quote name)]
@defform/none[(quote part)]
)]{

A quoted name is a symbol. A quote part is an abbreviation for a nested lists.

Normally, this quotation is written with a @litchar{'}, like
@racket['(apple banana)], but it can also be written with @racket[quote], like
@racket[(@#,racket[quote] (apple banana))].}


@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont{`}@racket[name]})]
@defform/none[(unsyntax @elem{@racketvalfont{`}@racket[part]})]
@defform[(quasiquote name)]
@defform/none[(quasiquote part)]
)]{

Like @racket[quote], but also allows escaping to expression ``unquotes.''

Normally, quasi-quotations are written with a backquote, @litchar{`}, like
@racket[`(apple ,(+ 1 2))], but they can also be written with
@racket[quasiquote], like
@racket[(@#,racket[quasiquote] (apple ,(+ 1 2)))].}


@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont{,}@racket[expression]})]
@defform[(unquote expression)]
)]{

Under a single quasiquote, @racketfont{,}@racket[expression] escapes from
the quote to include an evaluated expression whose value is inserted
into the abbreviated list.

Under multiple quasiquotes, @racketfont{,}@racket[expression] is really
the literal @racketfont{,}@racket[expression], decrementing the quasiquote count
by one for @racket[expression].

Normally, an unquote is written with @litchar{,}, but it can also be
written with @racket[unquote].}


@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont[",@"]@racket[expression]})]
@defform[(unquote-splicing expression)]
)]{

Under a single quasiquote, @racketfont[",@"]@racket[expression] escapes from
the quote to include an evaluated expression whose result is a list to
splice into the abbreviated list.

Under multiple quasiquotes, a splicing unquote is like an unquote;
that is, it decrements the quasiquote count by one.

Normally, a splicing unquote is written with @litchar{,}, but it can
also be written with @racket[unquote-splicing].}


@; ----------------------------------------------------------------------
@section[#:tag "beginner-abbr-common-syntax"]{Common Syntax}

@(define-forms/normal define)
@(define-form/explicit-lambda define lambda)


@prim-forms[("beginner-abbr")
            define 
            lambda
            define-struct
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
            require]


@; ----------------------------------------

@section[#:tag "beginner-abbr-pre-defined"]{Pre-defined Functions}

@prim-op-defns['(lib "htdp-beginner-abbr.ss" "lang") #'here '()]

