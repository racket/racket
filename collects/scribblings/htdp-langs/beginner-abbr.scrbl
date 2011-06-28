#lang scribble/doc
@(require "common.rkt" "std-grammar.rkt" "prim-ops.rkt"
          (for-label lang/htdp-beginner-abbr))

@(define-syntax-rule (bd beg-define beg-define-struct beg-cond beg-if beg-and beg-or beg-check-expect beg-require)
   (begin
    (require (for-label lang/htdp-beginner))
    (define beg-define @racket[define])
    (define beg-define-struct @racket[define-struct])
    (define beg-cond @racket[cond])
    (define beg-if @racket[if])
    (define beg-and @racket[and])
    (define beg-or @racket[or])
    (define beg-check-expect @racket[check-expect])
    (define beg-require @racket[require])))
@(bd beg-define beg-define-struct beg-cond beg-if beg-and beg-or beg-check-expect beg-require)


@title[#:style 'toc #:tag "beginner-abbr"]{Beginning Student with List Abbreviations}

@declare-exporting[lang/htdp-beginner-abbr]

@racketgrammar*+qq[
#:literals (define define-struct lambda cond else if and or empty true false require lib planet
            check-expect check-within check-error)
(check-expect check-within check-error require)
[program (code:line def-or-expr ...)]
[def-or-expr definition
             expr
             test-case
             library-require]
[definition (define (id id id ...) expr)
            (define id expr)
            (define id (lambda (id id ...) expr))
            (define-struct id (id ...))]
[expr (code:line (id expr expr ...) (code:comment @#,seclink["beginner-call"]{function call}))
      (code:line (prim-op expr ...) (code:comment @#,seclink["beginner-prim-call"]{primitive operation call}))
      (cond [expr expr] ... [expr expr])
      (cond [expr expr] ... [else expr])
      (if expr expr expr)
      (and expr expr expr ...)
      (or expr expr expr ...)
      empty
      id
      (code:line @#,elem{@racketvalfont{'}@racket[_quoted]} (code:comment @#,seclink["beginner-abbr-quote"]{quoted value}))
      (code:line @#,elem{@racketvalfont{`}@racket[_quasiquoted]} (code:comment @#,seclink["beginner-abbr-quasiquote"]{quasiquote}))
      number
      true
      false
      string
      character]
]

@|prim-nonterms|

@prim-ops['(lib "htdp-beginner-abbr.rkt" "lang") #'here]

@; ----------------------------------------

@section[#:tag "beginner-abbr-quote"]{Quote}

@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont{'}@racket[quoted]})]
@defform[(quote quoted)]
)]{

Creates symbols and abbreviates nested lists.

Normally, this form is written with a @litchar{'}, like
@racket['(apple banana)], but it can also be written with @racket[quote], like
@racket[(@#,racket[quote] (apple banana))].}

@; ----------------------------------------

@section[#:tag "beginner-abbr-quasiquote"]{Quasiquote}

@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont{`}@racket[quasiquoted]})]
@defform[(quasiquote quasiquoted)]
)]{

Creates symbols and abbreviates nested lists, but also allows escaping
to expression ``unquotes.''

Normally, this form is written with a backquote, @litchar{`}, like
@racket[`(apple ,(+ 1 2))], but it can also be written with
@racket[quasiquote], like
@racket[(@#,racket[quasiquote] (apple ,(+ 1 2)))].}


@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont{,}@racket[quasiquoted]})]
@defform[(unquote expr)]
)]{

Under a single quasiquote, @racketfont{,}@racket[expr] escapes from
the quote to include an evaluated expression whose result is inserted
into the abbreviated list.

Under multiple quasiquotes, @racketfont{,}@racket[expr] is really
@racketfont{,}@racket[quasiquoted], decrementing the quasiquote count
by one for @racket[quasiquoted].

Normally, an unquote is written with @litchar{,}, but it can also be
written with @racket[unquote].}


@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont[",@"]@racket[quasiquoted]})]
@defform[(unquote-splicing expr)]
)]{

Under a single quasiquote, @racketfont[",@"]@racket[expr] escapes from
the quote to include an evaluated expression whose result is a list to
splice into the abbreviated list.

Under multiple quasiquotes, a splicing unquote is like an unquote;
that is, it decrements the quasiquote count by one.

Normally, a splicing unquote is written with @litchar{,}, but it can
also be written with @racket[unquote-splicing].}


@; ----------------------------------------

@section[#:tag "beginner-abbr-prim-ops"]{Primitive Operations}

@prim-op-defns['(lib "htdp-beginner-abbr.rkt" "lang") #'here '()]

@; ----------------------------------------------------------------------

@section{Unchanged Forms}

@deftogether[(
@defform[(define (id id id ...) expr)]
@defform/none[#:literals (define)
              (define id expr)]
@defform/none[#:literals (define lambda)
              (define id (lambda (id id ...) expr))]
@defidform[lambda]
)]{

The same as Beginning's @|beg-define|.}


@defform[(define-struct structid (fieldid ...))]{

The same as Beginning's @|beg-define-struct|.}


@deftogether[(
@defform[(cond [expr expr] ... [expr expr])]
@defidform[else]
)]{

The same as Beginning's @|beg-cond|.}

@defform[(if expr expr expr)]{

The same as Beginning's @|beg-if|.}

@deftogether[(
@defform[(and expr expr expr ...)]
@defform[(or expr expr expr ...)]
)]{

The same as Beginning's @|beg-and| and @|beg-or|.}

@deftogether[(
@defform[(check-expect expr expr)]
@defform[(check-within expr expr expr)]
@defform*[[(check-error expr expr)
           (check-error expr)]]
@defform[(check-member-of expr expr expr ...)]
@defform[(check-range expr expr expr)]
)]{

The same as Beginning's @|beg-check-expect|, etc.}

@deftogether[(
@defthing[empty empty?]
@defthing[true boolean?]
@defthing[false boolean?]
)]{

Constants for the empty list, true, and false.}

@defform[(require module-path)]{

The same as Beginning's @|beg-require|.}
