#lang scribble/doc
@(require "common.ss"
          "std-grammar.ss"
          "prim-ops.ss"
          (for-label lang/htdp-beginner-abbr))

@(define-syntax-rule (bd beg-define beg-define-struct beg-cond beg-if beg-and beg-or beg-check-expect beg-require)
   (begin
    (require (for-label lang/htdp-beginner))
    (define beg-define (scheme define))
    (define beg-define-struct (scheme define-struct))
    (define beg-cond (scheme cond))
    (define beg-if (scheme if))
    (define beg-and (scheme and))
    (define beg-or (scheme or))
    (define beg-check-expect (scheme check-expect))
    (define beg-require (scheme require))))
@(bd beg-define beg-define-struct beg-cond beg-if beg-and beg-or beg-check-expect beg-require)


@title[#:style 'toc #:tag "beginner-abbr"]{Beginning Student with List Abbreviations}

@declare-exporting[lang/htdp-beginner-abbr]

@schemegrammar*+qq[
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
      (code:line @#,elem{@schemevalfont{'}@scheme[_quoted]} (code:comment @#,seclink["beginner-abbr-quote"]{quoted value}))
      (code:line @#,elem{@schemevalfont{`}@scheme[_quasiquoted]} (code:comment @#,seclink["beginner-abbr-quasiquote"]{quasiquote}))
      number
      true
      false
      string
      character]
]

@|prim-nonterms|

@prim-ops['(lib "htdp-beginner-abbr.ss" "lang") #'here]

@; ----------------------------------------

@section[#:tag "beginner-abbr-quote"]{Quote}

@deftogether[(
@defform/none[(unsyntax @elem{@schemevalfont{'}@scheme[quoted]})]
@defform[(quote quoted)]
)]{

Creates symbols and abbreviates nested lists.

Normally, this form is written with a @litchar{'}, like
@scheme['(apple banana)], but it can also be written with @scheme[quote], like
@scheme[(@#,scheme[quote] (apple banana))].}

@; ----------------------------------------

@section[#:tag "beginner-abbr-quasiquote"]{Quasiquote}

@deftogether[(
@defform/none[(unsyntax @elem{@schemevalfont{`}@scheme[quasiquoted]})]
@defform[(quasiquote quasiquoted)]
)]{

Creates symbols and abbreviates nested lists, but also allows escaping
to expression ``unquotes.''

Normally, this form is written with a backquote, @litchar{`}, like
@scheme[`(apple ,(+ 1 2))], but it can also be written with
@scheme[quasiquote], like
@scheme[(@#,scheme[quasiquote] (apple ,(+ 1 2)))].}


@deftogether[(
@defform/none[(unsyntax @elem{@schemevalfont{,}@scheme[quasiquoted]})]
@defform[(unquote expr)]
)]{

Under a single quasiquote, @schemefont{,}@scheme[expr] escapes from
the quote to include an evaluated expression whose result is inserted
into the abbreviated list.

Under multiple quasiquotes, @schemefont{,}@scheme[expr] is really
@schemefont{,}@scheme[quasiquoted], decrementing the quasiquote count
by one for @scheme[quasiquoted].

Normally, an unquote is written with @litchar{,}, but it can also be
written with @scheme[unquote].}


@deftogether[(
@defform/none[(unsyntax @elem{@schemevalfont[",@"]@scheme[quasiquoted]})]
@defform[(unquote-splicing expr)]
)]{

Under a single quasiquote, @schemefont[",@"]@scheme[expr] escapes from
the quote to include an evaluated expression whose result is a list to
splice into the abbreviated list.

Under multiple quasiquotes, a splicing unquote is like an unquote;
that is, it decrements the quasiquote count by one.

Normally, a splicing unquote is written with @litchar{,}, but it can
also be written with @scheme[unquote-splicing].}


@; ----------------------------------------

@section[#:tag "beginner-abbr-prim-ops"]{Primitive Operations}

@prim-op-defns['(lib "htdp-beginner-abbr.ss" "lang") #'here '()]

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
