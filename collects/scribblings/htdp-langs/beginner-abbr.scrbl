#lang scribble/doc
@(require "common.ss"
          "std-grammar.ss"
          "prim-ops.ss"
          (for-label lang/htdp-beginner-abbr))

@title[#:style 'toc]{Beginner Student with List Abbreviations}

@schemegrammar*+qq[
#:literals (define define-struct lambda cond else if and or empty true false require lib planet)
[program def-or-expr]
[def-or-expr definition
             expr
             library-require]
[definition (define (id id id ...) expr)
            (define id expr)
            (define id (lambda (id id ...) expr))
            (define-struct id (id ...))]
[expr (code:line (id expr expr ...) (code:comment #, @seclink["beginner-call"]{function call}))
      (code:line (prim-op expr ...) (code:comment #, @seclink["beginner-prim-call"]{primitive operation call}))
      (cond [expr expr] ... [expr expr])
      (cond [expr expr] ... [else expr])
      (if expr expr expr)
      (and expr expr expr ...)
      (or expr expr expr ...)
      empty
      id
      (code:line #, @elem{@schemevalfont{'}@scheme[quoted]} (code:comment #, @seclink["beginner-abbr-quote"]{quoted value}))
      (code:line #, @elem{@schemevalfont{`}@scheme[quasiquoted]} (code:comment #, @seclink["beginner-abbr-quasiquote"]{quasiquote}))
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
@scheme[((unsyntax @scheme[quote]) (apple banana))].}

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
@scheme[quasiquote], like @scheme[((unsyntax @scheme[quasiquote])
(apple ,(+ 1 2)))].}


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
