#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@(define qq (racket quasiquote))
@(define uq (racket unquote))

@title[#:tag "qq"]{Quasiquoting: @racket[quasiquote] and @racketvalfont{`}}

@refalso["quasiquote"]{@racket[quasiquote]}

The @racket[quasiquote] form is similar to @racket[quote]:

@specform[(#,qq datum)]

However, for each @racket[(#,uq _expr)]
that appears within the @racket[_datum], the @racket[_expr] is
evaluated to produce a value that takes the place of the
@racket[unquote] sub-form.

@examples[
(eval:alts (#,qq (1 2 (#,uq (+ 1 2)) (#,uq (- 5 1))))
           `(1 2 ,(+ 1 2), (- 5 1)))
]

The @racket[unquote-splicing] form is similar to @racket[unquote], but
its @racket[_expr] must produce a list, and the
@racket[unquote-splicing] form must appear in a context that produces
either a list or a vector. As the name suggests, the resulting list
is spliced into the context of its use.

@examples[
(eval:alts (#,qq (1 2 (#,(racket unquote-splicing) (list (+ 1 2) (- 5 1))) 5))
           `(1 2 ,@(list (+ 1 2) (- 5 1)) 5))
]

If a @racket[quasiquote] form appears within an enclosing
@racket[quasiquote] form, then the inner @racket[quasiquote]
effectively cancels one layer of @racket[unquote] and
@racket[unquote-splicing] forms, so that a second @racket[unquote]
or @racket[unquote-splicing] is needed.

@examples[
(eval:alts (#,qq (1 2 (#,qq (#,uq (+ 1 2) 
                            (#,uq (#,uq (- 5 1)))))))
           `(1 2 (,(string->uninterned-symbol "quasiquote") 
                  (,(string->uninterned-symbol "unquote") (+ 1 2)) 
                  (,(string->uninterned-symbol "unquote") 4))))
]

The evaluation above will not actually print as shown. Instead, the
shorthand form of @racket[quasiquote] and @racket[unquote] will be
used: @litchar{`} (i.e., a backquote) and @litchar{,} (i.e., a comma).
The same shorthands can be used in expressions:

@examples[
`(1 2 `(,(+ 1 2) ,,(- 5 1)))
]

The shorthand for of @racket[unquote-splicing] is @litchar[",@"]:

@examples[
`(1 2 ,@(list (+ 1 2) (- 5 1)))
]
