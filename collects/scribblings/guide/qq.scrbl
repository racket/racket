#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@(define qq (scheme quasiquote))
@(define uq (scheme unquote))

@title{Quasiquoting: @scheme[quasiquote] and @schemevalfont{`}}

The @scheme[quasiquote] form is similar to @scheme[quote]:

@specform[(#,qq datum)]

However, for each @scheme[(#,uq _expr)]
that appears within the @scheme[_datum], the @scheme[_expr] is
evaluated to produce a value that takes the place of the
@scheme[unquote] sub-form.

@examples[
(eval:alts (#,qq (1 2 (#,uq (+ 1 2)) (#,uq (- 5 1))))
           `(1 2 ,(+ 1 2), (- 5 1)))
]

The @scheme[unquote-splicing] form is similar to @scheme[unquote], but
its @scheme[_expr] must produce a list, and the
@scheme[unquote-splicing] form must appear in a context that produces
either a list or a vector. As the name suggests, the resulting list
is spliced into the context of its use.

@examples[
(eval:alts (#,qq (1 2 (#,(scheme unquote-splicing) (list (+ 1 2) (- 5 1))) 5))
           `(1 2 ,@(list (+ 1 2) (- 5 1)) 5))
]

If a @scheme[quasiquote] form appears within an enclosing
@scheme[quasiquote] form, then the inner @scheme[quasiquote]
effectively cancels one layer of @scheme[unquote] and
@scheme[unquote-splicing] forms, so that a second @scheme[unquote]
or @scheme[unquote-splicing] is needed.

@examples[
(eval:alts (#,qq (1 2 (#,qq (#,uq (+ 1 2) 
                            (#,uq (#,uq (- 5 1)))))))
           `(1 2 (,(string->uninterned-symbol "quasiquote") 
                  (,(string->uninterned-symbol "unquote") (+ 1 2)) 
                  (,(string->uninterned-symbol "unquote") 4))))
]

The evaluation above will not actually print as shown. Instead, the
shorthand form of @scheme[quasiquote] and @scheme[unquote] will be
used: @litchar{`} (i.e., a backquote) and @litchar{,} (i.e., a comma).
The same shorthands can be used in expressions:

@examples[
`(1 2 `(,(+ 1 2) ,,(- 5 1)))
]

The shorthand for of @scheme[unquote-splicing] is @litchar[",@"]:

@examples[
`(1 2 ,@(list (+ 1 2) (- 5 1)))
]
