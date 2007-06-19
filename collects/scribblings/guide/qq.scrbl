#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title{Quasiquoting}

[Explain why...]

@;------------------------------------------------------------------------
@section{Escapes: @scheme[quasiquote], @scheme[unquote], and @scheme[unquote-splicing]}

The @scheme[quasiquote] form is similar to @scheme[quote]:

@specform[(#,(schemekeywordfont "quasiquote") datum)]

However, for each @scheme[(#,(schemekeywordfont "unquote") _expr)]
that appears within the @scheme[_datum], the @scheme[_expr] is
evaluated to produce a value that takes the place of the
@scheme[unsyntax] sub-form.

@examples[
(eval:alts (#,(schemekeywordfont "quasiquote") (1 2 (#,(schemekeywordfont "unquote") (+ 1 2)) (#,(schemekeywordfont "unquote") (- 5 1))))
           `(1 2 ,(+ 1 2), (- 5 1)))
]

The @scheme[unquote-splicing] form is similar to @scheme[unquote], but
its @scheme[_expr] must produce a list, and the
@scheme[unquote-splicing] form must appear in a context that produces
either a list of vector. As the name suggests, the resulting list
spliced into the context of its use.

@examples[
(eval:alts (#,(schemekeywordfont "quasiquote") (1 2 (#,(schemekeywordfont "unquote-splicing") (list (+ 1 2) (- 5 1))) 5))
           `(1 2 ,@(list (+ 1 2) (- 5 1)) 5))
]

If a @scheme[quasiquote] form appears within an enclosing
@scheme[quasiquote] form, then the inner @scheme[quasiquote]
effectively cancels one layer of @scheme[unquote] and
@scheme[unquote-splicing] forms, so that a second @scheme[unquote]
or @scheme[unquote-splicing] is needed.

@;------------------------------------------------------------------------
@section{Abbreviating with @schememetafont{`}, @schememetafont{,}, and @schememetafont[",@"]}

