#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt"
          (for-label racket/undefined
                     racket/shared))

@title[#:tag "void+undefined"]{Void and Undefined}

Some procedures or expression forms have no need for a result
value. For example, the @racket[display] procedure is called only for
the side-effect of writing output. In such cases the result value is
normally a special constant that prints as @|void-const|.  When the
result of an expression is simply @|void-const|, the @tech{REPL} does not
print anything.

The @racket[void] procedure takes any number of arguments and returns
@|void-const|. (That is, the identifier @racketidfont{void} is bound
to a procedure that returns @|void-const|, instead of being bound
directly to @|void-const|.)

@examples[
(void)
(void 1 2 3)
(list (void))
]

The @racket[undefined] constant, which prints as @|undefined-const|, is
sometimes used as the result of a reference whose value is not yet
available. In previous versions of Racket (before version 6.1),
referencing a local binding too early produced @|undefined-const|;
too-early references now raise an exception, instead.

@margin-note{The @racket[undefined] result can still be produced
in some cases by the @racket[shared] form.}

@def+int[
(define (fails)
  (define x x)
  x)
(fails)
]
