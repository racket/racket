#lang scribble/doc
@(require "mz.rkt" scribble/eval (for-label racket/block))

@(define ev (make-base-eval))
@(ev '(require racket/block))

@title[#:tag "block"]{Blocks: @racket[block]}

@note-lib-only[racket/block]

@defform[(block defn-or-expr ...)]{

Supports a mixture of expressions and mutually recursive definitions,
as in a @racket[module] body. Unlike an @tech{internal-definition
context}, the last @racket[defn-or-expr] need not be an expression.

The result of the @racket[block] form is the result
of the last @racket[defn-or-expr] if it is an expression,
@|void-const| otherwise. If no @racket[defn-or-expr] is provided
(after flattening @racket[begin] forms), the result is @|void-const|.

The final @racket[defn-or-expr] is executed in tail position, if it is
an expression.  


@examples[#:eval ev
(define (f x)
  (block
    (define y (add1 x))
    (displayln y)
    (define z (* 2 y))
    (+ 3 z)))
(f 12)
]}

@close-eval[ev]
