#lang scribble/manual
@(require scribble/eval "utils.rkt"
          (for-label unstable/exn racket/contract racket/base))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/exn))

@title[#:tag "exn"]{Exceptions}

@defmodule[unstable/exn]

@unstable-header[]

@defproc[(network-error [s symbol?]
                        [fmt string?]
                        [v any/c] ...)
         void]{
 Like @racket[error], but throws a @racket[exn:fail:network].
}

@defproc[(exn->string [exn (or/c exn? any/c)])
         string?]{
 Formats @racket[exn] with @racket[(error-display-handler)] as a string.
}

@addition[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@defform[(try expr ...+)]{

Executes the first expression @racket[expr] in the sequence, producing its
result value(s) if it returns any.  If it raises an exception instead,
@racket[try] continues with the next @racket[expr].  Exceptions raised by
intermediate expressions are reported to the @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{current logger} at the @racket['debug]
level before continuing.  Exceptions raised by the final expression are not
caught by @racket[try].

@defexamples[
#:eval the-eval
(try (+ 1 2) (+ 3 4))
(try (+ 'one 'two) (+ 3 4))
(try (+ 'one 'two) (+ 'three 'four))
]

}

@(close-eval the-eval)
