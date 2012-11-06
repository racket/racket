#lang scribble/manual
@(require scribble/eval "utils.rkt"
          (for-label unstable/open-place racket/place racket/contract racket/base))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/class racket/place unstable/open-place))

@title[#:tag "open-place"]{Open @racket[place] expressions}
@unstable-header[]

@defmodule[unstable/open-place]

@addition[@author+email["Sam Tobin-Hochstadt" "samth@racket-lang.org"]]

@defform[(open-place id body ...+)]{ 

Like @racket[(place id body ...)], but @racket[body ...] may have free lexical
variables, which are automatically sent to the newly-created @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{place}.
Note that these variables must have values accepted by
@racket[place-message-allowed?], otherwise an @racket[exn:fail:contract] exception is raised.
}


@close-eval[the-eval]
