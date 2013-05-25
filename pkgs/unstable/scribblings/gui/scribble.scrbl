#lang scribble/manual
@(require "../utils.rkt"
          (for-label slideshow
                     unstable/contract
                     unstable/gui/scribble))

@title{Scribble Utilities}
@unstable[@author+email["Jon Rafkind" "rafkind@racket-lang.org"]]

@defmodule[unstable/gui/scribble]

@defproc[(codeblock->pict [block block?]) pict?]{

Converts a scribble block element into a pict.
}
