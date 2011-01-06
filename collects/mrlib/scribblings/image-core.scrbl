#lang scribble/doc
@(require "common.ss")

@title{Image Core}

@defmodule[mrlib/image-core]

This library is the core part of the @racketmodname[2htdp/image] library that DrRacket
links into the namespace of all languages that it runs. This ensures that minimal
support for these images are the same in all languages, specifically including
support for printing the images and constructing the core data structures making
up an image.

@defproc[(render-image [image image?]
                       [dc (is-a?/c dc<%>)]
                       [dx number?]
                       [dy number?])
         void?]{
  Draws @racket[image] in @racket[dc] at the position (@racket[dx],@racket[dy]).
}
               
@defproc[(image? [v any/c]) boolean?]{
 Recognizes the images that library handles.
}

@defproc[(un/cache-image [image image?] [b any/c]) image?]{
  Returns an image that either caches its drawing in the 
  snip @method[snip% draw] method or doesn't, depending on @racket[b].
}