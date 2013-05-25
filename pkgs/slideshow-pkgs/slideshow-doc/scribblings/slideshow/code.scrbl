#lang scribble/doc
@(require "ss.rkt" 
          scribble/eval
          (for-label slideshow/code
                     racket/gui/base))

@title{Typesetting Racket Code in Slideshow}

@defmodule[slideshow/code]{
The @racketmodname[slideshow/code] library
provides all of the exports of
@racketmodname[pict/code] and also initializes
@racket[get-current-code-font-size] to @racket[current-font-size].}

