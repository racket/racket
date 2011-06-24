#lang scribble/doc
@(require scribble/manual
          scribble/extract
          (for-label file/gif))

@title[#:tag "gif"]{GIF File Writing}

@defmodule[file/gif]

The @racketmodname[file/gif] library provides functions for
writing GIF files to a stream, including GIF files with multiple
images and controls (such as animated GIFs).

A GIF stream is created by @racket[gif-start], and then individual
images are written with @racket[gif-add-image]. Optionally,
@racket[gif-add-control] inserts instructions for rendering the
images. The @racket[gif-end] function ends the GIF stream.

A GIF stream can be in any one of the following states:

@itemize[

 @item{@racket['init] : no images or controls have been added to the
       stream}

 @item{@racket['image-or-control] : another image or control can be
       written}

 @item{@racket['image] : another image can be written (but not a
       control, since a control was written)}

 @item{@racket['done] : nothing more can be added}

]

@(include-extracted "../gif.rkt")
