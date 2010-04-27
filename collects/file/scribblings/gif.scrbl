#lang scribble/doc
@(require scribble/manual
          scribble/extract
          (for-label file/gif))

@title[#:tag "gif"]{GIF File Writing}

@defmodule[file/gif]

The @schememodname[file/gif] library provides functions for
writing GIF files to a stream, including GIF files with multiple
images and controls (such as animated GIFs).

A GIF stream is created by @scheme[gif-start], and then individual
images are written with @scheme[gif-add-image]. Optionally,
@scheme[gif-add-control] inserts instructions for rendering the
images. The @scheme[gif-end] function ends the GIF stream.

A GIF stream can be in any one of the following states:

@itemize[

 @item{@scheme['init] : no images or controls have been added to the
       stream}

 @item{@scheme['image-or-control] : another image or control can be
       written}

 @item{@scheme['image] : another image can be written (but not a
       control, since a control was written)}

 @item{@scheme['done] : nothing more can be added}

]

@(include-extracted "../gif.rkt")
