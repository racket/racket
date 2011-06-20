#lang scribble/doc
@(require "common.rkt"
          (for-label mrlib/matrix-snip
                     (only-in mrlib/cache-image-snip
                              cache-image-snip%)))

@title{Matrix Snip}

@defmodule[mrlib/matrix-snip]

The @racket[mrlib/matrix-snip] library implements a matrix value that displays
in 2-D graphical form.

@defclass[visible-matrix% cache-image-snip% ()]{

A 2-D graphical matrix.}
