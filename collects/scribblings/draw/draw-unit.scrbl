#lang scribble/doc
@(require "common.rkt"
          (for-label racket/draw/draw-unit racket/draw/draw-sig))

@title{Signature and Unit}

The @racketmodname[racket/draw/draw-sig] and
@racketmodname[racket/draw/draw-unit] libraries define the
@racket[draw^] signature and @racket[draw@] implementation.

@section{Draw Unit}

@defmodule[racket/draw/draw-unit]

@defthing[draw@ unit?]{
Re-exports all of the exports of @racketmodname[racket/draw].}


@section{Draw Signature}

@defmodule[racket/draw/draw-sig]

@defsignature[draw^ ()]

Includes all of the identifiers exported by @racketmodname[racket/draw].
