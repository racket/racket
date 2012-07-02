#lang scribble/doc
@(require "common.rkt" (for-label racket/class))

@title[#:tag "init"]{Initialization}

@defmodule[sgl/init]

Requiring the @racketmodname[sgl/init] library initializes
platform-specific OpenGL state to help avoid crashes when OpenGL
commands are incorrectly used without a current context.  This library
is @racket[require]d by @racketmodname[sgl] and
@racketmodname[sgl/gl], so it normally does not need to be
@racket[require]d explicitly.

On Mac OS X, @racketmodname[sgl/init] checks whether any GL context is
current, and if not, it creates a dummy context and sets it as the
current context.
