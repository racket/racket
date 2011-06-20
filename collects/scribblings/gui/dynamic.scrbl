#lang scribble/doc
@(require "common.rkt" (for-label racket/gui/dynamic))

@title{Dynamic Loading}

@defmodule[racket/gui/dynamic]{The @racketmodname[racket/gui/dynamic]
library provides functions for dynamically accessing the
@racketmodname[racket/gui/base] library, instead of directly requiring
@racketmodname[racket/gui] or @racketmodname[racket/gui/base].}

@defproc[(gui-available?) boolean?]{

Returns @racket[#t] if dynamic access to the GUI bindings is
available. The bindings are available if
@racketmodname[racket/gui/base] has been loaded, instantiated, and
attached to the namespace in which @racket[racket/gui/dynamic] was
instantiated.}


@defproc[(gui-dynamic-require [sym symbol?]) any]{

Like @racket[dynamic-require], but specifically to access exports of
@racketmodname[racket/gui/base].}
