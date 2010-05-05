#lang scribble/doc
@(require "common.ss"
          (for-label racket/gui/dynamic))

@title{Dynamic Loading}

@defmodule[racket/gui/dynamic]{The @racketmodname[racket/gui/dynamic]
library provides functions for dynamically accessing the Racket
GUI toolbox, instead of directly requiring @racket[racket/gui] or
@racket[racket/gui/base].}

@defproc[(gui-available?) boolean?]{

Returns @racket[#t] if dynamic access to the GUI bindings are
available---that is, that the program is being run as a
GRacket-based application, as opposed to a pure
Racket-based application, and that GUI modules are attached
to the namespace in which @racket[racket/gui/dynamic] was
instantiated.

This predicate can be used in code that optionally uses GUI elements
when they are available.}


@defproc[(gui-dynamic-require [sym symbol?]) any]{

Like @racket[dynamic-require], but specifically to access exports of
@racket[racket/gui/base].}
