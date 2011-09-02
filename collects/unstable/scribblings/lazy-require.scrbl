#lang scribble/manual
@(require scribble/eval
          "utils.rkt"
          (for-label racket/base
                     racket/runtime-path
                     unstable/lazy-require))

@title[#:tag "lazy-require"]{Lazy Require}

@defmodule[unstable/lazy-require]

@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defform[(lazy-require [mod-expr (imported-fun-id ...)] ...)
         #:contracts ([mod-expr module-path?])]{

Defines each @racket[imported-fun-id] as a function that, when called,
dynamically requires the export named @racket['imported-fun-id] from
the module specified by @racket[mod-expr] and calls it with the same
arguments.
                                                
As with @racket[define-runtime-module-path-index], @racket[mod-expr]
is evaluated both in phase 0 and phase 1.
}
