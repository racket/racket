#lang scribble/doc
@(require "common.rkt" (for-label racket/class))

@title[#:tag "bitmaps"]{Bitmaps}

@defmodule[sgl/bitmap]


@defproc[(bitmap->gl-list [bitmap (is-a?/c bitmap%)]
                          [#:with-gl with-gl-proc ((-> any) . -> . any)
                                     (lambda (f) (f))]
                          [#:mask mask (or/c (is-a?/c bitmap%) false/c)
                                       (send bitmap get-loaded-mask)])
         exact-integer?]{

Converts the given bitmap into an OpenGL list that can be rendered
with @racket[gl-call-list] or @racket[glCallList]. The rendered object
is a square on the @math{z=0} plane with corners at @math{(0,0)} and
@math{(1,1)}.

The @racket[with-gl-proc] must accept a thunk and call it while the
relevant OpenGL context is selected. Otherwise, the relevant OpenGL
context must be selected already.

If @racket[mask] is not @racket[#f], it is used as the mask bitmap for
extracting alpha values.}

