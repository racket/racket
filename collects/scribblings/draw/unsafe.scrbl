#lang scribble/doc
@(require "common.rkt"
          (for-label (except-in ffi/unsafe ->)
                     racket/draw/unsafe/brush
                     racket/draw/unsafe/cairo-lib))

@title[#:tag "unsafe"]{Unsafe Libraries}

The @racketmodname[racket/draw] library is currently implemented using Cairo
and Pango. The @xmethod[bitmap% get-handle] method exposes the
underlying Cairo surface for a @racket[bitmap%] object, while
@racket[make-handle-brush] supports the creation of a brush from an
existing Cairo surface. The representation of handles for these
methods, however, is subject to change if the @racketmodname[racket/draw]
library is implemented differently in the future.

@section{Handle Brushes}

@defmodule[racket/draw/unsafe/brush]

@defproc[(make-handle-brush [handle cpointer?] 
                            [width exact-nonnegative-integer?]
                            [height exact-nonnegative-integer?]
                            [transformation  (or/c #f (vector/c (vector/c real? real? real? 
                                                                          real? real? real?)
                                                                real? real? real? real? real?))]
                            [#:copy? copy? any/c #t])
         (is-a?/c brush%)]{

Creates a brush given a @racket[handle] that (currently) is a
@tt{cairo_surface_t}. If @racket[copy?] is true, then the surface is
copied, so that it can be freed or modified after the brush is
created; if @racket[copy?] is @racket[#f], the surface must remain available
and unchanged as long as the brush can be used.

The @racket[width] and @racket[height] arguments specify the surface
bounds for use when the surface must be copied---even when
@racket[copy?] is @racket[#f]. The surface may need to be converted to a
stipple bitmap, for example, when drawing to a monochrome target.

The given surface is treated much like a stipple bitmap: it is
implicitly repeated, and the given @racket[transformation] (if any)
determines the surface's alignment relative to the target drawing
context.

When the brush is used with a @racket[record-dc%] object, and if that
object's @method[record-dc% get-recorded-datum] method is called, then the
surface is effectively converted to a stipple bitmap for the result datum.}


@section{Cairo Library}

@defmodule[racket/draw/unsafe/cairo-lib]

@defthing[cairo-lib (or/c ffi-lib? #f)]{

A reference to the Cairo library for use with functions such as
@racket[get-ffi-obj], or @racket[#f] if Cairo is unavailable.}


