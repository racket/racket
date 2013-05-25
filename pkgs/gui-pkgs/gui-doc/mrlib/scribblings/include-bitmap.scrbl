#lang scribble/doc
@(require "common.rkt"
          (for-label mrlib/include-bitmap scheme/runtime-path scheme/include))

@title{Include Bitmap}

@defmodule[mrlib/include-bitmap]{The @racket[include-bitmap] form
takes a filename containing a bitmap and ``inlines'' the bitmap into
the program.}

Historically, the advantage of inlining the bitmap is that a
stand-alone executable can be created that contains the bitmap and
does not refer to the original image file. The
@racket[define-runtime-path] form, however, now provides a better
alternative.

@defform*[[(include-bitmap path-spec)
           (include-bitmap path-spec type-expr)]]{


The @racket[path-spec] is the same as for @racket[include] form. The
@racket[type-expr] should produce @racket['unknown],
@racket['unknown/mask], etc., as for @racket[bitmap%], and the default
is @racket['unknown/mask].}


@defform*[[(include-bitmap/relative-to source path-spec)
           (include-bitmap/relative-to source path-spec [type-expr])]]{

Analogous to @racket[include-at/relative-to], though only a source is
needed (no context).}
