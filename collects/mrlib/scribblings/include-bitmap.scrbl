#lang scribble/doc
@(require "common.ss"
          (for-label mrlib/include-bitmap
                     scheme/runtime-path
                     scheme/include))

@title{Include Bitmap}

@defmodule[mrlib/include-bitmap]{The @scheme[include-bitmap] form
takes a filename containing a bitmap and ``inlines'' the bitmap into
the program.}

Historically, the advantage of inlining the bitmap is that a
stand-alone executable can be created that contains the bitmap and
does not refer to the original image file. The
@scheme[define-runtime-path] form, however, now provides a better
alternative.

@defform*[[(include-bitmap path-spec)
           (include-bitmap path-spec type-expr)]]{


The @scheme[path-spec] is the same as for @scheme[include] form. The
@scheme[type-expr] should produce @scheme['unknown],
@scheme['unknown/mask], etc., as for @scheme[bitmap%], and the default
is @scheme['unknown/mask].}


@defform*[[(include-bitmap/relative-to source path-spec)
           (include-bitmap/relative-to source path-spec [type-expr])]]{

Analogous to @scheme[include-at/relative-to], though only a source is
needed (no context).}
