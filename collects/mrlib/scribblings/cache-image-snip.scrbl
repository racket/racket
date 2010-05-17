#lang scribble/doc
@(require "common.ss"
          (for-label mrlib/cache-image-snip))

@title{Cache-image Snip}

@defmodule[mrlib/cache-image-snip]{

The @schememodname[mrlib/cache-image-snip] library provides the core
data structure for DrRacket's @filepath{image.ss} teachpack. Images in
the @filepath{image.ss} teachpack are instances of the
@scheme[cache-image-snip%] class.}

The library also defines a new type, @scheme[argb], that represents a
bitmap, but with alpha values. It has a maker, two selectors, and a
predicate.


@defclass[cache-image-snip% image-snip% ()]{

The @scheme[cache-image-snip%] class is a subclass of
@scheme[image-snip%] simply so that its instances can be compared with
@scheme[image-snip%] using @scheme[equal?]. All @scheme[image-snip%]
functionality is overridden or ignored.

@defmethod[#:mode overrride
           (equal-to? [snip (is-a?/c image-snip%)]
                      [equal? (any/c any/c . -> . boolean?)])
           boolean?]{

Calls the @method[cache-image-snip% other-equal-to?] method of
@scheme[snip] if it is also a @scheme[cache-image-snip%] instance,
otherwise calls the @method[cache-image-snip% other-equal-to?] of
@this-obj[].}


@defmethod[(get-argb)
           argb?]{

    Returns a pixel array for this image, forcing it to be
    computed.
}

@defmethod[(get-argb-proc)
           (argb? exact-integer? exact-integer? . -> . void?)]{

    Returns a procedure that fills in an argb with the contents of this image
    at the given offset


}

@defmethod[(get-argb/no-compute)
           (or/c false/c argb?)]{

    Returns a pixel array for this image or @scheme[#f] if it has not
    been computed yet.


}

@defmethod[#:mode override
           (get-bitmap) (or/c false/c (is-a?/c bitmap%))]{

    Builds (if not yet built) a bitmap corresponding to
    this snip and returns it.

    If the width or the height of the snip is @scheme[0], 
    this method return @scheme[#f].
}

@defmethod[(get-dc-proc)
           (or/c false/c ((is-a?/c dc<%>) real? real? -> void?))]{

    Either returns false, or a procedure that draws the
    contents of this snip into a dc.

}

@defmethod[(get-pinhole)
           (values real? real?)]{

    Returns the pinhole coordinates for this image, counting
    from the top-left of the image.


}

@defmethod[(get-size)
           (values exact-nonnegative-integer? exact-nonnegative-integer?)]{

    Returns the width and height for the image.

}

@defmethod[#:mode override
           (other-equal-to? [snip (is-a?/c image-snip%)]
                            [equal? (any/c any/c . -> . boolean?)])
           boolean?]{

Refines the comparison of @xmethod[image-snip% other-equal-to?] to
exactly match alpha channels.}}

@; ----------------------------------------

@defthing[snip-class (is-a?/c snip-class%)]{

This snipclass is used for saved cache image snips.}


@defproc[(make-argb [vectorof (integer-in 0 255)]
                    [width exact-nonnegative-integer?]
                    [height exact-nonnegative-integer?])
         argb?]{

 Constructs a new argb value. The vector has four entries
  for each pixel, an alpha, red, green, and blue value. The
   int specifies the width of the image; the height is the
   size of the vector, divided by 4, divided by the width.}

@defproc[(argb-vector [argb argb?]) (vectorof (integer-in 0 255))]{

  Extracts the vector from @scheme[argb].}

@defproc[(argb-width [argb argb?]) exact-nonnegative-integer?]{

  Extracts the width from @scheme[argb].}

@defproc[(argb-height [argb argb?]) exact-nonnegative-integer?]{

  Extracts the height from @scheme[argb].}


@defproc[(argb? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is an argb, @scheme[#f] otherwise.}


@defproc[(overlay-bitmap [dest argb?]
                         [dx exact-integer?]
                         [dy exact-integer?]
                         [img (is-a?/c bitmap%)]
                         [mask (is-a?/c bitmap%)])
         void?]{

Changes @scheme[argb], overlaying @scheme[img] with masking based on
@scheme[mask] at @math{(@scheme[dx], @scheme[dy])} from the top-left.}


@defproc[(build-bitmap [draw ((is-a?/c dc<%>) . -> . any)]
                       [width (integer-in 1 10000)]
                       [height (integer-in 1 10000)])
         (is-a?/c bitmap%)]{

Builds a bitmap of size @scheme[width] by @scheme[height], using the
procedure @scheme[draw] to render the bitmap content into the given
@scheme[dc<%>].}


@defproc[(flatten-bitmap [bitmap (is-a?/c bitmap%)]) (is-a?/c bitmap%)]{

    Builds a new bitmap that flattens the original @scheme[bitmap]
    with its mask (as determined by @xmethod[bitmap%
    get-loaded-mask]), producing a bitmap that has no mask, and looks
    the way that bitmap would draw (when drawn with the mask) onto a
    white background.}

           
@defproc[(argb->cache-image-snip [argb argb?][dx real?][dy real?])
         (is-a?/c cache-image-snip%)]{

 Builds a new @scheme[cache-image-snip%] based on the contents of
 @scheme[argb], using @scheme[dx] and @scheme[dy] as the pinhole.}


@defproc[(argb->bitmap [argb argb?]) (or/c false/c (is-a?/c bitmap%))]{

   Builds a bitmap that draws the same way as @scheme[argb]; the alpha
   pixels are put into the bitmap's @method[bitmap% get-loaded-mask]
   bitmap.

   If the width or height of @scheme[argb] is @scheme[0],
   this returns @scheme[#f].
}

