#lang scribble/doc
@(require "common.rkt" 
          (for-label mrlib/cache-image-snip)
          scribble/decode)

@(define dont-use
   (splice
    @list{This function is in a library that will be 
          removed in a future version. Do not use it.
          
          The mentions of ``argb'' in this library are not
          @racket[bytes?] objects (i.e, not the
          same as for example, the result of 
          @method[bitmap-dc% get-argb-pixels])}))

@title{Cache-image Snip}

@defmodule[mrlib/cache-image-snip]{

@deprecated[@racketmodname[racket/gui]]{This library will
  no longer be public in a future release; much of it will
  be available privately to continue to support the implementation
  of @racket[htdp/image], but the other exported functions
  here are not useful and have names that confusingly match
  unrelated other libraries.}
                                   
The @racketmodname[mrlib/cache-image-snip] library provides the core
data structure for DrRacket's @filepath{image.rkt} teachpack. Images in
the @filepath{image.rkt} teachpack are instances of the
@racket[cache-image-snip%] class.}

The library also defines a new type, @racket[argb], that represents a
bitmap, but with alpha values. It has a maker, two selectors, and a
predicate.


@defclass[cache-image-snip% image-snip% ()]{

The @racket[cache-image-snip%] class is a subclass of
@racket[image-snip%] simply so that its instances can be compared with
@racket[image-snip%] using @racket[equal?]. All @racket[image-snip%]
functionality is overridden or ignored.

@defmethod[#:mode overrride
           (equal-to? [snip (is-a?/c image-snip%)]
                      [equal? (any/c any/c . -> . boolean?)])
           boolean?]{

Calls the @method[cache-image-snip% other-equal-to?] method of
@racket[snip] if it is also a @racket[cache-image-snip%] instance,
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

    Returns a pixel array for this image or @racket[#f] if it has not
    been computed yet.


}

@defmethod[#:mode override
           (get-bitmap) (or/c false/c (is-a?/c bitmap%))]{

    Builds (if not yet built) a bitmap corresponding to
    this snip and returns it.

    If the width or the height of the snip is @racket[0], 
    this method return @racket[#f].
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


@defproc[(make-argb [vectorof byte?]
                    [width exact-nonnegative-integer?]
                    [height exact-nonnegative-integer?])
         argb?]{

 Constructs a new argb value with the given width and height,
 using the data in the vector. The vector has four entries
  for each pixel, an alpha, red, green, and blue value.
  The pixels are specified in row-major order, so that the
  pixel at location (x,y) comes from vector entry (4*(x+(width*y))).
}

@defproc[(argb-vector [argb argb?]) (vectorof byte?)]{

  Extracts the vector from @racket[argb]. The resulting vector
 has entries in row-major order, so that the data for the pixel
 at (x,y) winds up in four vector entries beginning at
 (4*(x+(width*y))).
 
   @dont-use
}

@defproc[(argb-width [argb argb?]) exact-nonnegative-integer?]{

   Extracts the width from @racket[argb].
                           
   @dont-use
}

@defproc[(argb-height [argb argb?]) exact-nonnegative-integer?]{

  Extracts the height from @racket[argb].
                           
  @dont-use}


@defproc[(argb? [v any/c]) boolean?]{

  Returns @racket[#t] if @racket[v] is an argb, @racket[#f] otherwise.
          
  @dont-use
}


@defproc[(overlay-bitmap [dest argb?]
                         [dx exact-integer?]
                         [dy exact-integer?]
                         [img (is-a?/c bitmap%)]
                         [mask (is-a?/c bitmap%)])
         void?]{

Changes @racket[argb], overlaying @racket[img] with masking based on
@racket[mask] at @math{(@racket[dx], @racket[dy])} from the top-left.

  @dont-use
}


@defproc[(build-bitmap [draw ((is-a?/c dc<%>) . -> . any)]
                       [width (integer-in 1 10000)]
                       [height (integer-in 1 10000)])
         (is-a?/c bitmap%)]{

Builds a bitmap of size @racket[width] by @racket[height], using the
procedure @racket[draw] to render the bitmap content into the given
@racket[dc<%>].

  @dont-use
}


@defproc[(flatten-bitmap [bitmap (is-a?/c bitmap%)]) (is-a?/c bitmap%)]{

    Builds a new bitmap that flattens the original @racket[bitmap]
    with its mask (as determined by @xmethod[bitmap%
    get-loaded-mask]), producing a bitmap that has no mask, and looks
    the way that bitmap would draw (when drawn with the mask) onto a
    white background.
    
    @dont-use
}


@defproc[(argb->cache-image-snip [argb argb?][dx real?][dy real?])
         (is-a?/c cache-image-snip%)]{

 Builds a new @racket[cache-image-snip%] based on the contents of
 @racket[argb], using @racket[dx] and @racket[dy] as the pinhole.
 
  @dont-use
}


@defproc[(argb->bitmap [argb argb?]) (or/c false/c (is-a?/c bitmap%))]{

   Builds a bitmap that draws the same way as @racket[argb]; the alpha
   pixels are put into the bitmap's @method[bitmap% get-loaded-mask]
   bitmap.

   If the width or height of @racket[argb] is @racket[0],
   this returns @racket[#f].
   
   @dont-use
}

