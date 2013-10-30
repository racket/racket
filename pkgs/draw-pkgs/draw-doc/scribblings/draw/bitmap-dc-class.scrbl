#lang scribble/doc
@(require "common.rkt")

@defclass/title[bitmap-dc% object% (dc<%>)]{

A @racket[bitmap-dc%] object allows drawing directly into a bitmap.  A
 @racket[bitmap%] object must be supplied at initialization or
 installed into a bitmap DC using @method[bitmap-dc% set-bitmap]
 before any other method of the DC is called, except @method[dc<%>
 get-text-extent], @method[dc<%> get-char-height], or @method[dc<%>
 get-char-width]. If any other @racket[bitmap-dc%] method is called
 before a bitmap is selected, the method call is ignored.

Drawing to a @racket[bitmap-dc%] with a color bitmap is guaranteed to
 produce the same result as drawing into a @racket[canvas%] instance
 (with appropriate clipping and offsets). Thus, a @racket[bitmap-dc%]
 can be used for offscreen staging of canvas content.


@defconstructor[([bitmap (or/c (is-a?/c bitmap%) #f)])]{

Creates a new bitmap DC. If @racket[bitmap] is not @racket[#f], it is
 installed into the DC so that drawing commands on the DC draw to
 @racket[bitmap]. Otherwise, no bitmap is installed into the DC and
 @method[bitmap-dc% set-bitmap] must be called before any other method
 of the DC is called.

}

@defmethod[(draw-bitmap-section-smooth [source (is-a?/c bitmap%)]
                                       [dest-x real?]
                                       [dest-y real?]
                                       [dest-width (and/c real? (not/c negative?))]
                                       [dest-height (and/c real? (not/c negative?))]
                                       [src-x real?]
                                       [src-y real?]
                                       [src-width (and/c real? (not/c negative?))]
                                       [src-height (and/c real? (not/c negative?))]
                                       [style (or/c 'solid 'opaque 'xor) 'solid]
                                       [color (is-a?/c color%) (send the-color-database find-color "black")]
                                       [mask (or/c (is-a?/c bitmap%) #f) #f])
           boolean?]{

The same as @method[dc<%> draw-bitmap-section], except that
 @racket[dest-width] and @racket[dest-height] cause the DC's
 transformation to be adjusted while drawing the bitmap so that the
 bitmap is scaled; and, if the DC's smoothing mode is
 @racket['unsmoothed], it is changed to @racket['aligned] while
 drawing.}

@defmethod[(get-argb-pixels [x real?]
                            [y real?]
                            [width exact-nonnegative-integer?]
                            [height exact-nonnegative-integer?]
                            [pixels (and/c bytes? (not/c immutable?))]
                            [just-alpha? any/c #f]
                            [pre-multiplied? any/c #f])
           void?]{

Gets a rectangle of pixels in the bitmap, subject to the same rules
 and performance characteristics of @method[bitmap-dc% get-pixel],
 except that the block get is likely to be faster than the sequence of
 individual gets. Also, the @racket[bitmap%] class also provides the
 same method directly, so it is not necessary to select a bitmap into
 a DC to extracts its pixel values.

The pixel RGB values and alphas are copied into @racket[pixels] 
 (or just alpha values if @racket[just-alpha?] is true). The first byte
 represents an alpha value of the pixel at (@racket[x], @racket[y]),
 the second byte represents a red value of the pixel at (@racket[x],
 @racket[y]), the third byte is the green value, etc. In this way, the
 first @math{@racket[width] * @racket[height] * 4} bytes of
 @racket[pixels] are set to reflect the current pixel values in the
 DC. The pixels are in row-major order, left to right then top to
 bottom.

If the bitmap has an alpha channel, then the alpha value for each pixel
 is always set in @racket[pixels]. 
 If @racket[just-alpha?] is false and the bitmap does not have an alpha
 channel, then the alpha value for each pixel is set to 255. If
 @racket[just-alpha?] is true, then @italic{only} the alpha value is set
 for each pixel; if the bitmap has no alpha channel, then the alpha
 value is based on each pixel's inverted RGB average. Thus, when a
 bitmap has a separate mask bitmap, the same @racket[pixels] byte
 string is in general filled from two bitmaps: one (the main image)
 for the pixel values and one (the mask) for the alpha values.

If @racket[pre-multiplied?] is true, @racket[just-alpha?] is false,
 and the bitmap has an alpha channel, then RGB values in the result
 are scaled by the corresponding alpha value (i.e., multiplied by the
 alpha value and then divided by 255).

If the bitmap has a backing scale (see @xmethod[bitmap%
 get-backing-scale]) other than @racket[1.0], the the result of
 @method[bitmap-dc% get-argb-pixels] is as if the bitmap is drawn to a
 bitmap with a backing scale of @racket[1.0] and the pixels of the
 target bitmap are returned.}


@defmethod[(get-bitmap)
           (or/c (is-a?/c bitmap%) #f)]{

Gets the bitmap currently installed in the DC, or @racket[#f] if no
 bitmap is installed. See @method[bitmap-dc% set-bitmap] for more
 information.

}

@defmethod[(get-pixel [x real?]
                      [y real?]
                      [color (is-a?/c color%)])
           boolean?]{

Fills @racket[color] with the color of the current pixel at position
(@racket[x], @racket[y]) in the drawing context. If the color is
successfully obtained, the return value is @racket[#t], otherwise the
result is @racket[#f].

}

@defmethod[(set-argb-pixels [x real?]
                            [y real?]
                            [width exact-nonnegative-integer?]
                            [height exact-nonnegative-integer?]
                            [pixels bytes?]
                            [just-alpha? any/c #f]
                            [pre-multiplied? any/c #f])
           void?]{


Sets a rectangle of pixels in the bitmap, unless
 the DC's current bitmap was produced by @racket[make-screen-bitmap] or 
 @xmethod[canvas% make-bitmap] (in which case @|MismatchExn|).

The pixel RGB values are taken from @racket[pixels]. The first byte
 represents an alpha value, the second byte represents a red value to
 used for the pixel at (@racket[x], @racket[y]), the third byte is a blue
 value, etc. In this way, the first
 @math{@racket[width] * @racket[height] * 4} bytes of @racket[pixels]
 determine the new pixel values in the DC. The pixels are in row-major
 order, left to right then top to bottom.

If @racket[just-alpha?] is false, then the alpha value for each pixel is
 used only if the DC's current bitmap has an alpha channel. If 
 @racket[just-alpha?] is true and the bitmap has no alpha channel, then each
 pixel is set based @italic{only} on the alpha value, but inverted to serve
 as a mask. Thus, when working with bitmaps that have an associated mask
 bitmap instead of an alpha channel, the same
 @racket[pixels] byte string is used with two bitmaps: one
 (the main image) for the pixel values and one (the mask) for the
 alpha values.

If @racket[pre-multiplied?] is true, @racket[just-alpha?] is false,
 and the bitmap has an alpha channel, then RGB values in
 @racket[pixels] are interpreted as scaled by the corresponding alpha value
 (i.e., multiplied by the alpha value and then divided by 255). If an
 R, G, or B value is greater than its corresponding alpha value (which
 is not possible if the value is properly scaled), then it is effectively 
 reduced to the alpha value.

}

@defmethod[(set-bitmap [bitmap (or/c (is-a?/c bitmap%) #f)])
           void?]{

Installs a bitmap into the DC, so that drawing operations on the bitmap
 DC draw to the bitmap. A bitmap is removed from a DC by setting the
 bitmap to @racket[#f].

A bitmap can be selected into at most one bitmap DC, and only when it
 is not used by a control (as a label) or in a @racket[pen%] or
 @racket[brush%] (as a stipple). If the argument to @method[bitmap-dc%
 set-bitmap] is already in use by another DC, a control, a
 @racket[pen%], or a @racket[brush%], @|MismatchExn|.

}

@defmethod[(set-pixel [x real?]
                      [y real?]
                      [color (is-a?/c color%)])
           void?]{

Sets a pixel in the bitmap.

The current clipping region might not affect the pixel change.  Under
 X, interleaving drawing commands with @method[bitmap-dc% set-pixel]
 calls (for the same @racket[bitmap-dc%] object) incurs a substantial
 performance penalty, except for interleaved calls to
 @method[bitmap-dc% get-pixel], @method[bitmap-dc% get-argb-pixels],
 and @method[bitmap-dc% set-argb-pixels].

}}

