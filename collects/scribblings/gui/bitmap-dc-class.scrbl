#lang scribble/doc
@(require "common.ss")

@defclass/title[bitmap-dc% object% (dc<%>)]{

A @scheme[bitmap-dc%] object allows drawing directly into a bitmap.  A
 @scheme[bitmap%] object must be supplied at initialization or
 installed into a bitmap DC using @method[bitmap-dc% set-bitmap]
 before any other method of the DC is called, except @method[dc<%>
 get-text-extent], @method[dc<%> get-char-height], or @method[dc<%>
 get-char-width]. If any other @scheme[bitmap-dc%] method is called
 before a bitmap is selected, the method call is ignored.

Drawing to a @scheme[bitmap-dc%] with a color bitmap is guaranteed to
 produce the same result as drawing into a @scheme[canvas%] instance
 (with appropriate clipping and offsets). Thus, a @scheme[bitmap-dc%]
 can be used for offscreen staging of canvas content.


@defconstructor[([bitmap (or/c (is-a?/c bitmap%) false/c)])]{

Creates a new memory DC. If @scheme[bitmap] is not @scheme[#f], it is
 installed into the DC so that drawing commands on the DC draw to
 @scheme[bitmap]. Otherwise, no bitmap is installed into the DC and
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
                                       [mask (or/c (is-a?/c bitmap%) false/c)])
           boolean?]{

Display part of a bitmap with smooth scaling. For most platforms, this
 method produces better results than adjusting the scale of a drawing
 context before using @method[dc<%> draw-bitmap] and @method[dc<%>
 draw-bitmap-section], but this method is much slower.

}

@defmethod[(get-argb-pixels [x real?]
                            [y real?]
                            [width (integer-in 1 10000)]
                            [height (integer-in 1 10000)]
                            [pixels (and/c bytes? (not/c immutable?))]
                            [alpha? any/c #f])
           void?]{

Gets a rectangle of pixels in the bitmap, subject to the same rules
 and performance characteristics of @method[bitmap-dc% get-pixel],
 except that the block get is likely to be faster than the sequence of
 individual gets. Also, the @scheme[bitmap%] class also provides the
 same method directly, so it is not necessary to select a bitmap into
 a DC to extracts its pixel values.

The pixel RGB values are copied into @scheme[pixels]. The first byte
 represents an alpha value of the pixel at (@scheme[x], @scheme[y]),
 the second byte represents a red value of the pixel at (@scheme[x],
 @scheme[y]), the third byte is the blue value, etc. In this way, the
 first @math{@scheme[width] * @scheme[height] *4} bytes of
 @scheme[pixels] are set to reflect the current pixel values in the
 DC. The pixels are in row-major order, left to right then top to
 bottom.

If @scheme[alpha?] is false, if the bitmap does not have an alpha
 channel, then the alpha value for each pixel is set to 255. If
 @scheme[alpha?] is true, then @italic{only} the alpha value is set
 for each pixel, based on each pixel's inverted value. Thus, when a
 bitmap has a separate mask bitmap, the same @scheme[pixels] byte
 string is in general filled from two bitmaps: one (the main image)
 for the pixel values and one (the mask) for the alpha values.

}

@defmethod[(get-bitmap)
           (or/c (is-a?/c bitmap%) false/c)]{

Gets the bitmap currently installed in the DC, or @scheme[#f] if no
 bitmap is installed. See @method[bitmap-dc% set-bitmap] for more
 information.

}

@defmethod[(get-pixel [x real?]
                      [y real?]
                      [color (is-a?/c color%)])
           boolean?]{

Fills @scheme[color] with the color of the current pixel at position
(@scheme[x], @scheme[y]) in the drawing context. If the color is
successfully obtained, the return value is @scheme[#t], otherwise the
result is @scheme[#f].

Under X, interleaving drawing commands with @method[bitmap-dc%
get-pixel] calls (for the same @scheme[bitmap-dc%] object) incurs a
substantial performance penalty, except for interleaved calls to
@method[bitmap-dc% set-pixel], @method[bitmap-dc% set-argb-pixels],
and @method[bitmap-dc% get-argb-pixels].

}

@defmethod[(set-argb-pixels [x real?]
                            [y real?]
                            [width (integer-in 1 10000)]
                            [height (integer-in 1 10000)]
                            [pixels bytes?]
                            [alpha? any/c #f])
           void?]{


Sets a rectangle of pixels in the bitmap, subject to the same
 rules and performance characteristics of 
@method[bitmap-dc% set-pixel], except that the block set is likely to be faster than the
 sequence of individual sets.

The pixel RGB values are taken from @scheme[pixels]. The first byte
 represents an alpha value, the second byte represents a red value to
 used for the pixel at (@scheme[x], @scheme[y]), the third byte is a blue
 value, etc. In this way, the first
 @math{@scheme[width] * @scheme[height] * 4} bytes of @scheme[pixels]
 determine the new pixel values in the DC. The pixels are in row-major
 order, left to right then top to bottom.

If @scheme[alpha?] is false, then the alpha value for each pixel is
 ignored. If @scheme[alpha?] is true, then each
 pixel is set based @italic{only} on the alpha value, but inverted to serve
 as a mask. Thus, the same
 @scheme[pixels] byte string is in general used with two bitmaps, one
 (the main image) for the pixel values and one (the mask) for the
 alpha values.

}

@defmethod[(set-bitmap [bitmap (or/c (is-a?/c bitmap%) false/c)])
           void?]{

Installs a bitmap into the DC, so that drawing operations on the bitmap
 DC draw to the bitmap. A bitmap is removed from a DC by setting the
 bitmap to @scheme[#f].

A bitmap can be selected into at most one bitmap DC, and only when it
 is not used by a control (as a label) or in a @scheme[pen%] or
 @scheme[brush%] (as a stipple). If the argument to @method[bitmap-dc%
 set-bitmap] is already in use by another DC, a control, a
 @scheme[pen%], or a @scheme[brush%], @|MismatchExn|.

}

@defmethod[(set-pixel [x real?]
                      [y real?]
                      [color (is-a?/c color%)])
           void?]{

Sets a pixel in the bitmap.

The current clipping region might not affect the pixel change.  Under
 X, interleaving drawing commands with @method[bitmap-dc% set-pixel]
 calls (for the same @scheme[bitmap-dc%] object) incurs a substantial
 performance penalty, except for interleaved calls to
 @method[bitmap-dc% get-pixel], @method[bitmap-dc% get-argb-pixels],
 and @method[bitmap-dc% set-argb-pixels].

}}

