#lang scribble/doc
@(require "common.ss")

@defclass/title[bitmap% object% ()]{

A @scheme[bitmap%] object is a pixel-based image, either
 monochrome, color, or color with an alpha channel.

Sometimes, a bitmap object creation fails in a low-level manner. In
 that case, the @method[bitmap% ok?] method returns @scheme[#f], and
 the bitmap cannot be supplied to methods that consume or operate on
 bitmaps (otherwise, @|MismatchExn|).


@defconstructor*/make[(([width (integer-in 1 10000)]
                        [height (integer-in 1 10000)]
                        [monochrome? any/c #f]
                        [alpha? any/c #f])
                       ([in (or/c path-string? input-port?)]
                        [kind (one-of/c 'unknown 'unknown/mask 'unknown/alpha
                                        'gif 'gif/mask 'gif/alpha 
                                        'jpeg 'jpeg/alpha
                                        'png 'png/mask 'png/alpha
                                        'xbm 'xbm/alpha 'xpm 'xpm/alpha
                                        'bmp 'bmp/alpha)
                              'unknown]
                        [bg-color (or/c (is-a?/c color%) false/c) #f])
                       ([bits bytes?]
                        [width (integer-in 1 10000)]
                        [height (integer-in 1 10000)]))]{

When @scheme[width] and @scheme[height] are provided: Creates a new
 bitmap. If @scheme[monochrome?] is true, the bitmap is monochrome; if
 @scheme[monochrome?] is @scheme[#f] and @racket[alpha?] is true, the
 bitmap has an alpha channel; otherwise, the bitmap is color without
 an alpha channel. The initial content of the bitmap is undefined.

When @scheme[in] is provided: Creates a bitmap from a file format,
 where @scheme[kind] specifies the format. See @method[bitmap%
 load-file] for details.


When a @scheme[bits] byte string is provided: Creates a monochrome
 bitmap from an array of bit values, where each byte in @scheme[bits]
 specifies eight bits, and padding bits are added so that each bitmap
 line starts on a character boundary. A @scheme[1] bit value indicates
 black, and @scheme[0] indicates white. If @scheme[width] times
 @scheme[height] is larger than 8 times the length of @scheme[bits],
 @|MismatchExn|.


}

@defmethod[(get-argb-pixels [x real?]
                            [y real?]
                            [width (integer-in 1 10000)]
                            [height (integer-in 1 10000)]
                            [pixels (and/c bytes? mutable?)]
                            [alpha? any/c #f])
           void?]{

Produces the same result as @xmethod[bitmap-dc% get-argb-pixels], but the
bitmap does not have to be selected into the DC (and this method works even if
the bitmap is selected into another DC, attached as a button label, etc.).

}

@defmethod[(get-depth)
           exact-nonnegative-integer?]{

Gets the color depth of the bitmap, which is @racket[1] for a
monochrome bitmap and @racket[32] for a color bitmap. See also
@method[bitmap% is-color?].

}

@defmethod[(get-gl-config [config (is-a?/c gl-config%)])
           void?]{

Returns a copy of this bitmap's requested OpenGL configuration. See
 also @method[bitmap% set-gl-config].

}

@defmethod[(get-height)
           (integer-in 1 10000)]{

Gets the height of the bitmap in pixels.

}

@defmethod[(get-loaded-mask)
           (or/c (is-a?/c bitmap%) false/c)]{

Returns a mask bitmap that is stored with this bitmap.

When a GIF file is loaded with @scheme['gif/mask] or
 @scheme['unknown/mask] and the file contains a transparent ``color,''
 a mask bitmap is generated to identify the transparent pixels. The
 mask bitmap is monochrome, with white pixels where the loaded bitmap
 is transparent and black pixels everywhere else.

When a PNG file is loaded with @scheme['png/mask] or
 @scheme['unknown/mask] and the file contains a mask or alpha channel,
 a mask bitmap is generated to identify the mask or alpha channel.  If
 the file contains a mask or an alpha channel with only extreme
 values, the mask bitmap is monochrome, otherwise it is grayscale
 (representing the alpha channel inverted).

When an XPM file is loaded with @scheme['xpm/mask] or
 @scheme['unknown/mask], a mask bitmap is generated to indicate which
 pixels are set.

When @scheme['unknown/alpha] and similar modes are used to load a
 bitmap, transparency information is instead represented by an alpha
 channel, not by a mask bitmap.

Unlike an alpha channel, the mask bitmap is @italic{not} used
 automatically by drawing routines. The mask bitmap can be extracted
 and supplied explicitly as a mask (e.g., as the sixth argument to
 @method[dc<%> draw-bitmap]). The mask bitmap is used by
 @method[bitmap% save-file] when saving a bitmap as @scheme['png] if
 the mask has the same dimensions as the saved bitmap. The mask bitmap
 is also used automatically when the bitmap is a control label.

}

@defmethod[(get-width)
           (integer-in 1 10000)]{

Gets the width of the bitmap in pixels.

}

@defmethod[(is-color?)
           boolean?]{

Returns @scheme[#f] if the bitmap is monochrome, @scheme[#t] otherwise.

}

@defmethod[(load-file [in (or/c path-string? input-port?)]
                      [kind (one-of/c 'unknown 'unknown/mask 'unknown/alpha
                                      'gif 'gif/mask 'gif/alpha 
                                      'jpeg 'jpeg/alpha
                                      'png 'png/mask 'png/alpha
                                      'xbm 'xbm/alpha 'xpm 'xpm/alpha
                                      'bmp 'bmp/alpha)
                            'unknown]
                      [bg-color (or/c (is-a?/c color%) false/c) #f])
           boolean?]{

Loads a bitmap from a file format that read from @racket[in].
 If the bitmap is in use by a
 @scheme[bitmap-dc%] object or a control, the image data is not
 loaded. The bitmap changes its size and depth to match that of 
 the loaded image.

The @scheme[kind] argument specifies the file's format:

@itemize[
@item{@scheme['unknown] --- examine the file to determine its format; creates either a monochrome
                            or color bitmap without an alpha channel}
@item{@scheme['unknown/mask] --- like @scheme['unknown], but see @method[bitmap% get-loaded-mask]}
@item{@scheme['unknown/alpha] --- like @scheme['unknown], but if the bitmap is color, it has an
                                  alpha channel, and transparency in the image file is recorded
                                  in the alpha channel}
@item{@scheme['gif] --- load a @as-index{GIF} bitmap file, creating a color bitmap}
@item{@scheme['gif/mask] --- like @scheme['gif], but see @method[bitmap% get-loaded-mask]}
@item{@scheme['gif/alpha] --- like @scheme['gif], but with an alpha channel}
@item{@scheme['jpeg] --- load a @as-index{JPEG} bitmap file, creating a color bitmap}
@item{@scheme['jpeg/alpha] --- like @racket['jpeg], but with an alpha channel}
@item{@scheme['png] --- load a @as-index{PNG} bitmap file, creating a color or monochrome bitmap}
@item{@scheme['png/mask] --- like @scheme['png], but see @method[bitmap% get-loaded-mask]}
@item{@scheme['png/alpha] --- like @scheme['png], but always color and with an alpha channel}
@item{@scheme['xbm] --- load an X bitmap (@as-index{XBM}) file; creates a monochrome bitmap}
@item{@scheme['xbm/alpha] --- like @racket['xbm], but creates a color bitmap with an alpha channel}
@item{@scheme['xpm] --- load an @as-index{XPM} bitmap file, creating a color bitmap}
@item{@scheme['xpm/alpha] --- like @racket['xpm], but with an alpha channel}
@item{@scheme['bmp] --- load a Windows bitmap file, creating a color bitmap}
@item{@scheme['bmp/alpha] --- like @racket['bmp], but with an alpha channel}
]

An XBM image is always loaded as a monochrome bitmap. A 1-bit
 grayscale PNG without a mask or alpha channel is also loaded as a
 monochrome bitmap. An image in any other format is always loaded as a
 color bitmap.

For PNG loading, if @scheme[bg-color] is not @scheme[#f], then it is
 combined with the file's alpha channel or mask (if any) while loading
 the image; in this case, no separate mask bitmap is generated and the
 alpha channel fills the bitmap, even if @scheme['unknown/mask],
 @scheme['png/mask] is specified for the format. If the format is
 specified as @scheme['unknown] or @scheme['png] and @scheme[bg-color]
 is not specified, the PNG file is consulted for a background color to
 use for loading, and white is used if no background color is
 indicated in the file.

@index["gamma correction"]{In} all PNG-loading modes, gamma correction
 is applied when the file provides a gamma value, otherwise gamma
 correction is not applied. The current display's gamma factor is
 determined by the @ResourceFirst{gamma} (see @|mrprefsdiscuss|) if it
 is set, or else by the @indexed-envvar{SCREEN_GAMMA} environment
 variable if it is defined. If the preference and environment variable
 are both undefined, a platform-specific default is used.

}

@defmethod[(ok?)
           boolean?]{

Returns @scheme[#t] if the bitmap is usable (created or changed
 successfully). If @scheme[#f] is returned, the bitmap cannot be
 supplied to methods that consume or operate on bitmaps (otherwise,
 @|MismatchExn|).

}

@defmethod[(save-file [name path-string?]
                      [kind (one-of/c 'png 'jpeg 'xbm 'xpm 'bmp)]
                      [quality (integer-in 0 100) 75])
           boolean?]{

Saves a bitmap in the named file.

The @scheme[kind] argument determined the type of file that is created,
 one of:

@itemize[

 @item{@scheme['png] --- save a @as-index{PNG} file}

 @item{@scheme['jpeg] --- save a @as-index{JPEG} file}

 @item{@scheme['xbm] --- save an X bitmap (@as-index{XBM}) file}

 @item{@scheme['xpm] --- save an @as-index{XPM} bitmap file}

 @item{@scheme['bmp] --- save a Windows bitmap file}

]

The @scheme[quality] argument is used only for saving as @scheme['jpeg], in
 which case it specifies the trade-off between image precision (high
 quality matches the content of the @scheme[bitmap%] object more
 precisely) and size (low quality is smaller).

When saving as @scheme['png], if @method[bitmap% get-loaded-mask]
 returns a bitmap of the same size as this one, a grayscale version is
 included in the PNG file as the alpha channel.

A monochrome bitmap saved as @scheme['png] without a mask bitmap
 produces a 1-bit grayscale PNG file (which, when read with
 @method[bitmap% load-file], creates a monochrome @scheme[bitmap%]
 object.)

}

@defmethod[(set-gl-config [config (is-a?/c gl-config%)])
           void?]{

Sets the requested OpenGL configuration for this bitmap. The
 configuration is used when the bitmap selected into a drawing
 context, and then a GL context is created for the drawing context.

The given @scheme[gl-config%] object is copied, so that changes to
 the object do not affect the bitmap's configuration.

}

@defmethod[(set-loaded-mask [mask (is-a?/c bitmap%)])
           void?]{

See @method[bitmap% get-loaded-mask].

}}

