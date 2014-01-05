#lang scribble/doc
@(require "common.rkt"
          (for-label (only-in ffi/unsafe cpointer?)))

@defclass/title[bitmap% object% ()]{

A @racket[bitmap%] object is a pixel-based image, either monochrome,
 color, or color with an alpha channel. See also @racket[make-bitmap],
 @racket[make-platform-bitmap], @racket[make-screen-bitmap] (from
 @racketmodname[racket/gui/base]), @xmethod[canvas% make-bitmap] (from
 @racketmodname[racket/gui/base]), and @secref["Portability"].

A bitmap has a @deftech{backing scale}, which is the number of pixels
 that correspond to a drawing unit for the bitmap, either when the
 bitmap is used as a target for drawing or when the bitmap is drawn
 into another context.  For example, on Mac OS X when the main monitor
 is in Retina mode, @racket[make-screen-bitmap] returns a bitmap whose
 backing scale is @racket[2.0]. A monochrome bitmap always has a
 backing scale of @racket[1.0].

A bitmap is convertible to @racket['png-bytes] through the
@racketmodname[file/convertible] protocol.


@defconstructor*/make[(([width exact-positive-integer?]
                        [height exact-positive-integer?]
                        [monochrome? any/c #f]
                        [alpha? any/c #f]
                        [backing-scale (>/c 0.0) 1.0])
                       ([in (or/c path-string? input-port?)]
                        [kind (or/c 'unknown 'unknown/mask 'unknown/alpha
                                    'gif 'gif/mask 'gif/alpha 
                                    'jpeg 'jpeg/alpha
                                    'png 'png/mask 'png/alpha
                                    'xbm 'xbm/alpha 'xpm 'xpm/alpha
                                    'bmp 'bmp/alpha)
                              'unknown]
                        [bg-color (or/c (is-a?/c color%) #f) #f]
                        [complain-on-failure? any/c #f]
                        [backing-scale (>/c 0.0) 1.0])
                       ([bits bytes?]
                        [width exact-positive-integer?]
                        [height exact-positive-integer?]))]{

The @racket[make-bitmap], @racket[make-monochrome-bitmap], and
 @racket[read-bitmap] functions create @racket[bitmap%] instances, but
 they are also preferred over using @racket[make-object] 
 with @racket[bitmap%] directly, because the functions are
 less overloaded and they enable alpha channels by default.
 See also @secref["Portability"].
 
When @racket[width] and @racket[height] are provided: Creates a new
 bitmap. If @racket[monochrome?] is true, the bitmap is monochrome; if
 @racket[monochrome?] is @racket[#f] and @racket[alpha?] is true, the
 bitmap has an alpha channel; otherwise, the bitmap is color without
 an alpha channel. The @racket[backing-scale] argument sets the
 bitmap's @tech{backing scale}, and it must be @racket[1.0] if
 @racket[monochrome] is true.
 The initial content of the bitmap is ``empty'': all white, and with
 zero alpha in the case of a bitmap with an alpha channel.

When @racket[in] is provided: Creates a bitmap from a file format,
 where @racket[kind] specifies the format. See @method[bitmap%
 load-file] for details. The @racket[backing-scale] argument sets the
 bitmap's @tech{backing scale}, so that the bitmap's size (as reported
 by @method[bitmap% get-width] and @method[bitmap% get-height]) is the
 @racket[ceiling] of the bitmap's size from @racket[in] divided by
 @racket[backing-scale]; the backing scale must be @racket[1.0] if the
 bitmap is monocrhome or loaded with a mask.

When a @racket[bits] byte string is provided: Creates a monochrome
 bitmap from an array of bit values, where each byte in @racket[bits]
 specifies eight bits, and padding bits are added so that each bitmap
 line starts on a character boundary. A @racket[1] bit value indicates
 black, and @racket[0] indicates white. If @racket[width] times
 @racket[height] is larger than 8 times the length of @racket[bits],
 @|MismatchExn|.

@history[#:changed "1.1" @elem{Added the @racket[backing-scale]
optional arguments.}]}

@defmethod[(get-argb-pixels [x real?]
                            [y real?]
                            [width exact-nonnegative-integer?]
                            [height exact-nonnegative-integer?]
                            [pixels (and/c bytes? (not/c immutable?))]
                            [just-alpha? any/c #f]
                            [pre-multiplied? any/c #f]
                            [#:unscaled? unscaled? any/c #f])
           void?]{

Produces the same result as @xmethod[bitmap-dc% get-argb-pixels] when
@racket[unscaled?] is @racket[#f], but the bitmap does not have to be
selected into the DC (and this method works even if the bitmap is
selected into another DC, attached as a button label, etc.).

If the bitmap has a @tech{backing scale} other than @racket[1.0] and
@racket[unscaled?] is true, then the result corresponds to the
bitmap's pixels ignoring the @tech{backing scale}. In that case,
@racket[x], @racket[y], @racket[width], and @racket[height] are
effectively in pixels instead of drawing units.

@history[#:changed "1.1" @elem{Added the @racket[#:unscaled?]
optional argument.}]}


@defmethod[(get-backing-scale)
           (>/c 0.0)]{

Returns the bitmap's @tech{backing scale}.

@history[#:added "1.1"]}


@defmethod[(get-depth)
           exact-nonnegative-integer?]{

Gets the color depth of the bitmap, which is @racket[1] for a
monochrome bitmap and @racket[32] for a color bitmap. See also
@method[bitmap% is-color?].

}


@defmethod[(get-handle) cpointer?]{

Returns a low-level handle to the bitmap content. Currently, on all
platforms, a handle is a @tt{cairo_surface_t}. For a bitmap created
with @racket[make-bitmap], the handle is specifically a Cairo
image surface.}


@defmethod[(get-height)
           exact-positive-integer?]{

Gets the height of the bitmap in drawing units (which is the same as
pixels if the @tech{backing scale} is 1.0).}


@defmethod[(get-loaded-mask)
           (or/c (is-a?/c bitmap%) #f)]{

Returns a mask bitmap that is stored with this bitmap.

When a GIF file is loaded with @racket['gif/mask] or
 @racket['unknown/mask] and the file contains a transparent ``color,''
 a mask bitmap is generated to identify the transparent pixels. The
 mask bitmap is monochrome, with white pixels where the loaded bitmap
 is transparent and black pixels everywhere else.

When a PNG file is loaded with @racket['png/mask] or
 @racket['unknown/mask] and the file contains a mask or alpha channel,
 a mask bitmap is generated to identify the mask or alpha channel.  If
 the file contains a mask or an alpha channel with only extreme
 values, the mask bitmap is monochrome, otherwise it is grayscale
 (representing the alpha channel inverted).

When an XPM file is loaded with @racket['xpm/mask] or
 @racket['unknown/mask], a mask bitmap is generated to indicate which
 pixels are set.

When @racket['unknown/alpha] and similar modes are used to load a
 bitmap, transparency information is instead represented by an alpha
 channel, not by a mask bitmap.

Unlike an alpha channel, the mask bitmap is @italic{not} used
 automatically by drawing routines. The mask bitmap can be extracted
 and supplied explicitly as a mask (e.g., as the sixth argument to
 @method[dc<%> draw-bitmap]). The mask bitmap is used by
 @method[bitmap% save-file] when saving a bitmap as @racket['png] if
 the mask has the same dimensions as the saved bitmap. The mask bitmap
 is also used automatically when the bitmap is a control label.

}

@defmethod[(get-width)
           exact-positive-integer?]{

Gets the width of the bitmap in drawing units (which is the same as
pixels of the @tech{backing scale} is 1.0).}


@defmethod[(has-alpha-channel?)
           boolean?]{

Returns @racket[#t] if the bitmap has an alpha channel,
@racket[#f] otherwise.

}

@defmethod[(is-color?)
           boolean?]{

Returns @racket[#f] if the bitmap is monochrome, @racket[#t] otherwise.

}

@defmethod[(load-file [in (or/c path-string? input-port?)]
                      [kind (or/c 'unknown 'unknown/mask 'unknown/alpha
                                  'gif 'gif/mask 'gif/alpha 
                                  'jpeg 'jpeg/alpha
                                  'png 'png/mask 'png/alpha
                                  'xbm 'xbm/alpha 'xpm 'xpm/alpha
                                  'bmp 'bmp/alpha)
                            'unknown]
                      [bg-color (or/c (is-a?/c color%) #f) #f]
                      [complain-on-failure? any/c #f])
           boolean?]{

Loads a bitmap from a file format that read from @racket[in], unless
 the bitmap was produced by @racket[make-platform-bitmap], @racket[make-screen-bitmap],
 or @xmethod[canvas% make-bitmap] (in which case @|MismatchExn|).
 If the bitmap is in use by a
 @racket[bitmap-dc%] object or a control, the image data is not
 loaded. The bitmap changes its size and depth to match that of 
 the loaded image. If an error is encountered when reading the file format,
 an exception is raised only if @racket[complain-on-failure?] is true (which is
 @emph{not} the default).

The @racket[kind] argument specifies the file's format:

@itemize[

@item{@racket['unknown] --- examine the file to determine its format; creates
  either a monochrome or color bitmap without an alpha channel}
@item{@racket['unknown/mask] --- like @racket['unknown], but see
  @method[bitmap% get-loaded-mask]}
@item{@racket['unknown/alpha] --- like @racket['unknown], but if the bitmap is
  color, it has an alpha channel, and transparency in the image file is
  recorded in the alpha channel}
@item{@racket['gif] --- load a @as-index{GIF} bitmap file, creating a color
  bitmap}
@item{@racket['gif/mask] --- like @racket['gif], but see
  @method[bitmap% get-loaded-mask]}
@item{@racket['gif/alpha] --- like @racket['gif], but with an alpha channel}
@item{@racket['jpeg] --- load a @as-index{JPEG} bitmap file, creating a color
  bitmap}
@item{@racket['jpeg/alpha] --- like @racket['jpeg], but with an alpha channel}
@item{@racket['png] --- load a @as-index{PNG} bitmap file, creating a color or
  monochrome bitmap}
@item{@racket['png/mask] --- like @racket['png], but see
  @method[bitmap% get-loaded-mask]}
@item{@racket['png/alpha] --- like @racket['png], but always color and with an
  alpha channel}
@item{@racket['xbm] --- load an X bitmap (@as-index{XBM}) file; creates a
  monochrome bitmap}
@item{@racket['xbm/alpha] --- like @racket['xbm], but creates a color bitmap
  with an alpha channel}
@item{@racket['xpm] --- load an @as-index{XPM} bitmap file, creating a color
  bitmap}
@item{@racket['xpm/alpha] --- like @racket['xpm], but with an alpha channel}
@item{@racket['bmp] --- load a Windows bitmap file, creating a color bitmap}
@item{@racket['bmp/alpha] --- like @racket['bmp], but with an alpha channel}
]

An XBM image is always loaded as a monochrome bitmap. A 1-bit
 grayscale PNG without a mask or alpha channel is also loaded as a
 monochrome bitmap. An image in any other format is always loaded as a
 color bitmap.

For PNG loading, if @racket[bg-color] is not @racket[#f], then it is
 combined with the file's alpha channel or mask (if any) while loading
 the image; in this case, no separate mask bitmap is generated and the
 alpha channel fills the bitmap, even if @racket['unknown/mask],
 @racket['png/mask] is specified for the format. If the format is
 specified as @racket['unknown] or @racket['png] and @racket[bg-color]
 is not specified, the PNG file is consulted for a background color to
 use for loading, and white is used if no background color is
 indicated in the file.

@index["gamma correction"]{In} all PNG-loading modes, gamma correction
 is applied when the file provides a gamma value, otherwise gamma
 correction is not applied. The current display's gamma factor is
 determined by the @indexed-envvar{SCREEN_GAMMA} environment
 variable if it is defined. If the preference and environment variable
 are both undefined, a platform-specific default is used.

After a bitmap is created, @method[bitmap% load-file] can be used
 only if the bitmap's @tech{backing scale} is @racket[1.0].}


@defmethod[(make-dc)
           (is-a?/c bitmap-dc%)]{

Return @racket[(make-object bitmap-dc% this)].}


@defmethod[(ok?)
           boolean?]{

Returns @racket[#t] if the bitmap is valid in the sense that an image
 file was loaded successfully. If @method[bitmap% ok?] returns
 @racket[#f], then drawing to or from the bitmap has no effect.

}


@defmethod[(save-file [name (or/c path-string? output-port?)]
                      [kind (or/c 'png 'jpeg 'xbm 'xpm 'bmp)]
                      [quality (integer-in 0 100) 75]
                      [#:unscaled? unscaled? any/c #f])
           boolean?]{

Writes a bitmap to the named file or output stream.

The @racket[kind] argument determined the type of file that is created,
 one of:

@itemize[

 @item{@racket['png] --- save a @as-index{PNG} file}

 @item{@racket['jpeg] --- save a @as-index{JPEG} file}

 @item{@racket['xbm] --- save an X bitmap (@as-index{XBM}) file}

 @item{@racket['xpm] --- save an @as-index{XPM} bitmap file}

 @item{@racket['bmp] --- save a Windows bitmap file}

]

The @racket[quality] argument is used only for saving as @racket['jpeg], in
 which case it specifies the trade-off between image precision (high
 quality matches the content of the @racket[bitmap%] object more
 precisely) and size (low quality is smaller).

When saving as @racket['png], if @method[bitmap% get-loaded-mask]
 returns a bitmap of the same size as this one, a grayscale version is
 included in the PNG file as the alpha channel.

A monochrome bitmap saved as @racket['png] without a mask bitmap
 produces a 1-bit grayscale PNG file (which, when read with
 @method[bitmap% load-file], creates a monochrome @racket[bitmap%]
 object.)

If the bitmap has a @tech{backing scale} other than 1.0, then it is
 effectively converted to a single pixel per drawing unit before
 saving unless @racket[unscaled?] is true.

@history[#:changed "1.1" @elem{Added the @racket[#:unscaled?]
optional argument.}]}


@defmethod[(set-argb-pixels [x real?]
                            [y real?]
                            [width exact-nonnegative-integer?]
                            [height exact-nonnegative-integer?]
                            [pixels bytes?]
                            [just-alpha? any/c #f]
                            [pre-multiplied? any/c #f]
                            [#:unscaled? unscaled? any/c #f])
           void?]{

The same as @xmethod[bitmap-dc% set-argb-pixels] when
@racket[unscaled?] is @racket[#f], but the bitmap does not have to be
selected into the DC.

If the bitmap has a @tech{backing scale} other than @racket[1.0] and
@racket[unscaled?] is true, then pixel values are installed ignoring
the @tech{backing scale}. In that case, @racket[x], @racket[y],
@racket[width], and @racket[height] are effectively in pixels instead
of drawing units.

@history[#:changed "1.1" @elem{Added the @racket[#:unscaled?]
optional argument.}]}


@defmethod[(set-loaded-mask [mask (is-a?/c bitmap%)])
           void?]{

See @method[bitmap% get-loaded-mask].

}}

