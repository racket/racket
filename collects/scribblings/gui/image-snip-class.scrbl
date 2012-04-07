#lang scribble/doc
@(require "common.rkt")

@defclass/title[image-snip% snip% ()]{

An @racket[image-snip%] is a snip that can display bitmap images
 (usually loaded from a file). When the image file cannot be found, a
 box containing an ``X'' is drawn.


@defconstructor*/make[(([file (or/c path-string? input-port? #f) #f]
                        [kind (or/c 'unknown 'unknown/mask 'unknown/alpha
                                    'gif 'gif/mask 'gif/alpha 
                                    'jpeg 'png 'png/mask 'png/alpha
                                    'xbm 'xpm 'bmp 'pict) 'unknown]
                        [relative-path? any/c #f]
                        [inline? any/c #t])
                       ([bitmap (is-a?/c bitmap%)]
                        [mask (or/c (is-a?/c bitmap%) #f) #f]))]{

Creates an image snip, loading the image @racket[file] if
 specified (see also @method[image-snip% load-file]), or using the
 given @racket[bitmap].

}


@defmethod[(equal-hash-code-of [hash-code (any/c . -> . exact-integer?)])
           exact-integer?]{

Returns an integer that can be used as a @racket[equal?]-based hash
code for @this-obj[] (using the same notion of @racket[equal?] as
@method[image-snip% other-equal-to?]).

See also @racket[equal<%>].}

@defmethod[(equal-secondary-hash-code-of [hash-code (any/c . -> . exact-integer?)])
           exact-integer?]{

Returns an integer that can be used as a @racket[equal?]-based
secondary hash code for @this-obj[] (using the same notion of
@racket[equal?] as @method[image-snip% other-equal-to?]).

See also @racket[equal<%>].}

@defmethod[(get-bitmap)
           (or/c (is-a?/c bitmap%) #f)]{

Returns the bitmap that is displayed by the snip, whether set through
 @method[image-snip% set-bitmap] or @method[image-snip% load-file]. If
 no bitmap is displayed, the result is @racket[#f].

}

@defmethod[(get-bitmap-mask)
           (or/c (is-a?/c bitmap%) #f)]{

Returns the mask bitmap that is used for displaying by the snip, if
 one was installed with @method[image-snip% set-bitmap].  If no mask
 is used, the result is @racket[#f].

}

@defmethod[(get-filename [relative-path (or/c (box/c any/c) #f) #f])
           (or/c path-string? #f)]{

Returns the name of the currently loaded, non-inlined file, or
 @racket[#f] if a file is not loaded or if a file was loaded with
 inlining (the default).

@boxisfillnull[@racket[relative-path] @elem{@racket[#t] if the loaded file's path is
relative to the owning editor's path}]

}

@defmethod[(get-filetype)
           (or/c 'unknown 'unknown/mask 'unknown/alpha
                 'gif 'gif/mask 'gif/alpha 
                 'jpeg 'png 'png/mask 'png/alpha
                 'xbm 'xpm 'bmp 'pict)]{

Returns the kind used to load the currently loaded, non-inlined file,
 or @racket['unknown] if a file is not loaded or if a file was loaded
 with inlining (the default).

}

@defmethod[(load-file [file (or/c path-string? input-port? #f)]
                      [kind (or/c 'unknown 'unknown/mask 'unknown/alpha
                                  'gif 'gif/mask 'gif/alpha 
                                  'jpeg 'png 'png/mask 'png/alpha
                                  'xbm 'xpm 'bmp 'pict) 'unknown]
                      [relative-path? any/c #f]
                      [inline? any/c #t])
           void?]{

Loads the file by passing @racket[file] and @racket[kind] to
 @xmethod[bitmap% load-file]. If a bitmap had previously been specified
 with @method[image-snip% set-bitmap], that bitmap (and mask) will no
 longer be used. If @racket[file] is @racket[#f], then the current
 image is cleared.

When @racket['unknown/mask], @racket['gif/mask], or @racket['png/mask]
 is specified and the loaded bitmap object includes a mask (see
 @method[bitmap% get-loaded-mask]), the mask is used for drawing the
 bitmap (see @method[dc<%> draw-bitmap]). The @racket['unknown/alpha],
 @racket['gif/alpha], or @racket['png/alpha] variants are recommended,
 however.

If @racket[relative-path?] is not @racket[#f] and @racket[file] is a
 relative path, then the file will be read using the path of the
 owning editor's filename. If the image is not inlined, it will be
 saved as a relative pathname.

If @racket[inline?] is not @racket[#f], the image data will be saved
 directly to the file or clipboard when the image is saved or copied
 (preserving the bitmap's mask, if any).  The source filename and kind
 is no longer relevant.

}

@defmethod[(other-equal-to? [snip (is-a?/c image-snip%)]
                            [equal? (any/c any/c . -> . boolean?)])
           boolean?]{

Returns @racket[#t] if @this-obj[] and @racket[snip] both have bitmaps
and the bitmaps are the same. If either has a mask bitmap
with the same dimensions as the main bitmap, then the masks must be
the same (or if only one mask is present, it must correspond to a
solid mask).

The given @racket[equal?] function (for recursive comparisons) is not
used.}


@defmethod[#:mode override 
           (resize [w (and/c real? (not/c negative?))]
                   [h (and/c real? (not/c negative?))])
           boolean?]{

The bitmap will be cropped to fit in the given dimensions.

}

@defmethod[(set-bitmap [bm (is-a?/c bitmap%)]
                       [mask (or/c (is-a?/c bitmap%) #f) #f])
           void?]{

Sets the bitmap that is displayed by the snip.

An optional @racket[mask] is used when drawing the bitmap (see
 @method[dc<%> draw-bitmap]), but supplying the mask directly is
 deprecated. If no mask is supplied but the bitmap's
 @method[bitmap% get-loaded-mask] method produces a bitmap of the same
 dimensions, it is used as the mask; furthermore, such a mask is saved
 with the snip when it is saved to a file or copied (whereas a
 directly supplied mask is not saved). Typically, however, @racket[bm]
 instead should have an alpha channel instead of a separate mask bitmap.

}

@defmethod[(set-offset [dx real?]
                       [dy real?])
           void?]{

Sets a graphical offset for the bitmap within the image snip.

}}

