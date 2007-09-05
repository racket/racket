#reader(lib "defreader.ss" "scribble")
@require["common.ss"]
@require["snip-class.scrbl"]

@define-class-doc[image-snip% snip% ()]{

An @scheme[image-snip%] is a snip that can display bitmap images
 (usually loaded from a file). When the image file cannot be found, a
 box containing an ``X'' is drawn.


@defconstructor*/make[(([filename (or/c path-string? false/c) #f]
                        [kind (one-of/c 'unknown 'unknown/mask 
                                        'gif 'gif/mask 
                                        'jpeg 'png 'png/mask 
                                        'xbm 'xpm 'bmp 'pict) 'unknown]
                        [relative-path? any/c #f]
                        [inline? any/c #t])
                       ([bitmap (is-a?/c bitmap%)]
                        [mask (or/c (is-a?/c bitmap%) false/c) #f]))]{

Creates an image snip, loading the image @scheme[filename] if
 specified (see also @method[image-snip% load-file]), or using the
 given @scheme[bitmap].

}

@defmethod[(get-bitmap)
           (or/c (is-a?/c bitmap%) false/c)]{

Returns the bitmap that is displayed by the snip, whether set through
 @method[image-snip% set-bitmap] or @method[image-snip% load-file]. If
 no bitmap is displayed, the result is @scheme[#f].

The returned bitmap cannot be selected into a @scheme[bitmap-dc%] as
 long as it belongs to the snip, but it can be used as a pen or
 brush stipple.

}

@defmethod[(get-bitmap-mask)
           (or/c (is-a?/c bitmap%) false/c)]{

Returns the mask bitmap that is used for displaying by the snip, if
 one was installed with @method[image-snip% set-bitmap].  If no mask
 is used, the result is @scheme[#f].

The returned bitmap cannot be selected into a @scheme[bitmap-dc%] as
 long as it belongs to the snip, but it can be used as a pen or
 brush stipple.

}

@defmethod[(get-filename [relative-path (or/c (box/c any/c) false/c) #f])
           (or/c path-string? false/c)]{

Returns the name of the currently loaded, non-inlined file, or
 @scheme[#f] if a file is not loaded or if a file was loaded with
 inlining (the default).

@boxisfillnull[(scheme relative-path) @elem{@scheme[#t] if the loaded file's path is
relative to the owning editor's path}]

}

@defmethod[(get-filetype)
           (one-of/c 'unknown 'unknwon/mask
                     'gif 'gif/mask 
                     'jpeg 'png 'png/mask 'xbm 'xpm 'bmp 'pict)]{

Returns the kind used to load the currently loaded, non-inlined file,
 or @scheme['unknown] if a file is not loaded or if a file was loaded
 with inlining (the default).

}

@defmethod[(load-file [filename (or/c path-string? false/c)]
                      [kind (one-of/c 'unknown 'unknown/mask 
                                      'gif 'gif/mask 
                                      'jpeg 'png 'png/mask 
                                      'xbm 'xpm 'bmp 'pict) 'unknown]
                      [relative-path? any/c #f]
                      [inline? any/c #t])
           void?]{

Loads the file by passing @scheme[filename] and @scheme[kind] to
 @method[bitmap% load-file] If a bitmap had previously been specified
 with @method[image-snip% set-bitmap], that bitmap (and mask) will no
 longer be used. If @scheme[filename] is @scheme[#f], then the current
 image is cleared.

When @scheme['unknown/mask], @scheme['gif/mask], or @scheme['png/mask]
 is specified and the loaded bitmap object includes a mask (see
 @method[bitmap% get-loaded-mask]), the mask is used for drawing the
 bitmap (see @method[dc<%> draw-bitmap]).

If @scheme[relative-path?] is not @scheme[#f] and @scheme[filename] is a
 relative path, then the file will be read using the path of the
 owning editor's filename. If the image is not inlined, it will be
 saved as a relative pathname.

If @scheme[inline?] is not @scheme[#f], the image data will be saved
 directly to the file or clipboard when the image is saved or copied
 (preserving the bitmap's mask, if any).  The source filename and kind
 is no longer relevant.

}

@defmethod[#:mode override 
           (resize [w (and/c real? (not/c negative?))]
                   [h (and/c real? (not/c negative?))])
           void?]{

The bitmap will be cropped to fit in the given dimensions.

}

@defmethod[(set-bitmap [bm (is-a?/c bitmap%)]
                       [mask (or/c (is-a?/c bitmap%) false/c) #f])
           void?]{

Sets the bitmap that is displayed by the snip. This method also
 accepts an optional mask to be used when drawing the bitmap (see
 @method[dc<%> draw-bitmap]), but supplying the mask directly is now
 deprecated. Instead, if no mask is supplied but the bitmap's
 @method[bitmap% get-loaded-mask] method produces a bitmap of the same
 dimensions, it is used as the mask. Furthermore, such a mask is saved
 with the snip when it is saved to a file or copied (whereas a
 directly supplied mask is not saved).

The supplied bitmap must not be selected into a @scheme[bitmap-dc%]
 object, otherwise @|MismatchExn|, and it cannot be selected into
 a @scheme[bitmap-dc%] as long as it belongs to the snip, but it
 can be used as a pen or brush stipple.

}

@defmethod[(set-offset [dx real?]
                       [dy real?])
           void?]{

Sets a graphical offset for the bitmap within the image snip.

}}

