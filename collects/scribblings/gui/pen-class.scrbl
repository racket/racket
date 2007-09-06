#reader(lib "docreader.ss" "scribble")
@require["common.ss"]

@defclass/title[pen% object% ()]{

A pen is a drawing tool with a color, width, and style. A pen draws
 lines and outlines, such as the outline of a rectangle. On a
 monochrome display, all non-white pens are drawn as black.

In addition to its color, width, and style, a pen can have a stipple
 bitmap that is a 8 x 8 monochrome bitmap. This stipple is used only
 in unsmoothed mode (see @method[dc<%> set-smoothing]) or in a
 PostScript drawing context. Painting with a stipple pen is similar to
 calling @method[dc<%> draw-bitmap] with the stipple bitmap in region
 painted by the pen.

A pen's style is one of the following:

@itemize{

 @item{@indexed-scheme['transparent] --- Draws with no effect (on the
       outline of the drawn shape).}

 @item{@indexed-scheme['solid] --- Draws using the pen's color. If a
        (monochrome) stipple is installed into the pen, black pixels
        from the stipple are transferred to the destination using the
        brush's color, and white pixels from the stipple are not
        transferred.}

 @item{@indexed-scheme['xor] --- In unsmoothed mode, the pen's color
       or colored stipple is xor-ed with existing destination pixel
       values. The @scheme['xor] mapping is unspecified for arbitrary
       color combinations, but the mapping provides two guarantees:
       @itemize{

       @item{Black-and-white drawing to a color or monochrome
       destination always works as expected: black xor white = black,
       white xor black = black, black xor black = white, and white xor
       white = white.}

       @item{Performing the same drawing operation twice in a row with
       @scheme['xor] is equivalent to a no-op.}

       }
       In a smoothing mode, @scheme['xor] is equivalent to @scheme['solid].}

 @item{@indexed-scheme['hilite] --- In unsmoothed mode, existing
       destination pixels are ``highlighted'' in a platform-specific
       way when the pen color is black. Under Windows for a color
       drawing context, the inverted RGB components of destination
       pixel are combined with the RGB components of the system-wide
       highlight color using a bitwise ``or'', and the combination is
       used. Under Mac OS X for a color drawing context, the
       inverted RGB components of the system-wide highlight color are
       subtracted from the RGB components of each destination pixel,
       and the difference (or 0 for a negative result) is used. Under
       X or for any monochrome drawing context, @scheme['hilite] is the
       same as @scheme['xor].  In a smoothing mode, @scheme['hilite] is
       treated like @scheme['solid].}

 @item{The following special pen modes use the pen's color, and they only
       apply when a stipple is not used:
    @itemize{
  @item{@indexed-scheme['dot]}
  @item{@indexed-scheme['long-dash]}
  @item{@indexed-scheme['short-dash]}
  @item{@indexed-scheme['dot-dash]}
  @item{@indexed-scheme['xor-dot]}
  @item{@indexed-scheme['xor-long-dash]}
  @item{@indexed-scheme['xor-short-dash]}
  @item{@indexed-scheme['xor-dot-dash]}
  }}

}

To avoid creating multiple pens with the same characteristics, use the
 global @scheme[pen-list%] object @indexed-scheme[the-pen-list], or
 provide a color, width, and style to @xmethod[dc<%> set-pen].

A pen of size @scheme[0] uses the minimum line size for the
 destination drawing context.  In (unscaled) canvases and bitmaps in
 unsmoothed mode, a zero-width pen behaves the nearly same as a pen of
 size @scheme[1]. In a smoothing mode (including all
 @scheme[post-script-dc%] drawing), a pen of size @scheme[0] draws a
 line thinner than a pen of size @scheme[1]. If the pen's width is not
 an integer, then the width is truncated to an integer (even before
 scaling) in unsmoothed mode.




@defconstructor*/make[(()
                       ([color (is-a?/c color%)]
                        [width (real-in 0 255)]
                        [style (one-of/c 'transparent 'solid 'xor 'hilite 
                                         'dot 'long-dash 'short-dash 'dot-dash 
                                         'xor-dot 'xor-long-dash 'xor-short-dash 
                                         'xor-dot-dash)])
                       ([color-name string?]
                        [width (real-in 0 255)]
                        [style (one-of/c 'transparent 'solid 'xor 'dot 'hilite
                                         'long-dash 'short-dash 'dot-dash
                                         'xor-dot 'xor-long-dash 'xor-short-dash
                                         'xor-dot-dash)]))]{

When no argument are provided, the result is a solid black pen of
 width @scheme[0].  Otherwise, the result is a pen with the given
 color, width, and style. For the case that the color is specified
 using a name, see @scheme[color-database<%>] for information about
 color names; if the name is not known, the pen's color is black.

}

@defmethod[(get-cap)
           (one-of/c 'round 'projecting 'butt)]{

Returns the pen cap style (Windows unsmoothed, X unsmoothed, all
 smoothing). The default is @scheme['round].

}

@defmethod[(get-color)
           (is-a?/c color%)]{

Returns the pen's color object.

}

@defmethod[(get-join)
           (one-of/c 'round 'bevel 'miter)]{

Returns the pen join style (Windows unsmoothed, X unsmoothed, all
 smoothing). The default is @scheme['round].

}

@defmethod[(get-stipple)
           (or/c (is-a?/c bitmap%) false/c)]{

Gets the current stipple bitmap, or returns @scheme[#f] if no stipple
 bitmap is installed.

}

@defmethod[(get-style)
           (one-of/c 'transparent 'solid 'xor 'hilite 
                     'dot 'long-dash 'short-dash 'dot-dash 
                     'xor-dot 'xor-long-dash 'xor-short-dash 
                     'xor-dot-dash)]{

Returns the pen style. See @scheme[pen%] for information about
possible styles.

}

@defmethod[(get-width)
           (real-in 0 255)]{

Returns the pen width.

}

@defmethod[(set-cap [cap-style (one-of/c 'round 'projecting 'butt)])
           void?]{

Sets the pen cap style (Windows unsmoothed, X unsmoothed, all
 smoothing). See @method[pen% get-cap] for information about cap
 styles.

A pen cannot be modified if it was obtained from a @scheme[pen-list%]
 or while it is selected into a drawing context.

}

@defmethod*[([(set-color [color (is-a?/c color%)])
              void?]
             [(set-color [color-name string?])
              void?]
             [(set-color [red (integer-in 0 255)]
                         [green (integer-in 0 255)]
                         [blue (integer-in 0 255)])
              void?])]{

Sets the pen color.

A pen cannot be modified if it was obtained from a
 @scheme[pen-list%] or while it is selected into a drawing context.

}

@defmethod[(set-join [join-style (one-of/c 'round 'bevel 'miter)])
           void?]{

Sets the pen join style (Windows unsmoothed, X unsmoothed, all
 smoothing). See @method[pen% get-join] for information about join
 styles.

A pen cannot be modified if it was obtained from a
 @scheme[pen-list%] or while it is selected into a drawing context.

}

@defmethod[(set-stipple [stipple (or/c (is-a?/c bitmap%) false/c)])
           void?]{

Sets the pen stipple bitmap, which must be an 8 x 8 monochrome bitmap
 or @scheme[#f], which turns off the stipple bitmap.

A bitmap cannot be used as a stipple if it is selected into a
 @scheme[bitmap-dc%] object; if the given bitmap is selected into a
 @scheme[bitmap-dc%] object, @|MismatchExn|. A pen cannot be modified
 if it was obtained from a @scheme[pen-list%] or while it is selected
 into a drawing context.

A pen's stipple is not used in a smoothing mode, except for a
 @scheme[post-script-dc%] (which is always in smoothed mode).

}

@defmethod[(set-style [style (one-of/c 'transparent 'solid 'xor 'hilite 
                                       'dot 'long-dash 'short-dash 'dot-dash 
                                       'xor-dot 'xor-long-dash 'xor-short-dash 
                                       'xor-dot-dash)])
           void?]{

Sets the pen style. See @scheme[pen%] for information about the
 possible styles.

A pen cannot be modified if it was obtained from a
 @scheme[pen-list%] or while it is selected into a drawing context.

}

@defmethod[(set-width [width (real-in 0 255)])
           void?]{

Sets the pen width.

A pen cannot be modified if it was obtained from a
 @scheme[pen-list%] or while it is selected into a drawing context.

}}

