#lang scribble/doc
@(require "common.ss")

@defclass/title[brush% object% ()]{

A brush is a drawing tool with a color and a style that is used for
 filling in areas, such as the interior of a rectangle or ellipse.  On
 a monochrome display, all non-white brushes are drawn as black.

In addition to its color and style, a brush can have a stipple bitmap.
 This stipple is used only in unsmoothed mode (see @method[dc<%>
 set-smoothing]) or in a PostScript drawing context. Painting with a
 stipple brush is similar to calling @method[dc<%> draw-bitmap] with
 the stipple bitmap in the filled region, except that the bitmap may
 not be scaled in the same way (depending on the platform and device).

A brush's style is one of the following:

@itemize[

 @item{@indexed-scheme['transparent] --- Draws with no effect (on the
       interior of the drawn shape).}

 @item{@indexed-scheme['solid] --- Draws using the brush's color. If a
        monochrome stipple is installed into the brush, black pixels
        from the stipple are transferred to the destination using the
        brush's color, and white pixels from the stipple are not
        transferred.}

 @item{@indexed-scheme['opaque] --- Same as @scheme['solid], except when a
        monochrome stipple is installed for unsmoothed or PostScript
        drawing; in that case, white pixels from the stipple are
        transferred to the destination using the destination's
        background color.}

 @item{@indexed-scheme['xor] --- In a smoothing mode or if a color
       stipple is installed, @scheme['xor] is treated as
       @scheme['solid]. Otherwise, the brush's color or colored
       (monochrome) stipple is xor-ed with existing destination pixel
       values. The @scheme['xor] mapping is unspecified for arbitrary
       color combinations, but the mapping provides two guarantees:

       @itemize[

       @item{Black-and-white drawing to a color or monochrome
       destination always works as expected: black xor white = black,
       white xor black = black, black xor black = white, and white xor
       white = white.}

       @item{Performing the same drawing operation twice in a row with
       @scheme['xor] is equivalent to a no-op.}

       ]}

 @item{@indexed-scheme['hilite] --- Draws with black and a 30% alpha.}

 @item{@indexed-scheme['panel] --- the same as @scheme['solid].}

 @item{The following modes correspond to built-in stipples drawn in
       @scheme['solid] mode:

  @itemize[
  @item{@indexed-scheme['bdiagonal-hatch] --- diagonal lines, top-left to bottom-right}
  @item{@indexed-scheme['crossdiag-hatch] --- crossed diagonal lines}
  @item{@indexed-scheme['fdiagonal-hatch] --- diagonal lines, top-right to bottom-left}
  @item{@indexed-scheme['cross-hatch] --- crossed horizontal and vertical lines}
  @item{@indexed-scheme['horizontal-hatch] --- horizontal lines}
  @item{@indexed-scheme['vertical-hatch] --- vertical lines}
  ]

        However, when a specific stipple is installed into the brush
        for when drawing with a smoothing mode into a non-PostScript
        context, the above modes are ignored and @scheme['solid] is
        used, instead.}

]

@index['("drawing" "outlines")]{To} draw outline shapes (such as
 unfilled boxes and ellipses), use the @scheme['transparent] brush
 style. See @method[brush% set-style] for more information about
 styles.

To avoid creating multiple brushes with the same characteristics, use
 the global @scheme[brush-list%] object
 @indexed-scheme[the-brush-list], or provide a color and style to
 @xmethod[dc<%> set-brush].


@defconstructor[([color (or/c string? (is-a?/c color%)) "black"]
                 [style (one-of/c 'transparent 'solid 'opaque 
                                  'xor 'hilite 'panel 
                                  'bdiagonal-hatch 'crossdiag-hatch 
                                  'fdiagonal-hatch 'cross-hatch
                                  'horizontal-hatch 'vertical-hatch)
                         'solid]
                 [stipple (or/c #f (is-a?/c bitmap%))
                          #f])]{

When no argument are provided, the result is a solid black brush.
 Otherwise, the result is a brush with the given color, style, and stipple. For
 the case that the color is specified using a name, see
 @scheme[color-database<%>] for information about color names; if the
 name is not known, the brush's color is black.

}

@defmethod[(get-color)
           (is-a?/c color%)]{

Returns the brush's color.

}

@defmethod[(get-stipple)
           (or/c (is-a?/c bitmap%) false/c)]{

Gets the stipple bitmap, or @scheme[#f] if the brush has no stipple.

}

@defmethod[(get-style)
           (one-of/c 'transparent 'solid 'opaque 
                     'xor 'hilite 'panel 
                     'bdiagonal-hatch 'crossdiag-hatch
                     'fdiagonal-hatch 'cross-hatch 
                     'horizontal-hatch 'vertical-hatch)]{

Returns the brush's style. See @scheme[brush%] for information about
brush styles.

}

@defmethod*[([(set-color [color (is-a?/c color%)])
              void?]
             [(set-color [color-name string?])
              void?]
             [(set-color [red (integer-in 0 255)]
                         [green (integer-in 0 255)]
                         [blue (integer-in 0 255)])
              void?])]{

Sets the brush's color.  A brush cannot be modified if it was obtained
 from a @scheme[brush-list%] or while it is selected into a drawing
 context.

For the case that the color is specified using a string, see
 @scheme[color-database<%>] for information about color names.

}

@defmethod[(set-stipple [bitmap (or/c (is-a?/c bitmap%) false/c)])
           void?]{

Sets or removes the stipple bitmap, where @scheme[#f] removes the
 stipple. See @scheme[brush%] for information about drawing with
 stipples.

A bitmap cannot be used as a stipple if it is selected into a
 @scheme[bitmap-dc%] object; if the given bitmap is selected into a
 @scheme[bitmap-dc%] object, @|MismatchExn|. A brush cannot be
 modified if it was obtained from a @scheme[brush-list%] or while it
 is selected into a drawing context.

A pen's stipple is not used in a smoothing mode, except for a
 @scheme[post-script-dc%] (which is always in a smoothing mode).

}

@defmethod[(set-style [style (one-of/c 'transparent 'solid 'opaque
                                       'xor 'hilite 'panel 
                                       'bdiagonal-hatch 'crossdiag-hatch
                                       'fdiagonal-hatch 'cross-hatch
                                       'horizontal-hatch 'vertical-hatch)])
           void?]{

Sets the brush's style. See
@scheme[brush%] for information about the possible styles.

A brush cannot be modified if it was obtained from a
 @scheme[brush-list%] or while it is selected into a drawing
 context.

}}

