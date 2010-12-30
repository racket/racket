#lang scribble/doc
@(require "common.ss")

@defclass/title[brush% object% ()]{

A brush is a drawing tool with a color and a style that is used for
 filling in areas, such as the interior of a rectangle or ellipse.  In
 a monochrome destination, all non-white brushes are drawn as black.

In addition to its color and style, a brush can have a stipple bitmap.
 Painting with a
 stipple brush is similar to calling @method[dc<%> draw-bitmap] with
 the stipple bitmap in the filled region.

A brush's style is one of the following:

@itemize[

 @item{@indexed-scheme['transparent] --- Draws with no effect (on the
       interior of the drawn shape).}

 @item{@indexed-scheme['solid] --- Draws using the brush's color. If a
        monochrome stipple is installed into the brush, black pixels
        from the stipple are transferred to the destination using the
        brush's color, and white pixels from the stipple are not
        transferred.}

 @item{@indexed-scheme['opaque] --- The same as @scheme['solid] for a color
        stipple. For a monochrome stipple, white pixels from 
        the stipple are
        transferred to the destination using the destination's
        background color.}

 @item{@indexed-scheme['xor] --- The same as @racket['solid], accepted 
        only for partial backward compatibility.}

 @item{@indexed-scheme['hilite] --- Draws with black and a @racket[0.3] alpha.}

 @item{@indexed-scheme['panel] --- The same as @scheme['solid], accepted 
        only for partial backward compatibility.}

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

        However, when a specific stipple is installed into the brush,
        the above modes are ignored and @scheme['solid] is
        used, instead.}

]

@index['("drawing" "outlines")]{To} draw outline shapes (such as
 unfilled boxes and ellipses), use the @scheme['transparent] brush
 style.

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

Creates a brush with the given color, style, and stipple. For
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

@defmethod[(set-stipple [bitmap (or/c (is-a?/c bitmap%) #f)])
           void?]{

Sets or removes the stipple bitmap, where @scheme[#f] removes the
 stipple. See @scheme[brush%] for information about drawing with
 stipples.

If @racket[bitmap] is modified while is associated with a brush, the
 effect on the brush is unspecified. A brush cannot be modified if it
 was obtained from a @scheme[brush-list%] or while it is selected into
 a drawing context.

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

