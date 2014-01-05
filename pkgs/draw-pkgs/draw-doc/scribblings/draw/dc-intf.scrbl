#lang scribble/doc
@(require "common.rkt")

@definterface/title[dc<%> ()]{

A @racket[dc<%>] object is a drawing context for drawing graphics and
 text.  It represents output devices in a generic way; e.g., a canvas
 has a drawing context, as does a printer.


@defmethod[(cache-font-metrics-key)
           exact-integer?]{

Returns an integer that, if not @racket[0], corresponds to a
particular kind of device and scaling factor, such that text-extent
information (from @method[dc<%> get-text-extent], @method[dc<%>
get-char-height], etc.) is the same. The key is valid across all
@racket[dc<%>] instances, even among different classes.

A @racket[0] result indicates that the current configuration of
@this-obj[] does not fit into a common category, and so no key is
available for caching text-extent information.}


@defmethod[(clear)
           void?]{

Clears the drawing region (fills it with the current background color,
as determined by @method[dc<%> get-background]). See also @method[dc<%> erase].

}

@defmethod[(copy [x real?]
                 [y real?]
                 [width (and/c real? (not/c negative?))]
                 [height (and/c real? (not/c negative?))]
                 [x2 real?]
                 [y2 real?])
           void?]{

Copies the rectangle defined by @racket[x], @racket[y],
@racket[width], and @racket[height] of the drawing context to the same
drawing context at the position specified by @racket[x2] and
@racket[y2].

The result is undefined if the source and destination rectangles
overlap.}


@defmethod[(draw-arc [x real?]
                     [y real?]
                     [width (and/c real? (not/c negative?))]
                     [height (and/c real? (not/c negative?))]
                     [start-radians real?]
                     [end-radians real?])
           void?]{

Draws a counter-clockwise circular arc, a part of the ellipse
 inscribed in the rectangle specified by @racket[x] (left), @racket[y]
 (top), @racket[width], and @racket[height]. The arc starts at the angle
 specified by @racket[start-radians] (@racket[0] is three o'clock and
 half-pi is twelve o'clock) and continues counter-clockwise to
 @racket[end-radians]. If @racket[start-radians] and @racket[end-radians] are
 the same, a full ellipse is drawn.

The current pen is used for the arc. If the current brush is not
 transparent, it is used to fill the wedge bounded by the arc plus
 lines (not drawn) extending to the center of the inscribed ellipse.
 If both the pen and brush are non-transparent, the wedge is filled
 with the brush before the arc is drawn with the pen. 

The wedge and arc meet so that no space is left between them, but the
 precise overlap between the wedge and arc is platform- and
 size-specific.  Typically, the regions drawn by the brush and pen
 overlap.  In unsmoothed or aligned mode, the path for the outline is
 adjusted by shrinking the bounding ellipse width and height by, after scaling, one
 drawing unit divided by the alignment scale.


@|DrawSizeNote|

}


@defmethod[(draw-bitmap [source (is-a?/c bitmap%)]
                        [dest-x real?]
                        [dest-y real?]
                        [style (or/c 'solid 'opaque 'xor) 'solid]
                        [color (is-a?/c color%) (send the-color-database find-color "black")]
                        [mask (or/c (is-a?/c bitmap%) #f) #f])
           boolean?]{

Displays the @racket[source] bitmap. The @racket[dest-x] and @racket[dest-y] arguments
 are in DC coordinates.

For color bitmaps, the drawing style and color arguments are
 ignored. For monochrome bitmaps, @method[dc<%> draw-bitmap] uses the
 style and color arguments in the same way that a brush uses its style
 and color settings to draw a monochrome stipple (see @racket[brush%]
 for more information).

If a @racket[mask] bitmap is supplied, it must have the same width and height
 as @racket[source], and its @method[bitmap% ok?] must return
 true, otherwise @|MismatchExn|. The @racket[source] bitmap and @racket[mask]
 bitmap can be the same object, but if the drawing context is a
 @racket[bitmap-dc%] object, both bitmaps must be distinct from the
 destination bitmap, otherwise @|MismatchExn|.

The effect of @racket[mask] on drawing depends on the type of the
@racket[mask] bitmap:
@;
@itemlist[

 @item{If the @racket[mask] bitmap is monochrome, drawing occurs in
       the target @racket[dc<%>] only where the mask bitmap contains
       black pixels (independent of @racket[style], which controls how
       the white pixels of a monochrome @racket[source] are handled).}

 @item{If the @racket[mask] bitmap is color with an alpha channel, its
       alpha channel is used as the mask for drawing @racket[source],
       and its color channels are ignored.}

 @item{If the @racket[mask] bitmap is color without an alpha channel,
       the color components of a given pixel are averaged to arrive at
       an inverse alpha value for the pixel. In particular, if the
       @racket[mask] bitmap is grayscale, then the blackness of each
       mask pixel controls the opacity of the drawn pixel (i.e., the
       mask acts as an inverted alpha channel).}

]

The current brush, current pen, and current text for the DC have no
 effect on how the bitmap is drawn, but the bitmap is scaled if the DC
 has a scale, and the DC's alpha setting determines the opacity of the
 drawn pixels (in combination with an alpha channel of @racket[source],
 any given @racket[mask], and the alpha component of @racket[color] 
 when @racket[source] is monochrome).

For @racket[post-script-dc%] and @racket[pdf-dc%] output, opacity from
 an alpha channel in @racket[source], from @racket[mask], or from 
 @racket[color] is rounded to full transparency or opacity.

The result is @racket[#t] if the bitmap is successfully drawn,
 @racket[#f] otherwise (possibly because the bitmap's @method[bitmap%
 ok?] method returns @racket[#f]).

See also @method[dc<%> draw-bitmap-section].

@|DrawSizeNote|

}

@defmethod[(draw-bitmap-section [source (is-a?/c bitmap%)]
                                [dest-x real?]
                                [dest-y real?]
                                [src-x real?]
                                [src-y real?]
                                [src-width (and/c real? (not/c negative?))]
                                [src-height (and/c real? (not/c negative?))]
                                [style (or/c 'solid 'opaque 'xor) 'solid]
                                [color (is-a?/c color%) (send the-color-database find-color "black")]
                                [mask (or/c (is-a?/c bitmap%) #f) #f])
           boolean?]{

Displays part of a bitmap.

The @racket[src-x], @racket[src-y], @racket[src-width], and
 @racket[src-height] arguments specify a rectangle in the source
 bitmap to copy into this drawing context.

See @method[dc<%> draw-bitmap] for information about @racket[dest-x],
 @racket[dest-y], @racket[style], @racket[color], and @racket[mask].

}

@defmethod[(draw-ellipse [x real?]
                         [y real?]
                         [width (and/c real? (not/c negative?))]
                         [height (and/c real? (not/c negative?))])
           void?]{

Draws an ellipse contained in a rectangle with the given top-left
 corner and size. The current pen is used for the outline, and the
 current brush is used for filling the shape. If both the pen and
 brush are non-transparent, the ellipse is filled with the brush
 before the outline is drawn with the pen.

Brush filling and pen outline meet so that no space is left between
 them, but the precise overlap between the filling and outline is
 platform- and size-specific.  Thus, the regions drawn by the brush
 and pen may partially overlap. In unsmoothed or aligned mode, the
 path for the outline is adjusted by, after scaling, shrinking the
 ellipse width and height by one drawing unit divided by the
 @tech{alignment scale}.

@|DrawSizeNote|

}

@defmethod[(draw-line [x1 real?]
                      [y1 real?]
                      [x2 real?]
                      [y2 real?])
           void?]{

Draws a line from one point to another.  The current pen is used for
 drawing the line.

In unsmoothed mode, the points correspond to pixels, and the line
 covers both the start and end points. For a pen whose scaled width is
 larger than @racket[1], the line is drawn centered over the start and
 end points.

See also @method[dc<%> set-smoothing] for information on the
@racket['aligned] smoothing mode.

@|DrawSizeNote|

}

@defmethod[(draw-lines [points (or/c (listof (is-a?/c point%))
                                     (listof (cons/c real? real?)))]
                       [xoffset real? 0]
                       [yoffset real? 0])
           void?]{

Draws lines using a list @racket[points] of points, adding @racket[xoffset]
 and @racket[yoffset] to each point. A pair is treated as a point where the
 @racket[car] of the pair is the x-value and the @racket[cdr] is the y-value.
 The current pen is used for
 drawing the lines.

See also @method[dc<%> set-smoothing] for information on the
 @racket['aligned] smoothing mode.

@|DrawSizeNote|

}

@defmethod[(draw-path [path (is-a?/c dc-path%)]
                      [xoffset real? 0]
                      [yoffset real? 0]
                      [fill-style (or/c 'odd-even 'winding) 'odd-even])
           void?]{

Draws the sub-paths of the given @racket[dc-path%] object, adding
 @racket[xoffset] and @racket[yoffset] to each point. (See
 @racket[dc-path%] for general information on paths and sub-paths.)
 The current pen is used for drawing the path as a line, and the
 current brush is used for filling the area bounded by the path.

If both the pen and brush are non-transparent, the path is filled with
 the brush before the outline is drawn with the pen. The filling and
 outline meet so that no space is left between them, but the precise
 overlap between the filling and outline is platform- and
 size-specific.  Thus, the regions drawn by the brush and pen may
 overlap. More generally, the pen is centered over the path, rounding
 left and down in unsmoothed mode.

The @racket[fill-style] argument specifies the fill rule:
 @racket['odd-even] or @racket['winding]. In @racket['odd-even] mode, a
 point is considered enclosed within the path if it is enclosed by an
 odd number of sub-path loops. In @racket['winding] mode, a point is
 considered enclosed within the path if it is enclosed by more or less
 clockwise sub-path loops than counter-clockwise sub-path loops.

See also @method[dc<%> set-smoothing] for information on the
 @racket['aligned] smoothing mode.

@|DrawSizeNote|

}

@defmethod[(draw-point [x real?]
                       [y real?])
           void?]{

Plots a single point using the current pen.

@|DrawSizeNote|

}

@defmethod[(draw-polygon [points (or/c (listof (is-a?/c point%))
                                       (listof (cons/c real? real?)))]
                         [xoffset real? 0]
                         [yoffset real? 0]
                         [fill-style (or/c 'odd-even 'winding) 'odd-even])
           void?]{

Draw a filled polygon using a list @racket[points] of points, adding
 @racket[xoffset] and @racket[yoffset] to each point. 
 A pair is treated as a point where the
 @racket[car] of the pair is the x-value and the @racket[cdr] is the y-value.
 The polygon is
 automatically closed, so the first and last point can be
 different. The current pen is used for drawing the outline, and the
 current brush for filling the shape.

If both the pen and brush are non-transparent, the polygon is filled
 with the brush before the outline is drawn with the pen. The filling
 and outline meet so that no space is left between them, but the
 precise overlap between the filling and outline is platform- and
 shape-specific.  Thus, the regions drawn by the brush and pen may
 overlap. More generally, the pen is centered over the polygon lines,
 rounding left and down in unsmoothed mode.

The @racket[fill-style] argument specifies the fill rule:
 @racket['odd-even] or @racket['winding]. In @racket['odd-even] mode, a
 point is considered enclosed within the polygon if it is enclosed by
 an odd number of loops. In @racket['winding] mode, a point is
 considered enclosed within the polygon if it is enclosed by more or
 less clockwise loops than counter-clockwise loops.

See also @method[dc<%> set-smoothing] for information on the
 @racket['aligned] smoothing mode.

@|DrawSizeNote|

}


@defmethod[(draw-rectangle [x real?]
                           [y real?]
                           [width (and/c real? (not/c negative?))]
                           [height (and/c real? (not/c negative?))])
           void?]{

Draws a rectangle with the given top-left corner and size.  The
 current pen is used for the outline and the current brush for filling
 the shape. If both the pen and brush are non-transparent, the
 rectangle is filled with the brush before the outline is drawn with
 the pen.

In unsmoothed or aligned mode, when the pen is size 0 or 1, the
 filling precisely overlaps the entire outline. More generally, in
 unsmoothed or aligned mode, the path for the outline is adjusted by
 shrinking the rectangle width and height by, after scaling, one
 drawing unit divided by the @tech{alignment scale}.

See also @method[dc<%> set-smoothing] for information on the
@racket['aligned] smoothing mode.

@|DrawSizeNote|

}


@defmethod[(draw-rounded-rectangle [x real?]
                                   [y real?]
                                   [width (and/c real? (not/c negative?))]
                                   [height (and/c real? (not/c negative?))]
                                   [radius real? -0.25])
           void?]{

Draws a rectangle with the given top-left corner, and with the given
 size. The corners are quarter-circles using the given radius.  The
 current pen is used for the outline and the current brush for filling
 the shape. If both the pen and brush are non-transparent, the rectangle is filled
 with the brush before the outline is drawn with the pen.

If @racket[radius] is positive, the value is used as the radius of the
 rounded corner. If @racket[radius] is negative, the absolute value is
 used as the @italic{proportion} of the smallest dimension of the
 rectangle.

If @racket[radius] is less than @racket[-0.5] or more than half of
 @racket[width] or @racket[height], @|MismatchExn|.

Brush filling and pen outline meet so that no space is left between
 them, but the precise overlap between the filling and outline is
 platform- and size-specific.  Thus, the regions drawn by the brush
 and pen may partially overlap. In unsmoothed or aligned mode, the
 path for the outline is adjusted by, after scaling, shrinking the
 rectangle width and height by one drawing unit divided by the
 @tech{alignment scale}.

See also @method[dc<%> set-smoothing] for information on the
@racket['aligned] smoothing mode.

@|DrawSizeNote|

}

@defmethod[(draw-spline [x1 real?]
                        [y1 real?]
                        [x2 real?]
                        [y2 real?]
                        [x3 real?]
                        [y3 real?])
           void?]{

@index['("drawing curves")]{Draws} a spline from (@racket[x1],
 @racket[y1]) to (@racket[x3], @racket[y3]) using (@racket[x2],
 @racket[y2]) as the control point.

See also @method[dc<%> set-smoothing] for information on the
 @racket['aligned] smoothing mode. See also @racket[dc-path%] and
 @method[dc<%> draw-path] for drawing more complex curves.

@|DrawSizeNote|

}

@defmethod[(draw-text [text string?]
                      [x real?]
                      [y real?]
                      [combine? any/c #f]
                      [offset exact-nonnegative-integer? 0]
                      [angle real? 0])
           void?]{

Draws a text string at a specified point, using the current text font,
 and the current text foreground and background colors. For unrotated
 text, the specified point is used as the starting top-left point for
 drawing characters (e.g, if ``W'' is drawn, the point is roughly the
 location of the top-left pixel in the ``W''). Rotated text is rotated
 around this point.

The @racket[text] string is drawn starting from the @racket[offset]
 character, and continuing until the end of @racket[text] or the first
 null character.

If @racket[combine?] is @racket[#t], then @racket[text] may be
 measured with adjacent characters combined to ligature glyphs, with
 Unicode combining characters as a single glyph, with kerning, with
 right-to-left rendering of characters, etc. If @racket[combine?] is
 @racket[#f], then the result is the same as if each character is
 measured separately, and Unicode control characters are ignored.

The string is rotated by @racket[angle] radians counter-clockwise. If
 @racket[angle] is not zero, then the text is always drawn in
 transparent mode (see @method[dc<%> set-text-mode]).

The current brush and current pen settings for the DC have no effect
 on how the text is drawn.

See @method[dc<%> get-text-extent] for information on the size of the
 drawn text.

See also @method[dc<%> set-text-foreground], @method[dc<%>
 set-text-background], and @method[dc<%> set-text-mode].

@|DrawSizeNote|

}

@defmethod[(end-doc)
           void?]{

Ends a document, relevant only when drawing to a printer, PostScript,
 PDF, or SVG device.

For relevant devices, an exception is raised if
@method[dc<%> end-doc] is called when the document is not started with
@method[dc<%> start-doc], when a page is currently started by
@method[dc<%> start-page] and not ended with @method[dc<%> end-page],
or when the document has been ended already.

}


@defmethod[(end-page)
           void?]{

Ends a single page, relevant only when drawing to a printer,
 PostScript, PDF, or SVG device.

For relevant devices, an exception is raised if
@method[dc<%> end-page] is called when a page is not currently started by
@method[dc<%> start-page].}


@defmethod[(erase)
           void?]{

For a drawing context that has an alpha channel, @method[dc<%> erase]
sets all alphas to zero. Similarly, for a transparent canvas,
@method[dc<%> erase] erases all drawing to allow the background window
to show through. For other drawing contexts that have no alpha channel
or transparency, @method[dc<%> erase] fills the drawing context with
white.}


@defmethod[(flush) void?]{

Calls the @xmethod[canvas<%> flush] method for
@racket[canvas<%>] output, and has no effect for other kinds of
drawing contexts.}



@defmethod[(get-alpha)
           (real-in 0 1)]{

Gets the current opacity for drawing; see
@method[dc<%> set-alpha].

}

@defmethod[(get-background)
           (is-a?/c color%)]{

Gets the color used for painting the background. See also
@method[dc<%> set-background].

}

@defmethod[(get-brush)
           (is-a?/c brush%)]{

Gets the current brush. See also @method[dc<%> set-brush].

}

@defmethod[(get-char-height)
           (and/c real? (not/c negative?))]{

Gets the height of a character using the current font.

Unlike most methods, this method can be called for a
 @racket[bitmap-dc%] object without a bitmap installed.

}

@defmethod[(get-char-width)
           (and/c real? (not/c negative?))]{

Gets the average width of a character using the current font.

Unlike most methods, this method can be called for a
 @racket[bitmap-dc%] object without a bitmap installed.

}

@defmethod[(get-clipping-region)
           (or/c (is-a?/c region%) #f)]{

Gets the current clipping region, returning @racket[#f] if the drawing
 context is not clipped (i.e., the clipping region is the entire
 drawing region).

}


@defmethod[(get-device-scale)
           (values (and/c real? (not/c negative?))
                   (and/c real? (not/c negative?)))]{

Gets an ``external'' scaling factor for drawing coordinates to the
target device. For most DCs, the result is @racket[1.0] and
@racket[1.0].

A @racket[post-script-dc%] or @racket[pdf-dc%] object returns scaling
factors determined via @xmethod[ps-setup% get-scaling] at the time
that the DC was created. A @racket[printer-dc%] may also have a
user-configured scaling factor.}


@defmethod[(get-font)
           (is-a?/c font%)]{

Gets the current font. See also @method[dc<%> set-font].

}

@defmethod[(get-gl-context)
           (or/c (is-a?/c gl-context<%>) #f)]{

Returns a @racket[gl-context<%>] object for this drawing context
 if it supports OpenGL, @racket[#f] otherwise.

See @racket[gl-context<%>] for more information.

}

@defmethod[(get-initial-matrix)
           (vector/c real? real? real? real? real? real?)]{

Returns a transformation matrix that converts logical coordinates to
 device coordinates. The matrix applies before additional origin
 offset, scaling, and rotation.

The vector content corresponds to a transformation matrix in the
following order:

@itemlist[

 @item{@racket[_xx]: a scale from the logical @racket[_x] to the device @racket[_x]}

 @item{@racket[_xy]: a scale from the logical @racket[_x] added to the device @racket[_y]}

 @item{@racket[_yx]: a scale from the logical @racket[_y] added to the device @racket[_x]}

 @item{@racket[_yy]: a scale from the logical @racket[_y] to the device @racket[_y]}

 @item{@racket[_x0]: an additional amount added to the device @racket[_x]}

 @item{@racket[_y0]: an additional amount added to the device @racket[_y]}

]

See also @method[dc<%> set-initial-matrix] and @method[dc<%> get-transformation].

}


@defmethod[(get-origin)
           (values real? real?)]{

Returns the device origin, i.e., the location in device coordinates of
 @math{(0,0)} in logical coordinates. The origin offset applies after
 the initial transformation matrix, but before scaling and rotation.

See also @method[dc<%> set-origin] and @method[dc<%> get-transformation].

}


@defmethod[(get-pen)
           (is-a?/c pen%)]{

Gets the current pen. See also @method[dc<%> set-pen].

}
                     
                          
@defmethod[(get-path-bounding-box [path (is-a?/c dc-path%)] 
                                  [type (or/c 'path 'stroke 'fill)])
           (values real? real? real? real?)]{
Returns a rectangle that encloses the path’s points. 
The return values are the left, top, width, and, height of the rectangle.
The numbers are in logical coordinates.

For the type @racket['stroke] the rectangle covers the area that would be affected (``inked'')
when drawn with the current pen by draw-path in the drawing context (with a transparent brush). 
If the pen width is zero, then an empty rectangle will be returned. The size and clipping of the 
drawing context is ignored.

For the type @racket['fill] the rectangle covers the area that would be affected (``inked'')
by draw-path in the drawing context (with a non-transparent pen and brush). If the line width 
is zero, then an empty rectangle will be returned. The size and clipping of the drawing
context are ignored.

For the type @racket['path] the rectangle covers the path, but the pen and brush are ignored.
The size and clipping of the drawing context are also ignored.
More precisely: The result is defined as the limit of the bounding boxes returned
by the @racket['stroke] type for line widths approaching 0 with a round pen cap. The ``limit
process'' stops when an empty rectangle is returned. This implies that zero-area segments 
contributes to the rectangle.

For all types if the path is empty, then an empty rectangle @racket[(values 0 0 0 0)] 
will be returned.
}
 

@defmethod[(get-rotation) real?]{

Returns the rotation of logical coordinates in radians to device
coordinates. Rotation applies after the initial transformation matrix,
origin offset, and scaling.

See also @method[dc<%> set-rotation] and @method[dc<%> get-transformation].

}

@defmethod[(get-scale)
           (values real? real?)]{

Returns the scaling factor that maps logical coordinates to device
coordinates. Scaling applies after the initial transformation matrix
and origin offset, but before rotation.

See also @method[dc<%> set-scale] and @method[dc<%> get-transformation].

}

@defmethod[(get-size)
           (values (and/c real? (not/c negative?))
                   (and/c real? (not/c negative?)))]{

Gets the size of the destination drawing area. For a @racket[dc<%>]
 object obtained from a @racket[canvas<%>], this is the (virtual
 client) size of the destination window; for a @racket[bitmap-dc%]
 object, this is the size of the selected bitmap (or 0 if no bitmap is
 selected); for a @racket[post-script-dc%] or @racket[printer-dc%]
 drawing context, this gets the horizontal and vertical size of the
 drawing area.

}

@defmethod[(get-smoothing)
           (or/c 'unsmoothed 'smoothed 'aligned)]{

Returns the current smoothing mode. See @method[dc<%> set-smoothing].

}

@defmethod[(get-text-background)
           (is-a?/c color%)]{

Gets the current text background color. See also @method[dc<%>
set-text-background].

}

@defmethod[(get-text-extent [string string?]
                            [font (or/c (is-a?/c font%) #f) #f]
                            [combine? any/c #f]
                            [offset exact-nonnegative-integer? 0])
           (values (and/c real? (not/c negative?)) 
                   (and/c real? (not/c negative?))
                   (and/c real? (not/c negative?)) 
                   (and/c real? (not/c negative?)))]{


Returns the size of @racket[str] at it would be drawn in the drawing
 context, starting from the @racket[offset] character of @racket[str],
 and continuing until the end of @racket[str] or the first null
 character.  The @racket[font] argument specifies the font to use in
 measuring the text; if it is @racket[#f], the current font of the
 drawing area is used. (See also @method[dc<%> set-font].)

The result is four real numbers:

@itemize[

 @item{the total width of the text (depends on both the font and the
 text);}

 @item{the total height of the font (depends only on the font);}

 @item{the distance from the baseline of the font to the bottom of the
 descender (included in the height, depends only on the font); and}

 @item{extra vertical space added to the font by the font designer
 (included in the height, and often zero; depends only on the font).}

]

The returned width and height define a rectangle is that guaranteed to
 contain the text string when it is drawn, but the fit is not
 necessarily tight. Some undefined number of pixels on the left,
 right, top, and bottom of the drawn string may be ``whitespace,''
 depending on the whims of the font designer and the platform-specific
 font-scaling mechanism.

If @racket[combine?] is @racket[#t], then @racket[text] may be drawn
 with adjacent characters combined to ligature glyphs, with Unicode
 combining characters as a single glyph, with kerning, with
 right-to-left ordering of characters, etc. If @racket[combine?] is
 @racket[#f], then the result is the same as if each character is
 drawn separately, and Unicode control characters are ignored.

Unlike most methods, this method can be called for a
 @racket[bitmap-dc%] object without a bitmap installed.

}


@defmethod[(get-text-foreground)
           (is-a?/c color%)]{

Gets the current text foreground color. See also @method[dc<%>
set-text-foreground].

}


@defmethod[(get-text-mode)
           (or/c 'solid 'transparent)]{
Reports how text is drawn; see
@method[dc<%> set-text-mode].}


@defmethod[(get-transformation)
           (vector/c (vector/c real? real? real? real? real? real?)
                     real? real? real? real? real?)]{

Returns the current transformation setting of the drawing context in a
form that is suitable for restoration via @method[dc<%>
set-transformation].

The vector content is as follows:

@itemlist[

 @item{the initial transformation matrix; see @method[dc<%>
       get-initial-matrix];}

 @item{the X and Y origin; see @method[dc<%> get-origin];}

 @item{the X and Y scale; see @method[dc<%> get-origin];}

 @item{a rotation; see @method[dc<%> get-rotation].}

]}


@defmethod[(glyph-exists? [c char?])
           boolean?]{

Returns @racket[#t] if the given character has a corresponding glyph
 for this drawing context, @racket[#f] otherwise.

Due to automatic font substitution when drawing or measuring text, the
 result of this method does not depend on the given font, which merely
 provides a hint for the glyph search. If the font is @racket[#f], the
 drawing context's current font is used. The result depends on the
 type of the drawing context, but the result for @racket[canvas%]
 @racket[dc<%>] instances and @racket[bitmap-dc%] instances is always
 the same for a given platform and a given set of installed fonts.

See also @method[font% screen-glyph-exists?] .

}

@defmethod[(ok?)
           boolean?]{

Returns @racket[#t] if the drawing context is usable.

}


@defmethod[(resume-flush) void?]{

Calls the @xmethod[canvas<%> resume-flush] method for
@racket[canvas<%>] output, and has no effect for other kinds of
drawing contexts.}


@defmethod[(rotate [angle real?]) void?]{

Adds a rotation of @racket[angle] radians to the drawing context's
current transformation.

Afterward, the drawing context's transformation is represented in the
initial transformation matrix, and the separate origin, scale, and
rotation settings have their identity values.

}

@defmethod[(scale [x-scale real?]
                  [y-scale real?])
           void?]{

Adds a scaling of @racket[x-scale] in the X-direction and
@racket[y-scale] in the Y-direction to the drawing context's current
transformation.

Afterward, the drawing context's transformation is represented in the
initial transformation matrix, and the separate origin, scale, and
rotation settings have their identity values.

}

@defmethod[(set-alignment-scale [scale (>/c 0.0)])
           void?]{

Sets the drawing context's @deftech{alignment scale}, which determines
 how drawing coordinates and pen widths are adjusted for unsmoothed or
 aligned drawing (see @method[dc<%> set-smoothing]).

The default @tech{alignment scale} is @racket[1.0], which means that
 drawing coorinates and pen sizes are aligned to integer values.

An @tech{alignment scale} of @racket[2.0] aligns drawing coordinates
 to half-integer values. A value of @racket[2.0] could be suitable for
 a @racket[bitmap-dc%] whose destination is a bitmap with a
 @tech{backing scale} of @racket[2.0], since half-integer values
 coorespond to pixel boundaries. Even when a destinate context has a
 backing scale of @racket[2.0], however, an alignment scale of
 @racket[1.0] may be desirable to maintain consistency with drawing
 contexts that have a backing scale and alignment scale of
 @racket[1.0].

@history[#:added "1.1"]}


@defmethod[(set-alpha [opacity (real-in 0 1)])
           void?]{

Determines the opacity of drawing. A value of @racket[0.0] corresponds
to completely transparent (i.e., invisible) drawing, and @racket[1.0]
corresponds to completely opaque drawing. For intermediate values,
drawing is blended with the existing content of the drawing context.
A color (e.g. for a brush) also has an alpha value; it is combined
with the drawing context's alpha by multiplying.}


@defmethod*[([(set-background [color (is-a?/c color%)])
              void?]
             [(set-background [color-name string?])
              void?])]{

Sets the background color for drawing in this object (e.g., using
@method[dc<%> clear] or using a stippled @racket[brush%] with the mode
@racket['opaque]). For monochrome drawing, all non-black colors are
treated as white.

}

@defmethod*[([(set-brush [brush (is-a?/c brush%)])
              void?]
             [(set-brush [color (is-a?/c color%)]
                         [style (or/c 'transparent 'solid 'opaque 
                                      'xor 'hilite 'panel 
                                      'bdiagonal-hatch 'crossdiag-hatch
                                      'fdiagonal-hatch 'cross-hatch 
                                      'horizontal-hatch 'vertical-hatch)])
              void?]
             [(set-brush [color-name string?]
                         [style (or/c 'transparent 'solid 'opaque 
                                      'xor 'hilite 'panel
                                      'bdiagonal-hatch 'crossdiag-hatch
                                      'fdiagonal-hatch 'cross-hatch
                                      'horizontal-hatch 'vertical-hatch)])
              void?])]{

Sets the current brush for drawing in this object.  While a brush is
 selected into a drawing context, it cannot be modified. When a color
 and style are given, the arguments are as for @xmethod[brush-list%
 find-or-create-brush].

}


@defmethod[(set-clipping-rect [x real?]
                              [y real?]
                              [width (and/c real? (not/c negative?))]
                              [height (and/c real? (not/c negative?))])
           void?]{

Sets the clipping region to a rectangular region.

See also @method[dc<%> set-clipping-region] and @method[dc<%>
get-clipping-region].

@|DrawSizeNote|

}

@defmethod[(set-clipping-region [rgn (or/c (is-a?/c region%) #f)])
           void?]{

Sets the clipping region for the drawing area, turning off all
 clipping within the drawing region if @racket[#f] is provided.

The clipping region must be reset after changing a @racket[dc<%>]
 object's origin or scale (unless it is @racket[#f]); see
 @racket[region%] for more information.

See also @method[dc<%> set-clipping-rect] and @method[dc<%>
 get-clipping-region].

}

@defmethod[(set-font [font (is-a?/c font%)])
           void?]{

Sets the current font for drawing text in this object.

}

@defmethod[(set-initial-matrix [m (vector/c real? real? real? real? real? real?)])
           void?]{

Set a transformation matrix that converts logical coordinates to
 device coordinates. The matrix applies before additional origin
 offset, scaling, and rotation.

See @method[dc<%> get-initial-matrix] for information on the matrix as
 represented by a vector @racket[m].

See also @method[dc<%> transform], which adds a transformation to the
 current transformation, instead of changing the transformation
 composition in the middle.

@|DrawSizeNote|

}

@defmethod[(set-origin [x real?]
                       [y real?])
           void?]{

Sets the device origin, i.e., the location in device coordinates of
 @math{(0,0)} in logical coordinates. The origin offset applies after
 the initial transformation matrix, but before scaling and rotation.

See also @method[dc<%> translate], which adds a translation to the
 current transformation, instead of changing the transformation
 composition in the middle.

@|DrawSizeNote|

}

@defmethod*[([(set-pen [pen (is-a?/c pen%)])
              void?]
             [(set-pen [color (is-a?/c color%)]
                       [width (real-in 0 255)]
                       [style (or/c 'transparent 'solid 'xor 'hilite
                                    'dot 'long-dash 'short-dash 'dot-dash 
                                    'xor-dot 'xor-long-dash 'xor-short-dash 
                                    'xor-dot-dash)])
              void?]
             [(set-pen [color-name string?]
                       [width (real-in 0 255)]
                       [style (or/c 'transparent 'solid 'xor 'hilite 
                                    'dot 'long-dash 'short-dash 'dot-dash 
                                    'xor-dot 'xor-long-dash 'xor-short-dash 
                                    'xor-dot-dash)])
              void?])]{

Sets the current pen for this object. When a color, width, and style
 are given, the arguments are as for @xmethod[pen-list%
 find-or-create-pen].

The current pen does not affect text drawing; see also @method[dc<%>
 set-text-foreground].

While a pen is selected into a drawing context, it cannot be modified.

}

@defmethod[(set-rotation [angle real?]) void?]{

Set the rotation of logical coordinates in radians to device
coordinates. Rotation applies after the initial transformation matrix,
origin offset, and scaling.

See also @method[dc<%> rotate], which adds a rotation to the current
 transformation, instead of changing the transformation composition.

@|DrawSizeNote|

}

@defmethod[(set-scale [x-scale real?]
                      [y-scale real?])
           void?]{

Sets a scaling factor that maps logical coordinates to device
 coordinates.  Scaling applies after the initial transformation matrix
 and origin offset, but before rotation. Negative scaling factors have
 the effect of flipping.

See also @method[dc<%> scale], which adds a scale to the current
 transformation, instead of changing the transformation composition in
 the middle.

@|DrawSizeNote|

}

@defmethod[(set-smoothing [mode (or/c 'unsmoothed 'smoothed 'aligned)])
           void?]{

Enables or disables anti-aliased smoothing for drawing. (Text
 smoothing is not affected by this method, and is instead controlled
 through the @racket[font%] object.)

The smoothing mode is either @racket['unsmoothed], @racket['smoothed],
 or @racket['aligned]. Both @racket['aligned] and @racket['smoothed]
 are smoothing modes that enable anti-aliasing, while both
 @racket['unsmoothed] and @racket['aligned] adjust drawing coordinates
 to match pixel boundaries. For most applications that draw to the
 screen or bitmaps, @racket['aligned] mode is the best choice.

Conceptually, integer drawing coordinates correspond to the boundary
 between pixels, and pen-based drawing is centered over a given line
 or curve. Thus, drawing with pen width @racket[1] from @math{(0, 10)}
 to @math{(10, 10)} in @racket['smoothed] mode draws a 2-pixel wide
 line with @math{50%} opacity.

In @racket['unsmoothed] and @racket['aligned] modes, drawing
 coordinates are truncated based on the @tech{alignment scale} of the
 drawing context. Specifically, when the alignment scale is 1.0,
 drawing coordinates are truncated to integer coordinates. More
 generally, drawing coordinates are shifted toward zero so that the
 result multipled by the @tech{alignment scale} is integral. For line
 drawing, coordinates are further shifted based on the pen width and
 the alignment scale, where the shift corrsponds to half of the pen
 width (reduced to a value such that its multiplication times the
 alignment scale times two produces an integer). In addition, for pen
 drawing through @method[dc<%> draw-rectangle], @method[dc<%>
 draw-ellipse], @method[dc<%> draw-rounded-rectangle], and
 @method[dc<%> draw-arc], the given width and height are each
 decreased by @math{1.0} divided by the @tech{alignment scale}.}


@defmethod*[([(set-text-background [color (is-a?/c color%)])
              void?]
             [(set-text-background [color-name string?])
              void?])]{

Sets the current text background color for this object. The text
 background color is painted behind text that is drawn with
 @method[dc<%> draw-text], but only for the @racket['solid] text mode
 (see @method[dc<%> set-text-mode]).

For monochrome drawing, all non-white colors are treated as black.

}

@defmethod*[([(set-text-foreground [color (is-a?/c color%)])
              void?]
             [(set-text-foreground [color-name string?])
              void?])]{

Sets the current text foreground color for this object, used for
 drawing text with
@method[dc<%> draw-text].

For monochrome drawing, all non-black colors are treated as
 white.

}

@defmethod[(set-text-mode [mode (or/c 'solid 'transparent)])
           void?]{

Determines how text is drawn:

@itemize[

 @item{@racket['solid] --- Before text is drawn, the destination area
       is filled with the text background color (see @method[dc<%>
       set-text-background]).}

 @item{@racket['transparent] --- Text is drawn directly over any
       existing image in the destination, as if overlaying text
       written on transparent film.}

]

}


@defmethod[(set-transformation
            [t (vector/c (vector/c real? real? real? real? real? real?)
                         real? real? real? real? real?)])
           void?]{

Sets the draw context's transformation. See @method[dc<%>
get-transformation] for information about @racket[t].}


@defmethod[(start-doc [message string?])
           void?]{

Starts a document, relevant only when drawing to a printer,
 PostScript, PDF, or SVG device.  For some
 platforms, the @racket[message] string is displayed in a dialog until
 @method[dc<%> end-doc] is called.

For relevant devices, an exception is raised if
 @method[dc<%> start-doc] has been called already (even if @method[dc<%>
 end-doc] has been called as well). Furthermore, drawing methods raise
 an exception if not called while a page is active as determined by
 @method[dc<%> start-doc] and @method[dc<%> start-page].

}

@defmethod[(start-page)
           void?]{

Starts a page, relevant only when drawing to a printer, PostScript,
 SVG, or PDF device.

Relevant devices, an exception is raised if
 @method[dc<%> start-page] is called when a page is already started, or when
 @method[dc<%> start-doc] has not been called, or when @method[dc<%>
 end-doc] has been called already. In addition, in the case of
 PostScript output, Encapsulated PostScript (EPS) cannot contain
 multiple pages, so calling @racket[start-page] a second time for a
 @racket[post-script-dc%] instance raises an exception; to create
 PostScript output with multiple pages, supply @racket[#f] as the
 @racket[as-eps] initialization argument for @racket[post-script-dc%].

}


@defmethod[(suspend-flush) void?]{

Calls the @xmethod[canvas<%> suspend-flush] method for
@racket[canvas<%>] output, and has no effect for other kinds of
drawing contexts.}


@defmethod[(transform [m (vector/c real? real? real? real? real? real?)])
           void?]{

Adds a transformation by @racket[m] to the drawing context's current
transformation. 

See @method[dc<%> get-initial-matrix] for information on the matrix as
 represented by a vector @racket[m].

Afterward, the drawing context's transformation is represented in the
initial transformation matrix, and the separate origin, scale, and
rotation settings have their identity values.

}

@defmethod[(translate [dx real?]
                      [dy real?])
           void?]{

Adds a translation of @racket[dx] in the X-direction and @racket[dy] in
the Y-direction to the drawing context's current transformation.

Afterward, the drawing context's transformation is represented in the
initial transformation matrix, and the separate origin, scale, and
rotation settings have their identity values.

}


@defmethod[(try-color [try (is-a?/c color%)]
                      [result (is-a?/c color%)])
           void?]{

Determines the actual color used for drawing requests with the given
 color. The @racket[result] color is set to the RGB values that are
 actually produced for this drawing context to draw the color
 @racket[try].

}}
