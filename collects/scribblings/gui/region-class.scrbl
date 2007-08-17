#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[region% object% ()]{

A @scheme[region%] object specifies a portion of a drawing area
 (possibly discontinuous). It is normally used for clipping drawing
 operations.

Each @scheme[region%] object is associated to a particular
 @scheme[dc<%>] object, specified when the region is created. A region
 can only be used with its associated @scheme[dc<%>] object. The origin
 and scale of a drawing context determine the bounding box and drawing
 location of a region at the time that a region is created (or set); a
 region is independent of the current scale and origin when the region
 is used. For an auto-scrolled canvas, the canvas's current scrolling
 applies when the region is used (and it does not affect the region's
 bounding box).

See also @xmethod[dc<%> set-clipping-region] and @xmethod[dc<%>
 get-clipping-region].


@defconstructor[([dc (is-a?/c dc<%>)])]{

Creates an empty region.

}

@defmethod[(get-bounding-box)
           (values real? real? real? real?)]{

Returns a rectangle that encloses the region.  The return values are
 the left, top, width, and height of the rectangle. The bounding box
 is precisely correct for unsmoothed drawing, but it is only
 approximate for smoothed drawing.

}

@defmethod[(get-dc)
           (is-a?/c dc<%>)]{

Returns the region's drawing context.

}

@defmethod[(in-region? [x real?]
                       [y real?])
           boolean?]{

Returns @scheme[#t] if the given point is approximately within the
 region, @scheme[#f] otherwise. The given point is scaled and
 translated according to the region's @scheme[dc<%>]'s current scale
 and translation.

The approximate in-region test represents the true result for
 unsmoothed drawing, but it not necessarily for smoothed drawing.

}

@defmethod[(intersect [rgn (is-a?/c region%)])
           void?]{

Sets the region to the intersection of itself with the given region.

The DC of @scheme[rgn] and @this-obj[] must be the same.

The result is always reliable for unsmoothed and smoothed drawing. For
 smoothed drawing, an intersect corresponds to clipping with this
 region's path, and then clipping with the given region's path.
 Further combining sends to this region correspond to combination with
 the original path before initial clip, and further combination with
 this region as an argument correspond to a combination with the given
 path after the initial clip. Thus, an intersecting region is a poor
 input for @method[region% union], @method[region% subtract], or
 @method[region% xor], but it intersects properly in further calls to
 @method[region% intersect].

}

@defmethod[(is-empty?)
           boolean?]{

Returns @scheme[#t] if the region is approximately empty, @scheme[#f]
 otherwise. An approximately empty region is truly empty for
 unsmoothed drawing, but it may contain points for smoothed drawing.

}

@defmethod[(set-arc [x real?]
                    [y real?]
                    [width (and/c real? (not/c negative?))]
                    [height (and/c real? (not/c negative?))]
                    [start-radians real?]
                    [end-radians real?])
           void?]{

Sets the region to the interior of the specified wedge.

See also @xmethod[dc<%> draw-ellipse], since the region content is
 determined the same way as brush-based filling in a @scheme[dc<%>].

The result is reliable for both unsmoothed and smoothed drawing. For
 smoothed drawing, the region corresponds to a clockwise path with a
 @scheme['winding] fill. The region is also @defterm{atomic} for the
 purposes of region combination.

}

@defmethod[(set-ellipse [x real?]
                        [y real?]
                        [width (and/c real? (not/c negative?))]
                        [height (and/c real? (not/c negative?))])
           void?]{

Sets the region to the interior of the specified ellipse.

See also @xmethod[dc<%> draw-ellipse], since the region content is
 determined the same way as brush-based filling in a @scheme[dc<%>].

The result is reliable for both unsmoothed and smoothed drawing. For
 smoothed drawing, the region corresponds to a clockwise path with a
 @scheme['winding] fill. The region is also @defterm{atomic} for the
 purposes of region combination.

@|DrawSizeNote|

}

@defmethod[(set-path [path (is-a?/c dc-path%)]
                     [xoffset real? 0]
                     [yoffset real? 0]
                     [fill-style (one-of/c 'odd-even 'winding) 'odd-even])
           void?]{

Sets the region to the content of the given path.

See also @xmethod[dc<%> draw-path], since the region content is
 determined the same way as brush-based filling in a @scheme[dc<%>].

The result is reliable for both unsmoothed and smoothed drawing. For
 smoothed drawing, the fill style affects how well the region reliably
 combines with other regions (via @method[region% union],
 @method[region% xor], and @method[region% subtract]). The region is
 also @defterm{atomic} for the purposes of region combination.

}

@defmethod[(set-polygon [points (listof (is-a?/c point%))]
                        [xoffset real? 0]
                        [yoffset real? 0]
                        [fill-style (one-of/c 'odd-even 'winding) 'odd-even])
           void?]{
Sets the region to the interior of the specified polygon.

See also @xmethod[dc<%> draw-polygon], since the region content is
 determined the same way as brush-based filling in a @scheme[dc<%>].

The result is reliable for both unsmoothed and smoothed drawing. For
 smoothed drawing, the fill style affects how well the region reliably
 combines with other regions (via @method[region% union],
 @method[region% xor], and @method[region% subtract]).  The region is
 also @defterm{atomic} for the purposes of region combination.

}

@defmethod[(set-rectangle [x real?]
                          [y real?]
                          [width (and/c real? (not/c negative?))]
                          [height (and/c real? (not/c negative?))])
           void?]{

Sets the region to the interior of the specified rectangle.

See also @xmethod[dc<%> draw-rectangle], since the region content is
 determined the same way as brush-based filling in a @scheme[dc<%>].

The result is reliable for both unsmoothed and smoothed drawing. For
 smoothed drawing, the region corresponds to a clockwise path with a
 @scheme['winding] fill. The region is also @defterm{atomic} for the
 purposes of region combination.

@|DrawSizeNote|

}

@defmethod[(set-rounded-rectangle [x real?]
                                  [y real?]
                                  [width (and/c real? (not/c negative?))]
                                  [height (and/c real? (not/c negative?))]
                                  [radius real? -0.25])
           void?]{

Sets the region to the interior of the specified rounded rectangle.

See also @xmethod[dc<%> draw-rounded-rectangle], since the region
 content is determined the same way as brush-based filling in a
 @scheme[dc<%>].

The result is reliable for both unsmoothed and smoothed drawing. For
 smoothed drawing, the region corresponds to a clockwise path with a
 @scheme['winding] fill. The region is also @defterm{atomic} for the
 purposes of region combination.

@|DrawSizeNote|

}

@defmethod[(subtract [rgn (is-a?/c region%)])
           void?]{

Sets the region to the subtraction of itself minus the given region.
 In other words, a point is removed from the region if it is included
 in the given region. (The given region may contain points that are
 not in the current region; such points are ignored.)

This region's DC and given region's DC must be the same.

The result is always reliable for unsmoothed drawing. For smoothed
 drawing, the result is consistent across platforms and devices, but
 it is never a true subtraction. A subtraction corresponds to
 combining the sub-paths of this region with the reversed sub-paths of
 the given region, then intersecting the result with this region. This
 fails as a true subtraction, because the boundary of loops (with
 either @scheme['odd-even] or @scheme['winding] filling) is ambiguous.

}

@defmethod[(union [rgn (is-a?/c region%)])
           void?]{

Sets the region to the union of itself with the given region.

This region's DC and given region's DC must be the same.

The result is always reliable for unsmoothed drawing. For smoothed
 drawing, a union corresponds to combining the sub-paths of each
 region into one path, using an @scheme['odd-even] fill if either of
 the region uses an @scheme['odd-even] fill (otherwise using a
 @scheme['winding] fill). Consequently, while the result is consistent
 across platforms and devices, it is a true union only for certain
 input regions. For example, it is a true union for non-overlapping
 atomic and union regions. It is also a true union for atomic and
 union regions (potentially overlapping) that are all clockwise and
 use @scheme['winding] fill.

}

@defmethod[(xor [rgn (is-a?/c region%)])
           void?]{

Sets the region to the xoring of itself with the given region (i.e.,
 contains points that are enclosed by exactly one of the two regions).

This region's DC and given region's DC must be the same.

The result is always reliable for unsmoothed drawing. For smoothed
 drawing, the result is consistent across platforms and devices, but
 it is not necessarily a true xoring. An xoring corresponds to
 combining the sub-paths of this region with the reversed sub-paths of
 the given region. The result uses an @scheme['odd-even] fill if either
 of the region uses an @scheme['odd-even] fill (otherwise using a
 @scheme['winding] fill). Consequently, the result is a reliable xoring
 only for certain input regions. For example, it is reliable for
 atomic and xoring regions that all use @scheme['even-odd] fill.

}}

