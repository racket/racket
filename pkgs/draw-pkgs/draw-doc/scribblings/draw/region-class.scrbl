#lang scribble/doc
@(require "common.rkt")

@defclass/title[region% object% ()]{

A @racket[region%] object specifies a portion of a drawing area
 (possibly discontinuous). It is normally used for clipping drawing
 operations.

A @racket[region%] object can be associated to a particular
 @racket[dc<%>] object when the region is created. In that case, the
 region uses the drawing context's current transformation matrix,
 translation, scaling, and rotation, independent of the transformation
 that is in place when the region is installed. Otherwise, the region
 is transformed as usual when it is installed into a
 @racket[dc<%>]. For an auto-scrolled canvas, the canvas's current
 scrolling always applies when the region is used (and it does not
 affect the region's bounding box).

Region combination with operations like @racket[region% union] are
 approximate, and they are implemented by combining paths. Certain
 combinations work only if the paths have a suitable fill mode, which
 can be either @racket['winding], @racket['even-odd], or a
 @deftech{flexible fill} mode. When a region is installed as a device
 context's clipping region, any subpath with a @tech{flexible fill}
 mode uses @racket['even-odd] mode if any other path uses
 @racket['even-odd] mode.

See also @xmethod[dc<%> set-clipping-region] and @xmethod[dc<%>
 get-clipping-region].


@defconstructor[([dc (or/c (is-a?/c dc<%>) #f)])]{

Creates an empty region. If @racket[dc] is a @racket[dc<%>] object,
the @racket[dc<%>]'s current transformation matrix is essentially
recorded in the region.

}

@defmethod[(get-bounding-box)
           (values real? real? real? real?)]{

Returns a rectangle that approximately encloses the region.  The
 return values are the left, top, width, and height of the
 rectangle. If the region has an associated drawing context, the
 bounding box is in the drawing context's current logical coordinates.

}

@defmethod[(get-dc)
           (or/c (is-a?/c dc<%>) #f)]{

Returns the region's drawing context, if it was created for one.

}

@defmethod[(in-region? [x real?]
                       [y real?])
           boolean?]{

Returns @racket[#t] if the given point is approximately within the
 region, @racket[#f] otherwise. If the region has an associated
 drawing context, the given point is effectively transformed according
 to the region's @racket[dc<%>]'s current transformation matrix.

}

@defmethod[(intersect [rgn (is-a?/c region%)])
           void?]{

Sets the region to the intersection of itself with the given region.

The drawing context of @racket[rgn] and @this-obj[] must be the same,
 or they must both be unassociated to any drawing context.

An intersect corresponds to clipping with this region's path, and then
 clipping with the given region's path.  Further combining sends to
 this region correspond to combination with the original path before
 initial clip, and further combination with this region as an argument
 correspond to a combination with the given path after the initial
 clip. Thus, an intersecting region is a poor input for
 @method[region% union], @method[region% subtract], or @method[region%
 xor], but it intersects properly in further calls to @method[region%
 intersect].

}

@defmethod[(is-empty?)
           boolean?]{

Returns @racket[#t] if the region is approximately empty, @racket[#f]
 otherwise, but only if the region is associated with a drawing context.
 If the region is unassociated to any drawing context, the
 @racket[exn:fail:contract] exception is raised.

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
 determined the same way as brush-based filling in a @racket[dc<%>].

The region corresponds to a clockwise path with a @tech{flexible
 fill}. The region is also @tech{atomic} for the purposes of region
 combination.

}

@defmethod[(set-ellipse [x real?]
                        [y real?]
                        [width (and/c real? (not/c negative?))]
                        [height (and/c real? (not/c negative?))])
           void?]{

Sets the region to the interior of the specified ellipse.

See also @xmethod[dc<%> draw-ellipse], since the region content is
 determined the same way as brush-based filling in a @racket[dc<%>].

The region corresponds to a clockwise path with a @tech{flexible
 fill}. The region is also @tech{atomic} for the purposes of region
 combination.

@|DrawSizeNote|

}

@defmethod[(set-path [path (is-a?/c dc-path%)]
                     [xoffset real? 0]
                     [yoffset real? 0]
                     [fill-style (or/c 'odd-even 'winding) 'odd-even])
           void?]{

Sets the region to the content of the given path.

See also @xmethod[dc<%> draw-path], since the region content is
 determined the same way as brush-based filling in a @racket[dc<%>].

The fill style affects how well the region reliably combines with
 other regions (via @method[region% union], @method[region% xor], and
 @method[region% subtract]). The region is also @tech{atomic} for the
 purposes of region combination.

}

@defmethod[(set-polygon [points (or/c (listof (is-a?/c point%))
                                      (listof (cons/c real? real?)))]
                        [xoffset real? 0]
                        [yoffset real? 0]
                        [fill-style (or/c 'odd-even 'winding) 'odd-even])
           void?]{

Sets the region to the interior of the polygon specified by
 @racket[points]. A pair is treated as a point where the @racket[car]
 of the pair is the x-value and the @racket[cdr] is the y-value.

See also @xmethod[dc<%> draw-polygon], since the region content is
 determined the same way as brush-based filling in a @racket[dc<%>].

The fill style affects how well the region reliably combines with
 other regions (via @method[region% union], @method[region% xor], and
 @method[region% subtract]).  The region is also @tech{atomic} for the
 purposes of region combination.

}

@defmethod[(set-rectangle [x real?]
                          [y real?]
                          [width (and/c real? (not/c negative?))]
                          [height (and/c real? (not/c negative?))])
           void?]{

Sets the region to the interior of the specified rectangle.

The region corresponds to a clockwise path with a @tech{flexible
 fill}. The region is also @tech{atomic} for the purposes of region
 combination.

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
 @racket[dc<%>].

The region corresponds to a clockwise path with a @tech{flexible
 fill}. The region is also @tech{atomic} for the purposes of region
 combination.

@|DrawSizeNote|

}

@defmethod[(subtract [rgn (is-a?/c region%)])
           void?]{

Sets the region to the subtraction of itself minus the given region.
 In other words, a point is removed from the region if it is included
 in the given region. (The given region may contain points that are
 not in the current region; such points are ignored.)

This region's drawing context and given region's drawing context must
 be the same, or they must both be unassociated to any drawing
 context.

The result is consistent across platforms and devices, but it is never
 a true subtraction. A subtraction corresponds to combining the
 sub-paths of this region with the reversed sub-paths of the given
 region, then intersecting the result with this region. This fails as
 a true subtraction, because the boundary of loops (with either
 @racket['odd-even] or @racket['winding] filling) is ambiguous.

}

@defmethod[(union [rgn (is-a?/c region%)])
           void?]{

Sets the region to the union of itself with the given region.

This region's drawing context and given region's drawing context must
 be the same, or they must both be unassociated to any drawing
 context.

A union corresponds to combining the sub-paths of each region into one
 path, using an @racket['odd-even] fill if either of the region uses
 an @racket['odd-even] fill (otherwise using a @racket['winding]
 fill), a @racket['winding] fill in either region uses a
 @racket[winding] fill, or the fill remains a @tech{flexible fill}
 if both paths have a @tech{flexible fill}. Consequently, while the
 result is consistent across platforms and devices, it is a true union
 only for certain input regions. For example, it is a true union for
 non-overlapping @deftech{atomic} and union regions. It is also a true
 union for @tech{atomic} and union regions (potentially overlapping)
 that are all clockwise and use @racket['winding] fill or if the fills
 are all @tech{flexible fills}.

}

@defmethod[(xor [rgn (is-a?/c region%)])
           void?]{

Sets the region to the xoring of itself with the given region (i.e.,
 contains points that are enclosed by exactly one of the two regions).

This region's drawing context and given region's drawing context must
 be the same, or they must both be unassociated to any drawing
 context.

The result is consistent across platforms and devices, but it is not
 necessarily a true xoring. An xoring corresponds to combining the
 sub-paths of this region with the reversed sub-paths of the given
 region. The result uses an @racket['odd-even] fill if either of the
 region uses an @racket['odd-even] fill, a @racket['winding] fill in
 either region uses a @racket[winding] fill, or the fill remains a
 @tech{flexible fill} if both paths have a @tech{flexible
 fill}. Consequently, the result is a reliable xoring only for certain
 input regions. For example, it is reliable for @tech{atomic} and
 xoring regions that all use @racket['even-odd] fill.

}}

