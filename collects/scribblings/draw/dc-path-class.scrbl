#lang scribble/doc
@(require "common.ss")

@defclass/title[dc-path% object% ()]{

A path is a set of figures defined by curves. A path can be used with
the @method[dc<%> draw-path] method of a @scheme[dc<%>] object to draw
the path's curves as lines, fill the region bounded by the path's
curves, or both. A path can also be used with the @method[region%
set-path] method of a @scheme[region%] object to generate a region
bounded by the path's curves.

A path consists of zero or more @deftech{closed sub-paths}, and
 possibly one @deftech{open sub-path}. Some @scheme[dc-path%] methods
 extend the open sub-path, some @scheme[dc-path%] methods close the
 open sub-path, and some @scheme[dc-path%] methods add closed
 sub-paths. This approach to drawing formulation is inherited from
 PostScript @cite["Adobe99"].

When a path is drawn as a line, a closed sub-path is drawn as a closed
 figure, analogous to a polygon. An open sub-path is drawn with
 disjoint start and end points, analogous lines drawn with
 @xmethod[dc<%> draw-lines].

When a path is filled or used as a region, the open sub-path (if any)
 is treated as if it were closed. The content of a path is determined
 either through the @scheme['even-odd] rule or the @scheme['winding]
 rule, as selected at the time when the path is filled or used to
 generate a region.

A path is not connected to any particular @scheme[dc<%>] object, so
 setting a @scheme[dc<%>] origin or scale does not affect path
 operations. Instead, a @scheme[dc<%>]'s origin and scale apply at the
 time that the path is drawn or used to set a region.

@defconstructor[()]{

Creates a new path that contains no sub-paths (and no @tech{open
sub-path}).

}


@defmethod[(append [path (is-a?/c dc-path%)])
           void?]{

Adds the sub-paths of @scheme[path] to @this-obj[]. @tech{Closed
 sub-paths} of @scheme[path] are added as @tech{closed sub-paths} to
 @this-obj[]. If both paths have an @tech{open sub-path}, then this
 path's sub-path is extended by the given path's @tech{open sub-path},
 adding a line from this path's current ending point to the given
 path's starting point. If only one of the paths has an @tech{open
 sub-path}, then it becomes (or remains) this path's @tech{open
 sub-path}.

}

@defmethod[(arc [x real?]
                [y real?]
                [width (and/c real? (not/c negative?))]
                [height (and/c real? (not/c negative?))]
                [start-radians real?]
                [end-radians real?]
                [counter-clockwise? any/c #t])
           void?]{

Extends or starts the path's @tech{open sub-path} with a curve that
 corresponds to a section of an ellipse. The ellipse is the one
 bounded by a rectangle whose top-left corner is @math{(@scheme[x],
 @scheme[y])} and whose dimensions are @scheme[width] by
 @scheme[height]. The ellipse section starts a the angle
 @scheme[start-radians] (@scheme[0] is three o'clock and half-pi is
 twelve o'clock) and continues to the angle @scheme[end-radians]; if
 @scheme[counter-clockwise?] is true, then the arc runs
 counter-clockwise from @scheme[start-radians] to
 @scheme[end-radians], otherwise it runs clockwise.

If the path has no @tech{open sub-path}, a new one is started with the
 arc's starting point. Otherwise, the arc extends the existing
 sub-path, and the existing path is connected with a line to the arc's
 starting point.

}

@defmethod[(close)
           void?]{

Closes the path's @tech{open sub-path}. If the path has no @tech{open
 sub-path}, @|MismatchExn|.

}

@defmethod[(curve-to [x1 real?]
                     [y1 real?]
                     [x2 real?]
                     [y2 real?]
                     [x3 real?]
                     [y3 real?])
           void?]{

Extends the path's @tech{open sub-path} with a Bezier curve to the
 given point @math{(@scheme[x3],@scheme[y3])}, using the points
 @math{(@scheme[x1], @scheme[y1])} and @math{(@scheme[x2],
 @scheme[y2])} as control points. If the path has no @tech{open
 sub-path}, @|MismatchExn|.

}

@defmethod[(ellipse [x real?]
                    [y real?]
                    [width (and/c real? (not/c negative?))]
                    [height (and/c real? (not/c negative?))])
           void?]{

Closes the @tech{open sub-path}, if any, and adds a @tech{closed
 sub-path} that represents an ellipse bounded by a rectangle whose
 top-left corner is @math{(@scheme[x], @scheme[y])} and whose
 dimensions are @scheme[width] by @scheme[height]. (This convenience
 method is implemented in terms of @method[dc-path% close] and
 @method[dc-path% arc].)

}

@defmethod[(get-bounding-box)
           (values real? real? real? real?)]{

Returns a rectangle that encloses the path's points.  The return
 values are the left, top, width, and height of the rectangle.

For curves within the path, the bounding box enclosed the two control
 points as well as the start and end points. Thus, the bounding box
 does not always tightly bound the path.

}

@defmethod[(line-to [x real?]
                    [y real?])
           void?]{

Extends the path's @tech{open sub-path} with a line to the given
 point. If the path has no @tech{open sub-path}, @|MismatchExn|.

}

@defmethod[(lines [points (listof (is-a?/c point%))]
                  [xoffset real? 0]
                  [yoffset real? 0])
           void?]{

Extends the path's @tech{open sub-path} with a sequences of lines to
 the given points. If the path has no @tech{open sub-path},
 @|MismatchExn|.  (This convenience method is implemented in terms of
 @method[dc-path% line-to].)

}

@defmethod[(move-to [x real?]
                    [y real?])
           void?]{

After closing the @tech{open sub-path}, if any, starts a new
 @tech{open sub-path} with the given initial point.

}

@defmethod[(open?)
           boolean?]{

Returns @scheme[#t] if the path has an @tech{open sub-path},
@scheme[#f] otherwise.

}

@defmethod[(rectangle [x real?]
                      [y real?]
                      [width (and/c real? (not/c negative?))]
                      [height (and/c real? (not/c negative?))])
           void?]{

Closes the @tech{open sub-path}, if any, and adds a closed path that
 represents a rectangle whose top-left corner is @math{(@scheme[x],
 @scheme[y])} and whose dimensions are @scheme[width] by
 @scheme[height]. (This convenience method is implemented in terms of
 @method[dc-path% close], @method[dc-path% move-to], and
 @method[dc-path% line-to].)

}

@defmethod[(reset)
           void?]{

Removes all sub-paths of the path.

}

@defmethod[(reverse)
           void?]{

Reverses the order of all points in all sub-paths. If the path has an
 @tech{open sub-path}, the starting point becomes the ending point,
 and extensions to the @tech{open sub-path} build on this new ending
 point. Reversing a @tech{closed sub-path} affects how it combines
 with other sub-paths when determining the content of a path in
 @scheme['winding] mode.

}

@defmethod[(rotate [radians real?])
           void?]{

Adjusts all points within the path (including all sub-paths), rotating
 them @scheme[radians] counter-clockwise around @math{(0, 0)}. Future
 additions to the path are not rotated by this call.

}

@defmethod[(rounded-rectangle [x real?]
                              [y real?]
                              [width (and/c real? (not/c negative?))]
                              [height (and/c real? (not/c negative?))]
                              [radius real? -0.25])
           void?]{

Closes the @tech{open sub-path}, if any, and adds a @tech{closed
 sub-path} that represents a round-cornered rectangle whose top-left
 corner is @math{(@scheme[x] @scheme[y])} and whose dimensions are
 @scheme[width] by @scheme[height]. (This convenience method is
 implemented in terms of @method[dc-path% close], @method[dc-path%
 move-to], @method[dc-path% arc], and @method[dc-path% line-to].)

If @scheme[radius] is positive, the value is used as the radius of the
 rounded corner. If @scheme[radius] is negative, the absolute value is
 used as the @italic{proportion} of the smallest dimension of the
 rectangle.

If @scheme[radius] is less than @scheme[-0.5] or more than half of
 @scheme[width] or @scheme[height], @|MismatchExn|.

}

@defmethod[(scale [x real?]
                  [y real?])
           void?]{

@index['("paths" "flipping")]{Adjusts} all points within the path
 (including all sub-paths), multiplying each x-coordinate by
 @scheme[x] and each y-coordinate by @scheme[y]. Scaling by a negative
 number flips the path over the corresponding axis. Future additions
 to the path are not scaled by this call.

}

@defmethod[(translate [x real?]
                      [y real?])
           void?]{

Adjusts all points within the path (including all sub-paths), shifting
 then @scheme[x] to the right and @scheme[y] down.  Future additions
 to the path are not translated by this call.

}}

