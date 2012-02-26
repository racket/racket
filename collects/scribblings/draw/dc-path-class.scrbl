#lang scribble/doc
@(require "common.rkt")

@defclass/title[dc-path% object% ()]{

A path is a set of figures defined by curves. A path can be used with
the @method[dc<%> draw-path] method of a @racket[dc<%>] object to draw
the path's curves as lines, fill the region bounded by the path's
curves, or both. A path can also be used with the @method[region%
set-path] method of a @racket[region%] object to generate a region
bounded by the path's curves.

A path consists of zero or more @deftech{closed sub-paths}, and
 possibly one @deftech{open sub-path}. Some @racket[dc-path%] methods
 extend the open sub-path, some @racket[dc-path%] methods close the
 open sub-path, and some @racket[dc-path%] methods add closed
 sub-paths. This approach to drawing formulation is inherited from
 PostScript @cite["Adobe99"].

When a path is drawn as a line, a closed sub-path is drawn as a closed
 figure, analogous to a polygon. An open sub-path is drawn with
 disjoint start and end points, analogous lines drawn with
 @xmethod[dc<%> draw-lines].

When a path is filled or used as a region, the open sub-path (if any)
 is treated as if it were closed. The content of a path is determined
 either through the @racket['even-odd] rule or the @racket['winding]
 rule, as selected at the time when the path is filled or used to
 generate a region.

A path is not connected to any particular @racket[dc<%>] object, so
 setting a @racket[dc<%>] origin or scale does not affect path
 operations. Instead, a @racket[dc<%>]'s origin and scale apply at the
 time that the path is drawn or used to set a region.

@defconstructor[()]{

Creates a new path that contains no sub-paths (and no @tech{open
sub-path}).

}


@defmethod[(append [path (is-a?/c dc-path%)])
           void?]{

Adds the sub-paths of @racket[path] to @this-obj[]. @tech{Closed
 sub-paths} of @racket[path] are added as @tech{closed sub-paths} to
 @this-obj[]. If both paths have an @tech{open sub-path}, then this
 path's sub-path is extended by the given path's @tech{open sub-path},
 adding a line from this path's current ending point to the given
 path's starting point. If only one of the paths has an @tech{open
 sub-path}, then it becomes (or remains) this path's @tech{open
 sub-path}.

}

@defmethod[(arc [x real?]
                [y real?]
                [width real?]
                [height real?]
                [start-radians real?]
                [end-radians real?]
                [counter-clockwise? any/c #t])
           void?]{

Extends or starts the path's @tech{open sub-path} with a curve that
 corresponds to a section of an ellipse. If @racket[width] and @racket[height]
 are non-negative, the ellipse is the one
 bounded by a rectangle whose top-left corner is @math{(@racket[x],
 @racket[y])} and whose dimensions are @racket[width] by
 @racket[height]; if @racket[width] is negative, then
 the rectangle's right edge is @racket[x], and the ellipse
 width is @racket[(abs width)], while a negative @racket[height]
 similarly makes @racket[y] is the bottom edge of the ellipse and
 the height @racket[(abs height)].
 @margin-note*{Support for negative @racket[width] and @racket[height]
 helps avoid round-off problems for aligned drawing in an eventual 
 destination, since @method[dc-path% arc] reduces its input to a sequence of curves.
 In contrast, @xmethod[dc<%> draw-arc] can automatically correct for round off,
 since the drawing mode is known immediately.}
 The ellipse section starts a the angle
 @racket[start-radians] (@racket[0] is three o'clock and half-π is
 twelve o'clock) and continues to the angle @racket[end-radians]; if
 @racket[counter-clockwise?] is true, then the arc runs
 counter-clockwise from @racket[start-radians] to
 @racket[end-radians], otherwise it runs clockwise.

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
 given point @math{(@racket[x3],@racket[y3])}, using the points
 @math{(@racket[x1], @racket[y1])} and @math{(@racket[x2],
 @racket[y2])} as control points. If the path has no @tech{open
 sub-path}, @|MismatchExn|.

}

@defmethod[(ellipse [x real?]
                    [y real?]
                    [width (and/c real? (not/c negative?))]
                    [height (and/c real? (not/c negative?))])
           void?]{

Closes the @tech{open sub-path}, if any, and adds a @tech{closed
 sub-path} that represents an ellipse bounded by a rectangle whose
 top-left corner is @math{(@racket[x], @racket[y])} and whose
 dimensions are @racket[width] by @racket[height]. (This convenience
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

@defmethod[(lines [points (or/c (listof (is-a?/c point%))
                                (listof (cons/c real? real?)))]
                  [xoffset real? 0]
                  [yoffset real? 0])
           void?]{

Extends the path's @tech{open sub-path} with a sequences of lines to
 the given points. A pair is treated as a point where the @racket[car]
 of the pair is the x-value and the @racket[cdr] is the y-value.
 If the path has no @tech{open sub-path},
 @|MismatchExn|.  (This convenience method is implemented in terms of
 @method[dc-path% line-to].)

}

@defmethod[(move-to [x real?]
                    [y real?])
           void?]{

After closing the @tech{open sub-path}, if any, starts a new
 @tech{open sub-path} with the given initial point.}


@defmethod[(open?)
           boolean?]{

Returns @racket[#t] if the path has an @tech{open sub-path},
@racket[#f] otherwise.}


@defmethod[(rectangle [x real?]
                      [y real?]
                      [width (and/c real? (not/c negative?))]
                      [height (and/c real? (not/c negative?))])
           void?]{

Closes the @tech{open sub-path}, if any, and adds a closed path that
 represents a rectangle whose top-left corner is @math{(@racket[x],
 @racket[y])} and whose dimensions are @racket[width] by
 @racket[height]. (This convenience method is implemented in terms of
 @method[dc-path% close], @method[dc-path% move-to], and
 @method[dc-path% line-to].)}


@defmethod[(reset)
           void?]{

Removes all sub-paths of the path.}


@defmethod[(reverse)
           void?]{

Reverses the order of all points in all sub-paths. If the path has an
 @tech{open sub-path}, the starting point becomes the ending point,
 and extensions to the @tech{open sub-path} build on this new ending
 point. Reversing a @tech{closed sub-path} affects how it combines
 with other sub-paths when determining the content of a path in
 @racket['winding] mode.}


@defmethod[(rotate [radians real?])
           void?]{

Adjusts all points within the path (including all sub-paths), rotating
 them @racket[radians] counter-clockwise around @math{(0, 0)}. Future
 additions to the path are not rotated by this call.}


@defmethod[(rounded-rectangle [x real?]
                              [y real?]
                              [width (and/c real? (not/c negative?))]
                              [height (and/c real? (not/c negative?))]
                              [radius real? -0.25])
           void?]{

Closes the @tech{open sub-path}, if any, and adds a @tech{closed
 sub-path} that represents a round-cornered rectangle whose top-left
 corner is @math{(@racket[x] @racket[y])} and whose dimensions are
 @racket[width] by @racket[height]. (This convenience method is
 implemented in terms of @method[dc-path% close], @method[dc-path%
 move-to], @method[dc-path% arc], and @method[dc-path% line-to].)

If @racket[radius] is positive, the value is used as the radius of the
 rounded corner. If @racket[radius] is negative, the absolute value is
 used as the @italic{proportion} of the smallest dimension of the
 rectangle.

If @racket[radius] is less than @racket[-0.5] or more than half of
 @racket[width] or @racket[height], @|MismatchExn|.}


@defmethod[(scale [x real?]
                  [y real?])
           void?]{

@index['("paths" "flipping")]{Adjusts} all points within the path
 (including all sub-paths), multiplying each x-coordinate by
 @racket[x] and each y-coordinate by @racket[y]. Scaling by a negative
 number flips the path over the corresponding axis. Future additions
 to the path are not scaled by this call.}


@defmethod[(text-outline [font (is-a?/c font%)]
                         [str string?]
                         [x real?]
                         [y real?]
                         [combine? any/c #f])
           void?]{

Closes the @tech{open sub-path}, if any, and adds a @tech{closed
 sub-path} to outline @racket[str] using @racket[font]. The
 top left of the text is positioned at @racket[x] and @racket[y]. The
 @racket[combine?] argument enables kerning and character combinations
 as for @xmethod[dc<%> draw-text].}


@defmethod[(transform [m (vector/c real? real? real? real? real? real?)])
           void?]{

Adjusts all points within the path (including all sub-paths) by
applying the transformation represented by @racket[m].

See @method[dc<%> get-initial-matrix] for information on the matrix as
 represented by a vector @racket[m].}


@defmethod[(translate [x real?]
                      [y real?])
           void?]{

Adjusts all points within the path (including all sub-paths), shifting
 then @racket[x] to the right and @racket[y] down.  Future additions
 to the path are not translated by this call.}}
