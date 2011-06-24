#lang scribble/doc
@(require "common.rkt")

@definterface/title[graph-pasteboard<%> ()]{


@defmethod[(get-arrowhead-params)
           (values number number number)]{

Returns the current settings for the arrowhead's drawing.
}


@defmethod[(on-mouse-over-snips [lst (listof (is-a?/c snip%))])
           void?]{

This method is called when the mouse passes over any snips
in the editor. It is only called when the list of snips
under the editor changes (ie, if the mouse moves, but
remains over the same list of snips, the method is not
called). Also, this method is called with the empty list if
the mouse leaves the pasteboard.}


@defmethod[(set-arrowhead-params [angle-width real?]
                                 [short-side real?]
                                 [long-size real?])
           void?]{

Sets drawing parameters for the arrowhead. The first is the
angle of the arrowhead's point, in radians. The second is
the length of the outside line of the arrowhead and the last
is the distance from the arrowhead's point to the place where
the arrowhead comes together.}


@defmethod[(set-draw-arrow-heads? [draw-arrow-heads? any/c])
           void?]{

Sets a boolean controlling whether or not arrow heads are
drawn on the edges between nodes.

This setting does not affect self-links---only links between two
different nodes.

}

@defmethod[(set-flip-labels? [flip-labels? any/c])
           void?]{

Sets a boolean controlling whether or not arrow labels are flipped so
the are always right-side-up.  Note that if there are two nodes with
edges going from the first to the second, and from the second to the
first, and the two have labels, then this should be turned off or the
labels will appear in the same space.

This setting does not affect self-links---only links between two
different nodes.

}

@defmethod[(draw-edges [dc (is-a?/c dc<%>)]
[left real?]
[top real?]
[right real?]
[bottom real?]
[dx real?]
[dy real?]) void?]{
  This is called by the @method[editor<%> on-paint] callback of a
  graph pasteboard, and is expected to draw the edges between the
  snips. The argments are a subset of those passed to
  @method[editor<%> on-paint] and it is only called when the
  @racket[before?] argument to   @method[editor<%> on-paint] 
  is @racket[#t].
}

@defmethod[(draw-single-edge [dc (is-a?/c dc<%>)]
			     [dx real?]
			     [dy real?]
			     [from (is-a?/c graph-snip<%>)]
			     [to (is-a?/c graph-snip<%>)]
			     [from-x real?]
			     [from-y real?]
			     [to-x real?]
			     [to-y real?]
			     [arrow-point-ok? (-> real? real? boolean?)]) void?]{

This method is called to draw each edge in the graph, except
for the edges that connect a node to itself.

The @racket[dc], @racket[dx], and @racket[dy] arguments are
the same as in @method[editor<%> on-paint]. 

The
@racket[from-x], @racket[from-y], @racket[to-x], and
@racket[to-y] arguments specify points on the source and
destination snip's bounding box where a straight line
between the centers of the snip would intersect.

The @racket[arrow-point-ok?] function returns @racket[#t]
when the point specified by its arguments is inside the
smallest rectangle that covers both the source and
destination snips, but is outside of both of the rectangles
that surround the source and destination snips themselves.

This default implementation uses @racket[update-polygon] to compute
the arrowheads and otherwise draws a straight line between the two
points and then the arrowheads, unless the arrowhead points
are not ok according to @racket[arrow-point-ok?], in which case
it just draws the line.
}

@defmethod[(update-arrowhead-polygon [from-x real?] [from-y real?] [to-x real?] [to-y real?] 
                                     [point1 (is-a?/c point%)]
                                     [point2 (is-a?/c point%)]
                                     [point3 (is-a?/c point%)]
                                     [point4 (is-a?/c point%)]) void?]{

Updates the arguments @racket[point1], @racket[point2], @racket[point3], @racket[point4] with the coordinates
of an arrowhead for a line that connects (@racket[from-x],@racket[from-y]) to (@racket[to-x],@racket[to-y]).
}

}
