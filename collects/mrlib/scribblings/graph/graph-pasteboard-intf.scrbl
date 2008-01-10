#lang scribble/doc
@(require "common.ss")

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

}}

