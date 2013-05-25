#lang scribble/doc
@(require "common.rkt" (for-label mrlib/graph))

@title[#:style 'toc]{Graphs}

@defmodule[mrlib/graph]{The @racketmodname[mrlib/graph] library
provides a graph drawing toolkit built out of @racket[pasteboard%]s.}

@local-table-of-contents[]

@include-section["graph-pasteboard-intf.scrbl"]
@include-section["graph-pasteboard-mixin.scrbl"]
@include-section["graph-snip-intf.scrbl"]
@include-section["graph-snip-mixin.scrbl"]

@section{Graph Functions}

@defproc*[([(add-links [parent (is-a?/c graph-snip<%>)]
                       [child (is-a?/c graph-snip<%>)])
            void?]
           [(add-links [parent (is-a?/c graph-snip<%>)]
                       [child (is-a?/c graph-snip<%>)])
            void?]
           [(add-links [parent (is-a?/c graph-snip<%>)]
                       [child (is-a?/c graph-snip<%>)]
                       [dark-pen (or/c (is-a?/c pen) false/c)]
                       [light-pen (or/c (is-a?/c pen) false/c)]
                       [dark-brush (or/c (is-a?/c brush%) false/c)]
                       [light-brush (or/c (is-a?/c brush%) false/c)]
                       [label (or/c string? false/c) #f])
            void?]
           [(add-links [parent (is-a?/c graph-snip<%>)]
                       [child (is-a?/c graph-snip<%>)]
                       [dark-pen (or/c (is-a?/c pen) false/c)]
                       [light-pen (or/c (is-a?/c pen) false/c)]
                       [dark-brush (or/c (is-a?/c brush%) false/c)]
                       [light-brush (or/c (is-a?/c brush%) false/c)]
                       [dx real?]
                       [dy real?]
                       [label (or/c string? false/c) #f])
           void?])]{

Connects a parent snip to a child snip within a pasteboard.

The default @racket[dark-pen]/@racket[dark-brush] and
@racket[light-pen]/@racket[light-brush] are blue and purple,
respectively. The @racket[dark-pen] and @racket[dark-brush] are used
when the mouse cursor is over the snip (or a child or parent), and the
@racket[light-pen] and @racket[light-brush] are used when the mouse
cursor is not over the snip. The brush is used to draw inside the
arrow head and the pen is used to draw the border of the arrowhead and
the line connecting the two snips.

if @racket[label] is provided and not @racket[#f], it is used as a
label on the edge.

When @racket[dx] and @racket[dy] are provided, the are offsets for the
head and the tail of the arrow. Otherwise, @racket[0] offsets are
used.}

@defproc[(add-links/text-colors [parent (is-a?/c graph-snip<%>)]
                                [child (is-a?/c graph-snip<%>)]
                                [dark-pen (or/c (is-a?/c pen) false/c)]
                                [light-pen (or/c (is-a?/c pen) false/c)]
                                [dark-brush (or/c (is-a?/c brush%) false/c)]
                                [light-brush (or/c (is-a?/c brush%) false/c)]
                                [dark-text (or/c (is-a?/c color%) false/c)]
                                [light-text (or/c (is-a?/c color) false/c)]
                                [dx real?]
                                [dy real?]
                                [label (or/c string? false/c)])
          void?]{

Like @racket[add-links], but with extra @racket[dark-text] and
@racket[light-text] arguments to set the colors of the label.}

@defproc[(remove-links [parent (is-a?/c graph-snip<%>)]
                       [child (is-a?/c graph-snip<%>)])
          void?]{

Disconnects a parent snip from a child snip within a pasteboard.}

@defproc[(set-link-label [parent (is-a?/c graph-snip<%>)]
                         [child (is-a?/c graph-snip<%>)]
                         [label (or/c string? false/c)])
          void?]{

Changes the label on the edge going from @racket[child] to
@racket[parent] to be @racket[label].  If there is no existing edge
between the two nodes, then nothing happens.}
