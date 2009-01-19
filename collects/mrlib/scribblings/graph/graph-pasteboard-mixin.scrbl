#lang scribble/doc
@(require "common.ss")

@defmixin/title[graph-pasteboard-mixin (pasteboard%) (graph-pasteboard<%>)]{

@defconstructor/auto-super[([edge-labels? boolean? #t]
                            [edge-label-font (or/c #f (is-a?/c font%)) #f])]{

If @scheme[edge-labels?] is @scheme[#f], no edge labels are
drawn. Otherwise, they are.

If @scheme[edge-label-font] is supplied, it is used when drawing the
labels on the edges. Otherwise, the font is not set before drawing
the labels, defaulting to the @scheme[dc<%>] object's font.

}

This mixin overrides many methods to draw lines between
@scheme[graph-snip<%>] that it contains.}
