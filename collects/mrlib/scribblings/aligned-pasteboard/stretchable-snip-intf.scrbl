#lang scribble/doc
@(require "common.rkt")

@definterface/title[stretchable-snip<%> ()]{

This interface must be implemented by any snip class who's
objects will be stretchable when inserted into an
@racket[aligned-pasteboard<%>].



@defmethod[(get-aligned-min-height)
           real?]{
The minimum height that the snip can be resized to

}

@defmethod[(get-aligned-min-width)
           real?]{
The minimum width that the snip can be resized to.

}

@defmethod[(stretchable-height)
           boolean?]{
Whether or not the snip can be stretched in the Y dimension

}

@defmethod[(stretchable-width)
           boolean?]{
Whether or not the snip can be stretched in the X dimension

}}
