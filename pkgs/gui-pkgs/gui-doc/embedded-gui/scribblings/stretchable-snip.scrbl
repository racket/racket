#lang scribble/doc
@(require "common.rkt")

@definterface/title[stretchable-snip<%> ()]{

Must be implemented by any snip class whose objects will be
stretchable when inserted into an @racket[aligned-pasteboard<%>]
within a @racket[snip-wrapper%].

@defmethod[(get-aligned-min-width) (and/c real? (not/c negative?))]{

The minimum width that the snip can be resized to.}

@defmethod[(get-aligned-min-height) (and/c real? (not/c negative?))]{

The minimum height that the snip can be resized to.}

@defmethod*[([(stretchable-width) boolean?]
             [(stretchable-width [stretch? boolean?]) void?])]{

Gets/sets whether or not the snip can be stretched in the X
dimension.}

@defmethod*[([(stretchable-height) boolean?]
             [(stretchable-height [stretch? boolean?]) void?])]{

Gets/sets whether or not the snip can be stretched in the Y
dimension.}

}
