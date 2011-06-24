#lang scribble/doc
@(require "common.rkt")

@definterface/title[aligned-pasteboard<%> ()]{



@defmethod[(get-aligned-min-height)
           real?]{

The minimum height an aligned-pasteboard can be and still
fit the heights of all of its children.}


@defmethod[(get-aligned-min-width)
           real?]{
The minimum width an aligned-pasteboard can be and still
fit the widths of all of its children.

}

@defmethod*[([(realign [width exact-nonnegative-integer?]
                       [height exact-nonnegative-integer?])
              void?]
             [(realign)
              void?])]{

Realigns the children inside the @racket[aligned-pasteboard<%>] to
either a given @racket[width] and @racket[height] or the previously
alloted width and height.}

@defmethod[(set-aligned-min-sizes)
           void?]{

Calculates the minimum width and height of the of the
pasteboard based on children's min-sizes and stores it for
later retrieval via the getters.}}
