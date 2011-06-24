#lang scribble/doc
@(require "common.rkt")

@defclass/title[point% object% ()]{

A @racket[point%] is used for certain drawing commands. It
 encapsulates two real numbers.

@defconstructor*/make[(()
                       ([x real?]
                        [y real?]))]{

Creates a point. If @racket[x] and @racket[y] are not supplied, they
 are set to @racket[0].
}

@defmethod[(get-x)
           real?]{
Gets the point x-value.

}

@defmethod[(get-y)
           real?]{
Gets the point y-value.

}

@defmethod[(set-x [x real?])
           void?]{
Sets the point x-value.

}

@defmethod[(set-y [y real?])
           void?]{

Sets the point y-value.

}}

