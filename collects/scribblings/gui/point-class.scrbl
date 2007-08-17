#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[point% object% ()]{

A @scheme[point%] is used for certain drawing commands. It
 encapsulates two real numbers.

@defconstructor*/make[(()
                       ([x real?]
                        [y real?]))]{

Creates a point. If @scheme[x] and @scheme[y] are not supplied, they
 are set to @scheme[0].
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

