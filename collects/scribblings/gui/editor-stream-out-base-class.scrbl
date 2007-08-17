#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[editor-stream-out-base% object% ()]{

An @scheme[editor-stream-out-base%] object is used by an
 @scheme[editor-stream-out%] object to perform low-level writing of
 data.

The @scheme[editor-stream-out-base%] class is never instantiated
 directly, but the derived class
 @scheme[editor-stream-out-bytes-base%] can be instantiated.  New
 derived classes must override all of the methods described in this
 section.


@defmethod[(bad?)
           boolean?]{

Returns @scheme[#t] if there has been an error writing to the stream,
 @scheme[#f] otherwise.

}

@defmethod[(seek [pos nonnegative-exact-integer?])
           void?]{

Moves to the specified absolute position in the stream.

}

@defmethod[(tell)
           nonnegative-exact-integer?]{

Returns the current stream position.

}

@defmethod[(write [data bytes?])
           void?]{

Writes data to the stream.

}}
