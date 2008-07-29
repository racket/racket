#lang scribble/doc
@(require "common.ss")

@defclass/title[editor-stream-in-base% object% ()]{

An @scheme[editor-stream-in-base%] object is used by an
 @scheme[editor-stream-in%] object to perform low-level reading of
 data.

The @scheme[editor-stream-in-base%] class is never instantiated
 directly, but the derived class @scheme[editor-stream-in-bytes-base%]
 can be instantiated.  New derived classes must override all of the
 methods described in this section.


@defmethod[(bad?)
           boolean?]{

Returns @scheme[#t] if there has been an error reading from the
 stream, @scheme[#f] otherwise.

}

@defmethod[(read [data (and/c bytes? (not/c immutable?))])
           exact-nonnegative-integer?]{

Reads characters to fill the supplied vector. The return value is the
 number of characters read, which may be less than the number
 requested if the stream is emptied. If the stream is emptied, the
 next call to @method[editor-stream-in-base% bad?] must return
 @scheme[#t].

}


@defmethod[(seek [pos exact-nonnegative-integer?])
           void?]{

Moves to the specified absolute position in the stream.

}

@defmethod[(skip [n exact-nonnegative-integer?])
           void?]{

Skips past the next @scheme[n] characters in the stream.

}

@defmethod[(tell)
           exact-nonnegative-integer?]{

Returns the current stream position.

}}

