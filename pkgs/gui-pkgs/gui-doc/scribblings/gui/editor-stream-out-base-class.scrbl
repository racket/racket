#lang scribble/doc
@(require "common.rkt")

@defclass/title[editor-stream-out-base% object% ()]{

An @racket[editor-stream-out-base%] object is used by an
 @racket[editor-stream-out%] object to perform low-level writing of
 data.

The @racket[editor-stream-out-base%] class is never instantiated
 directly, but the derived class
 @racket[editor-stream-out-bytes-base%] can be instantiated.  New
 derived classes must override all of the methods described in this
 section.


@defmethod[(bad?)
           boolean?]{

Returns @racket[#t] if there has been an error writing to the stream,
 @racket[#f] otherwise.

}

@defmethod[(seek [pos exact-nonnegative-integer?])
           void?]{

Moves to the specified absolute position in the stream.

}

@defmethod[(tell)
           exact-nonnegative-integer?]{

Returns the current stream position.

}

@defmethod[(write [data (listof char?)])
           void?]{

Writes data (encoded as Latin-1 characters) to the stream. This method
is implemented by default via @method[editor-stream-out-base%
write-bytes].}

@defmethod[(write-bytes [bstr bytes?]) void?]{

Writes data to the stream.}}


