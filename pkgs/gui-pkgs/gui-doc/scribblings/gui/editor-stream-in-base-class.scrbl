#lang scribble/doc
@(require "common.rkt")

@defclass/title[editor-stream-in-base% object% ()]{

An @racket[editor-stream-in-base%] object is used by an
 @racket[editor-stream-in%] object to perform low-level reading of
 data.

The @racket[editor-stream-in-base%] class is never instantiated
 directly, but the derived class @racket[editor-stream-in-bytes-base%]
 can be instantiated.  New derived classes must override all of the
 methods described in this section.


@defmethod[(bad?)
           boolean?]{

Returns @racket[#t] if there has been an error reading from the
 stream, @racket[#f] otherwise.

}

@defmethod[(read [data (and/c vector? (not immutable?))])
           exact-nonnegative-integer?]{

Like @method[editor-stream-in-base% read-bytes], but fills a supplied
vector with Latin-1 characters instead of filling a byte string.  This method
is implemented by default via @method[editor-stream-in-base% read-bytes].}

@defmethod[(read-bytes [bstr (and/c bytes? (not immutable?))])
           exact-nonnegative-integer?]{

Reads bytes to fill the supplied byte string. The return value is the
 number of bytes read, which may be less than the number
 requested if the stream is emptied. If the stream is emptied, the
 next call to @method[editor-stream-in-base% bad?] must return
 @racket[#t].}

@defmethod[(read-byte) (or/c byte? #f)]{

Reads a single byte and return it, or returns @racket[#f] if no more
bytes are available. The default implementation of this method uses
@method[editor-stream-in-base% read-bytes].

}

@defmethod[(seek [pos exact-nonnegative-integer?])
           void?]{

Moves to the specified absolute position in the stream.

}

@defmethod[(skip [n exact-nonnegative-integer?])
           void?]{

Skips past the next @racket[n] characters in the stream.

}

@defmethod[(tell)
           exact-nonnegative-integer?]{

Returns the current stream position.

}}

