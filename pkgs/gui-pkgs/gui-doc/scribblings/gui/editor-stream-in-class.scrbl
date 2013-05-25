#lang scribble/doc
@(require "common.rkt")

@defclass/title[editor-stream-in% object% ()]{

An @racket[editor-stream-in%] object is used to read editor
 information from a file or other input stream (such as the
 clipboard).



@defconstructor/make[([base (is-a?/c editor-stream-in-base%)])]{

An in-stream base---possibly an @racket[editor-stream-in-bytes-base%]
 object---must be supplied in @racket[base].

}


@defmethod*[([(get [v (box/c exact-integer?)])
              (is-a?/c editor-stream-in%)]
             [(get [v (box/c real?)])
              (is-a?/c editor-stream-in%)])]{

Reads data from the stream, returning itself.
Reading from a bad stream always gives @racket[0].

@boxisfill[@racket[v] @elem{the next integer or floating-point value in the stream}]

}


@defmethod[(get-bytes [len (or/c (box/c exact-nonnegative-integer?) #f) #f])
           (or/c bytes? #f)]{

Like @method[editor-stream-in% get-unterminated-bytes], but the last
 read byte is assumed to be a nul terminator and discarded. Use this
 method when data is written by a call to @method[editor-stream-out%
 put] without an explicit byte count, and use
 @method[editor-stream-in% get-unterminated-bytes] when data is
 written with an explicit byte count.

@boxisfillnull[@racket[len] @elem{the length of the byte string plus one (to indicate the terminator)}]

}

@defmethod[(get-exact)
           exact-integer?]{

Returns the next integer value in the stream.

}

@defmethod[(get-fixed [v (box/c exact-integer?)])
           (is-a?/c editor-stream-in%)]{

@boxisfill[@racket[v] @elem{a fixed-size integer from the stream obtained through 
           @method[editor-stream-in% get-fixed-exact]}]

}

@defmethod[(get-fixed-exact)
           exact-integer?]{

Gets a fixed-sized integer from the stream. See
@method[editor-stream-out% put-fixed] for more information.
Reading from a bad stream always gives @racket[0].

}

@defmethod[(get-inexact)
           real?]{

Returns the next floating-point value in the stream.

}

@defmethod[(get-unterminated-bytes [len (or/c (box/c exact-nonnegative-integer?) #f) #f])
           (or/c bytes? #f)]{

Returns the next byte string from the stream.  This is
the recommended way to read bytes back in from a stream;
use @method[editor-stream-out% put] with two arguments
(passing along the length of the bytes) to write out the bytes
to match this method.

Reading from a bad stream returns @racket[#f] or @racket[#""].

Note that when @method[editor-stream-out% put] is not given a byte
 length, it includes an extra byte for a nul terminator; use
 @method[editor-stream-in% get-bytes] to read such byte strings.

@boxisfillnull[@racket[len] @elem{the length of the byte string}]

}

@defmethod[(jump-to [pos exact-nonnegative-integer?])
           void?]{

Jumps to a given position in the stream.

}

@defmethod[(ok?)
           boolean?]{

Returns @racket[#t] if the stream is ready for reading, @racket[#f] otherwise.
Reading from a bad stream always returns @racket[0] or @racket[""].

}

@defmethod[(remove-boundary)
           void?]{

See @method[editor-stream-in% set-boundary].

}

@defmethod[(set-boundary [n exact-nonnegative-integer?])
           void?]{

Sets a file-reading boundary at @racket[n] bytes past the current
 stream location. If there is an attempt to read past this boundary,
 an error is signaled. The boundary is removed with a call to
 @method[editor-stream-in% remove-boundary].  Every call to
 @method[editor-stream-in% set-boundary] must be balanced by a call to
 @method[editor-stream-in% remove-boundary].

Boundaries help keep a subroutine from reading too much data leading
 to confusing errors. However, a malicious subroutine can call
 @method[editor-stream-in% remove-boundary] on its own.

}


@defmethod[(skip [n exact-nonnegative-integer?])
           void?]{

Skips past the next @racket[n] bytes in the stream.

}

@defmethod[(tell)
           exact-nonnegative-integer?]{

Returns the current stream position.

}}
