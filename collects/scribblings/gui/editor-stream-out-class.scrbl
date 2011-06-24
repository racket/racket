#lang scribble/doc
@(require "common.rkt")

@defclass/title[editor-stream-out% object% ()]{

An @racket[editor-stream-out%] object is used to write editor
 information to a file or other output stream (such as the
 clipboard).



@defconstructor/make[([base (is-a?/c editor-stream-out-base%)])]{

An out-stream base---possibly an
@racket[editor-stream-out-bytes-base%] object---must be supplied in
@racket[base].

}

@defmethod[(jump-to [pos exact-nonnegative-integer?])
           void?]{
Jumps to a given position in the stream.

}

@defmethod[(ok?)
           boolean?]{
Returns @racket[#t] if the stream is ready for writing, @racket[#f] otherwise.
Writing to a bad stream has no effect.

}

@defmethod[(pretty-finish)
           void?]{

Ensures that the stream ends with a newline.
This method is called by
@racket[write-editor-global-footer].

}


@defmethod[(pretty-start)
           void?]{

Writes a ``comment'' into the stream that identifies the file format.
This method is called by @racket[write-editor-global-header].

}

@defmethod*[([(put [n exact-nonnegative-integer?]
                   [v bytes?])
              (is-a?/c editor-stream-out%)]
             [(put [v bytes?])
              (is-a?/c editor-stream-out%)]
             [(put [v exact-integer?])
              (is-a?/c editor-stream-out%)]
             [(put [v real?])
              (is-a?/c editor-stream-out%)])]{


Writes @racket[v], or @racket[n] bytes of @racket[v]. 

When @racket[n] is supplied with a byte-string @racket[v], use
 @method[editor-stream-in% get-unterminated-bytes] to read the bytes
 later. This is the recommended way to write out bytes to 
 be easily read in later; use @method[editor-stream-in%
 get-unterminated-bytes] to read the bytes back in.

If @racket[n] is not supplied and @racket[v] is a byte string, then
 for historical reasons, the actual number of bytes written includes a
 @racket[#\nul] terminator, so use @method[editor-stream-in%
 get-bytes] instead of @method[editor-stream-in%
 get-unterminated-bytes] to read the bytes later.

}


@defmethod[(put-fixed [v exact-integer?])
           (is-a?/c editor-stream-out%)]{

Puts a fixed-sized integer into the stream. This method is needed
 because numbers are usually written in a way that takes varying
 numbers of bytes. In some cases it is useful to temporary write a
 @racket[0] to a stream, write more data, and then go back and change
 the @racket[0] to another number; such a process requires a
 fixed-size number.

Numbers written to a stream with @method[editor-stream-out% put-fixed]
 must be read with @method[editor-stream-in% get-fixed-exact]
 or @method[editor-stream-in% get-fixed].}


@defmethod[(put-unterminated [v bytes?]) (is-a?/c editor-stream-out%)]{

The same as calling @method[editor-stream-out% put] with
@racket[(bytes-length v)] and @racket[v].}


@defmethod[(tell)
           exact-nonnegative-integer?]{

Returns the current stream position.

}}
