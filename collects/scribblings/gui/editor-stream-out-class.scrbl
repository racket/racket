#lang scribble/doc
@(require "common.ss")

@defclass/title[editor-stream-out% object% ()]{

An @scheme[editor-stream-out%] object is used to write editor
 information to a file or other output stream (such as the
 clipboard).



@defconstructor/make[([base (is-a?/c editor-stream-out-base%)])]{

An out-stream base---possibly an
@scheme[editor-stream-out-bytes-base%] object---must be supplied in
@scheme[base].


}

@defmethod[(jump-to [pos exact-nonnegative-integer?])
           void?]{
Jumps to a given position in the stream.

}

@defmethod[(ok?)
           boolean?]{
Returns @scheme[#t] if the stream is ready for writing, @scheme[#f] otherwise.
Writing to a bad stream has no effect.

}

@defmethod[(pretty-finish)
           void?]{

Ensures that the stream ends with a newline.
This method is called by
@scheme[write-editor-global-footer].

}


@defmethod[(pretty-start)
           void?]{

Writes a ``comment'' into the stream that identifies the file format.
This method is called by @scheme[write-editor-global-header].

}

@defmethod*[([(put [n exact-nonnegative-integer?]
                   [v bytes?])
              (is-a?/c editor-stream-out%)]
             [(put [v bytes?])
              (is-a?/c editor-stream-out%)]
             [(put [v (and/c exact? integer?)])
              (is-a?/c editor-stream-out%)]
             [(put [v real?])
              (is-a?/c editor-stream-out%)])]{


Writes @scheme[v], or @scheme[n] bytes of @scheme[v]. 

When @scheme[n] is supplied with a byte-string @scheme[v], use
 @method[editor-stream-in% get-unterminated-bytes] to read the bytes
 later. This is the recommended way to write out bytes to 
 be easily read in later; use @method[editor-stream-in%
 get-unterminated-bytes] to read the bytes back in.

If @scheme[n] is not supplied and @scheme[v] is a byte string, then
 for historical reasons, the actual number of bytes written includes a
 @scheme[#\nul] terminator, so use @method[editor-stream-in%
 get-bytes] instead of @method[editor-stream-in%
 get-unterminated-bytes] to read the bytes later.

}


@defmethod[(put-fixed [v (and/c exact? integer?)])
           (is-a?/c editor-stream-out%)]{

Puts a fixed-sized integer into the stream. This method is needed
 because numbers are usually written in a way that takes varying
 numbers of bytes. In some cases it is useful to temporary write a
 @scheme[0] to a stream, write more data, and then go back and change
 the @scheme[0] to another number; such a process requires a
 fixed-size number.

Numbers written to a stream with @method[editor-stream-out% put-fixed]
 must be read with @method[editor-stream-in% get-fixed-exact]
 or @method[editor-stream-in% get-fixed].}


@defmethod[(put-unterminated [v bytes?]) (is-a?/c editor-stream-out%)]{

The same as calling @method[editor-stream-out% put] with
@scheme[(bytes-length v)] and @scheme[v].}


@defmethod[(tell)
           exact-nonnegative-integer?]{

Returns the current stream position.

}}
