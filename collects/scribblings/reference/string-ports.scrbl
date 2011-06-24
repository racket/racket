#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "stringport"]{String Ports}

String input and output ports do not need to be explicitly closed. The
@racket[file-position] procedure works for string ports in
position-setting mode.

@defproc[(open-input-bytes [bstr bytes?] [name any/c 'string]) input-port?]{

Creates an input port that reads characters from @racket[bstr] (see
@secref["bytestrings"]). Modifying @racket[bstr] afterward does not
affect the byte stream produced by the port. The optional
@racket[name] argument is used as the name for the returned port.}

@defproc[(open-input-string [str string?] [name any/c 'string]) input-port?]{

Creates an input port that reads bytes from the UTF-8 encoding (see
@secref["encodings"]) of @racket[str]. The optional @racket[name]
argument is used as the name for the returned port.}

@defproc[(open-output-bytes [name any/c 'string]) output-port?]{

Creates an output port that accumulates the output into a byte
string. The optional @racket[name] argument is used as the name for
the returned port.}

@defproc[(open-output-string [name any/c 'string]) output-port?]{The
same as @racket[open-output-bytes].}

@defproc[(get-output-bytes [out output-port?]
                           [reset? any/c #f]
                           [start-pos exact-nonnegative-integer? 0]
                           [end-pos exact-nonnegative-integer? #f])
         bytes?]{

Returns the bytes accumulated in @racket[out] so far in a
freshly allocated byte string (including any bytes written after the
port's current position, if any). The @racket[out] port must be a
string output port produced by @racket[open-output-bytes] (or
@racket[open-output-string]) or a structure whose
@racket[prop:output-port] property refers to such an output port
(transitively).

If @racket[reset?] is true, then all bytes are removed from the port,
and the port's position is reset to @racket[0]; if @racket[reset?] is
@racket[#f], then all bytes remain in the port for further
accumulation (so they are returned for later calls to
@racket[get-output-bytes] or @racket[get-output-string]), and the
port's position is unchanged.

The @racket[start-pos] and @racket[end-pos] arguments specify the
range of bytes in the port to return; supplying @racket[start-pos] and
@racket[end-pos] is the same as using @racket[subbytes] on the result
of @racket[get-output-bytes], but supplying them to
@racket[get-output-bytes] can avoid an allocation. The
@racket[end-pos] argument can be @racket[#f], which corresponds to not
passing a second argument to @racket[subbytes].}

@defproc[(get-output-string [out output-port?]) string?]{
Returns @racket[(bytes->string/utf-8 (get-output-bytes out) #\?)].}

@examples[
(define i (open-input-string "hello world"))
(define o (open-output-string))
(write (read i) o)
(get-output-string o)
]
