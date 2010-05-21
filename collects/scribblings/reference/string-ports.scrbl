#lang scribble/doc
@(require "mz.ss")

@title[#:tag "stringport"]{String Ports}

String input and output ports do not need to be explicitly closed. The
@scheme[file-position] procedure works for string ports in
position-setting mode.

@defproc[(open-input-bytes [bstr bytes?] [name any/c 'string]) input-port?]{

Creates an input port that reads characters from @scheme[bstr] (see
@secref["bytestrings"]). Modifying @scheme[bstr] afterward does not
affect the byte stream produced by the port. The optional
@scheme[name] argument is used as the name for the returned port.}

@defproc[(open-input-string [str string?] [name any/c 'string]) input-port?]{

Creates an input port that reads bytes from the UTF-8 encoding (see
@secref["encodings"]) of @scheme[str]. The optional @scheme[name]
argument is used as the name for the returned port.}

@defproc[(open-output-bytes [name any/c 'string]) output-port?]{

Creates an output port that accumulates the output into a byte
string. The optional @scheme[name] argument is used as the name for
the returned port.}

@defproc[(open-output-string [name any/c 'string]) output-port?]{The
same as @scheme[open-output-bytes].}

@defproc[(get-output-bytes [out output-port?]
                           [reset? any/c #f]
                           [start-pos exact-nonnegative-integer? 0]
                           [end-pos exact-nonnegative-integer? #f])
         bytes?]{

Returns the bytes accumulated in @scheme[out] so far in a
freshly-allocated byte string (including any bytes written after the
port's current position, if any). the @scheme[out] port must be a
string output port produced by @scheme[open-output-bytes] (or
@scheme[open-output-string]) or a structure whose
@scheme[prop:output-port] property refers to such an output port
(transitively).

If @scheme[reset?] is true, then all bytes are removed from the port,
and the port's position is reset to @scheme[0]; if @scheme[reset?] is
@scheme[#f], then all bytes remain in the port for further
accumulation (so they are returned for later calls to
@scheme[get-output-bytes] or @scheme[get-output-string]), and the
port's position is unchanged.

The @scheme[start-pos] and @scheme[end-pos] arguments specify the
range of bytes in the port to return; supplying @scheme[start-pos] and
@scheme[end-pos] is the same as using @scheme[subbytes] on the result
of @scheme[get-output-bytes], but supplying them to
@scheme[get-output-bytes] can avoid an allocation. The
@scheme[end-pos] argument can be @scheme[#f], which corresponds to not
passing a second argument to @scheme[subbytes].}

@defproc[(get-output-string [out output-port?]) string?]{
Returns @scheme[(bytes->string/utf-8 (get-output-bytes out) #\?)].}

@examples[
(define i (open-input-string "hello world"))
(define o (open-output-string))
(write (read i) o)
(get-output-string o)
]
