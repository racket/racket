#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "mz:ports"]{Ports}

@deftech{Ports} produce and consume bytes. When a port is provided to
a character-based operation, such as @scheme[read], the port's bytes
are read and interpreted as a UTF-8 encoding of characters (see also
@secref["mz:encodings"]). Thus, reading a single character may require
reading multiple bytes, and a procedure like @scheme[char-ready?] may
need to peek several bytes into the stream to determine whether a
character is available. In the case of a byte stream that does not
correspond to a valid UTF-8 encoding, functions such as
@scheme[read-char] may need to peek one byte ahead in the stream to
discover that the stream is not a valid encoding.

When an input port produces a sequence of bytes that is not a valid
UTF-8 encoding in a character-reading context, then bytes that
constitute an invalid sequence are converted to the character
@litchar{?}. Specifically, bytes 255 and 254 are always converted to
@litchar{?}, bytes in the range 192 to 253 produce @litchar{?} when
they are not followed by bytes that form a valid UTF-8 encoding, and
bytes in the range 128 to 191 are converted to @litchar{?} when they
are not part of a valid encoding that was started by a preceding byte
in the range 192 to 253. To put it another way, when reading a
sequence of bytes as characters, a minimal set of bytes are changed to
63 (which is the value that @scheme[(char->integer #\?)] produces) so
that the entire sequence of bytes is a valid UTF-8 encoding.

See @secref["mz:bytestrings"] for procedures that facilitate
conversions using UTF-8 or other encodings. See also
@scheme[reencode-input-port] and @scheme[reencode-output-port] for
obtaining a UTF-8-based port from one that uses a different encoding
of characters.

The global variable @scheme[eof] is bound to the end-of-file
value. The standard predicate @scheme[eof-object?] returns @scheme[#t]
only when applied to this value. Reading from a port produces an
end-of-file result when the port has no more data, but some ports may
also return end-of-file mid-stream. For example, a port connected to a
Unix terminal returns an end-of-file when the user types control-D; if
the user provides more input, the port returns additional bytes after
the end-of-file.

@defproc[(input-port? [v any/c]) boolean?]{
Returns @scheme[#t] if @scheme[v] is an input port, @scheme[#f] otherwise.}

@defproc[(output-port? [v any/c]) boolean?]{
Returns @scheme[#t] if @scheme[v] is an output port, @scheme[#f] otherwise.}

@defproc[(port? [v any/c]) boolean?]{
Returns @scheme[#t] if either @scheme[(input-port? v)] or
@scheme[(output-port? v)] is @scheme[#t], @scheme[#f] otherwise.}

@defproc[(close-input-port [in input-port?]) void?]{
Closes the input port @scheme[in]. For some kinds of ports, closing
the port releases lower-level resources, such as a file handle. If
the port is already closed, @scheme[close-input-port] has no effect.}

@defproc[(close-output-port [out output-port?]) void?]{
Closes the output port @scheme[out]. For some kinds of ports, closing
the port releases lower-level resources, such as a file handle. Also,
if the port is buffered, closing may first flush the port before
closing it, and this flushing process can block. If the port is
already closed, @scheme[close-output-port] has no effect.}

@defproc[(port-closed? [port port?]) boolean?]{
Returns @scheme[#t] if the input or output port @scheme[port] is
closed, @scheme[#f] otherwise.}

@defparam[current-output-port output-port]{A parameter.}

@defproc[(file-stream-port? [port port?]) boolean?]{
Returns @scheme[#t] if the given port is a file-stream port (see
@secref["mz:file-ports"], @scheme[#f] otherwise.}

@defproc[(terminal-port? [port port?]) boolean?]{
Returns @scheme[#t] if the given port is attached to an interactive
terminal, @scheme[#f] otherwise.}


