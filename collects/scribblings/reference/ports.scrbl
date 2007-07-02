#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "mz:ports" #:style 'toc]{Ports}

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

The global variable @scheme[eof] is bound to the end-of-file value,
and @scheme[eof-object?] returns @scheme[#t] only when applied to this
value. Reading from a port produces an end-of-file result when the
port has no more data, but some ports may also return end-of-file
mid-stream. For example, a port connected to a Unix terminal returns
an end-of-file when the user types control-D; if the user provides
more input, the port returns additional bytes after the end-of-file.

Every port has a name, as reported by @scheme[object-name]. The name
can be any value, and it is used mostly for error-reporting
purposes. The @scheme[read-syntax] procedure uses the name of an input
port as the default source location for the @tech{syntax objects} that
it produces.

@;------------------------------------------------------------------------

@local-table-of-contents[]

@include-section["port-procs.scrbl"]
@include-section["port-buffers.scrbl"]
@include-section["port-line-counting.scrbl"]
@include-section["file-ports.scrbl"]
@include-section["string-ports.scrbl"]
@include-section["pipes.scrbl"]
@include-section["prop-port.scrbl"]
@include-section["custom-ports.scrbl"]
