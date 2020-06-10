#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "ports" #:style 'toc]{Ports}

@deftech{Ports} produce and/or consume bytes. An @deftech{input port}
produces bytes, while an @deftech{output port} consumes bytes (and
some ports are both input ports and output ports). When an input port
is provided to a character-based operation, the bytes are decoded to a
character, and character-based output operations similarly encode the
character to bytes; see @secref["encodings"]. In addition to bytes and
characters encoded as bytes, some ports can produce and/or consume
arbitrary values as @deftech{special} results.

When a port corresponds to a file, network connection, or some other
system resource, it must be explicitly closed via
@racket[close-input-port] or @racket[close-output-port] (or indirectly
via @racket[custodian-shutdown-all]) to release low-level resources
associated with the port. For any kind of port, after it is closed,
attempting to read from or write to the port raises @racket[exn:fail].

Data produced by a @tech{input port} can be read or @deftech[#:key
"peek"]{peeked}. When data is read, it is considered consumed and
removed from the port's stream. When data is @tech{peek}ed, it remains
in the port's stream to be returned again by the next read or
@tech{peek}. Previously peeked data can be @deftech[#:key
"commit"]{committed}, which causes the data to be removed from the
port as for a read in a way that can be synchronized with other
attempts to @tech{peek} or read through a @tech{synchronizable
event}. Both read and @tech{peek} operations are normally blocking, in
the sense that the read or @tech{peek} operation does not complete
until data is available from the port; non-blocking variants of read
and @tech{peek} operations are also available.

The global variable @racket[eof] is bound to the end-of-file value,
and @racket[eof-object?] returns @racket[#t] only when applied to this
value. Reading from a port produces an end-of-file result when the
port has no more data, but some ports may also return end-of-file
mid-stream. For example, a port connected to a Unix terminal returns
an end-of-file when the user types control-D; if the user provides
more input, the port returns additional bytes after the end-of-file.

Every port has a name, as reported by @racket[object-name]. The name
can be any value, and it is used mostly for error-reporting
purposes. The @racket[read-syntax] procedure uses the name of an input
port as the default source location for the @tech{syntax objects} that
it produces.

A port can be used as a @tech{synchronizable event}. An input port is
@tech{ready for synchronization} when @racket[read-byte] would not
block, and an output port is @tech{ready for synchronization} when
@racket[write-bytes-avail] would not block or when the port contains
buffered characters and @racket[write-bytes-avail*] can flush part of
the buffer (although @racket[write-bytes-avail] might block). A value
that can act as both an input port and an output port acts as an input
port for a @tech{synchronizable event}. @ResultItself{port}.

@;------------------------------------------------------------------------

@local-table-of-contents[]

@include-section["encodings.scrbl"]
@include-section["port-procs.scrbl"]
@include-section["port-buffers.scrbl"]
@include-section["port-line-counting.scrbl"]
@include-section["file-ports.scrbl"]
@include-section["string-ports.scrbl"]
@include-section["pipes.scrbl"]
@include-section["prop-port.scrbl"]
@include-section["custom-ports.scrbl"]
@include-section["port-lib.scrbl"]
