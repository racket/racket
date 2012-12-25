#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "ports" #:style 'toc]{Ports}

@deftech{Ports} produce and consume bytes. When a port is provided to
a character-based operation, the port's bytes are decoded; see
@secref["encodings"].

When a port corresponds to a file, network connection, or some other
system resource, it must be explicitly closed via
@racket[close-input-port] or @racket[close-output-port] (or indirectly
via @racket[custodian-shutdown-all]) to release low-level resources
associated with the port. For any kind of port, after it is closed,
attempting to read from or write to the port raises @racket[exn:fail].

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
