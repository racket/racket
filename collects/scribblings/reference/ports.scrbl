#lang scribble/doc
@(require "mz.ss")

@title[#:tag "ports" #:style 'toc]{Ports}

@deftech{Ports} produce and consume bytes. When a port is provided to
a character-based operation, the port's bytes are decoded; see
@secref["encodings"].

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
