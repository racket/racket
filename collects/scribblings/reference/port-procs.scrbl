#lang scribble/doc
@(require "mz.ss")

@title[#:tag "port-ops"]{Managing Ports}

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

@defparam[current-input-port in input-port?]{A parameter that
determines a default input port for many operations, such as
@scheme[read].}

@defparam[current-output-port out output-port?]{A parameter that
determines a default output port for many operations, such as
@scheme[write].}

@defparam[current-error-port out output-port?]{A parameter that
determines an output port that is typically used for errors and
logging. For example, the default error display handler writes to this
port.}

@defproc[(file-stream-port? [port port?]) boolean?]{
Returns @scheme[#t] if the given port is a @tech{file-stream port} (see
@secref["file-ports"]), @scheme[#f] otherwise.}

@defproc[(terminal-port? [port port?]) boolean?]{
Returns @scheme[#t] if the given port is attached to an interactive
terminal, @scheme[#f] otherwise.}

@defthing[eof eof-object?]{A value (distinct from all other values)
that represents an end-of-file.}

@defproc[(eof-object? [a any/c]) boolean?]{Returns @scheme[#t] is
@scheme[v] is @scheme[eof], @scheme[#f] otherwise.}
