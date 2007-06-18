#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "ports"]{Input and Output Ports}

A @defterm{port} encapsulates an I/O stream, normally for just one
direction. An @defterm{input port} reads from a stream, and an
@defterm{output port} writes to a string.

For many procedures that accept a port argument, the argument is
optional, and it defaults to either the @defterm{current input port}
or @defterm{current output port}. For @exec{mzscheme}, the current
ports are initialized to the process's stdin and stdout. The
@scheme[current-input-port] and @scheme[current-output-port]
procedures, whien called with no arguments, return the current output
and input port, respectively.

@examples[
(display "hello world\n")
(display "hello world\n" (current-output-port))
]

Ports are created by various procedures that specific to the different
kinds of streams. For example, @scheme[open-input-file] creates an
input port for reading from a file. Procedures like
@scheme[with-input-from-file] both create a port and install it as the
current port while calling a given body procedure.

See @secref["io"] for information about using ports.
