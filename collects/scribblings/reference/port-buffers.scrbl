#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "port-buffers"]{Port Buffers and Positions}

Some ports---especially those that read from and write to files---are
internally buffered:

@itemize[

 @item{An input port is typically block-buffered by default, which
       means that on any read, the buffer is filled with
       immediately-available bytes to speed up future reads. Thus, if
       a file is modified between a pair of reads to the file, the
       second read can produce stale data. Calling
       @racket[file-position] to set an input port's file position
       flushes its buffer.}

 @item{An output port is typically block-buffered by default, though
       a terminal output port is line-buffered, and the initial error
       output port is unbuffered.  An output buffer is filled with a
       sequence of written bytes to be committed as a group, either
       when the buffer is full (in block mode), when a newline is
       written (in line mode), when the port is closed via
       @racket[close-output-port], or when a flush is explicitly
       requested via a procedure like @racket[flush-output].}

]

If a port supports buffering, its buffer mode can be changed via
@racket[file-stream-buffer-mode] (even if the port is not a
@tech{file-stream port}).

For an input port, peeking always places peeked bytes into the port's
buffer, even when the port's buffer mode is @racket['none];
furthermore, on some platforms, testing the port for input (via
@racket[char-ready?] or @racket[sync]) may be implemented with a
peek. If an input port's buffer mode is @racket['none], then at most
one byte is read for @racket[read-bytes-avail!*],
@racket[read-bytes-avail!], @racket[peek-bytes-avail!*], or
@racket[peek-bytes-avail!]; if any bytes are buffered in the port
(e.g., to satisfy a previous peek), the procedures may access multiple
buffered bytes, but no further bytes are read.

In addition, the initial current output and error ports are
automatically flushed when @racket[read], @racket[read-line],
@racket[read-bytes], @racket[read-string], etc., are performed on the
initial standard input port; more precisely, flushing is performed by
the default port read handler (see @racket[port-read-handler]).

@defproc[(flush-output [out output-port? (current-output-port)]) void?]{

@index['("ports" "flushing")]{Forces} all buffered data in the given
output port to be physically written. Only @tech{file-stream ports},
TCP ports, and custom ports (see @secref["customport"]) use
buffers; when called on a port without a buffer, @racket[flush-output]
has no effect.}

@defproc*[([(file-stream-buffer-mode [port port?]) (or/c 'none 'line 'block #f)]
           [(file-stream-buffer-mode [port port?] [mode (or/c 'none 'line 'block)]) void?])]{

Gets or sets the buffer mode for @racket[port], if
possible. @tech{File-stream ports} support setting the buffer mode,
TCP ports (see @secref["networking"]) support setting and getting
the buffer mode, and custom ports (see @secref["customport"]) may
support getting and setting buffer modes.

If @racket[mode] is provided, it must be one of
@indexed-racket['none], @indexed-racket['line] (output only), or
@indexed-racket['block], and the port's buffering is set
accordingly. If the port does not support setting the mode, the
@exnraise[exn:fail].

If @racket[mode] is not provided, the current mode is returned, or
@racket[#f] is returned if the mode cannot be determined. If
@racket[port] is an input port and @racket[mode] is
@racket['line], the @exnraise[exn:fail:contract].}

@defproc*[([(file-position [port port?]) exact-nonnegative-integer?]
           [(file-position [port port?] [pos (or/c exact-nonnegative-integer? eof-object?)]) void?])]{

Returns or sets the current read/write position of @racket[port]. 

Calling @racket[file-position] without a position on a port other
than a @tech{file-stream port} or @tech{string port}
returns the number of bytes that have
been read from that port if the position is known (see
@secref["linecol"]), otherwise the @exnraise[exn:fail:filesystem].

For @tech{file-stream ports} and @tech{string ports}, the position-setting
variant sets the read/write position to @racket[pos] relative to the
beginning of the file or (byte) string if @racket[pos] is a number, or to the
current end of the file or (byte) string if @racket[pos] is @racket[eof]. In
position-setting mode, @racket[file-position] raises the
@racket[exn:fail:contract] exception for port kinds other than
@tech{file-stream ports} and @tech{string ports}. Furthermore, not all @tech{file-stream
ports} support setting the position; if @racket[file-position] is
called with a position argument on such a @tech{file-stream port}, the
@exnraise[exn:fail:filesystem].

When @racket[file-position] sets the position @racket[pos] beyond the
current size of an output file or (byte) string, the file/string is enlarged
to size @racket[pos] and the new region is filled with @racket[0]
bytes.  If @racket[pos] is beyond the end of an input file or (byte) string,
then reading thereafter returns @racket[eof] without changing the
port's position.

When changing the file position for an output port, the port is first
flushed if its buffer is not empty. Similarly, setting the position
for an input port clears the port's buffer (even if the new position
is the same as the old position). However, although input and output
ports produced by @racket[open-input-output-file] share the file
position, setting the position via one port does not flush the other
port's buffer.}

@defproc[(file-position* [port port?]) (or/c exact-nonnegative-integer? #f)]{

Like @racket[file-position] on a single argument, but returns
@racket[#f] if the position is not known.}

@defproc[(file-truncate [port (and/c output-port? file-stream-port?)]
                        [size exact-nonnegative-integer?]) 
         void?]{

Sets the size of the file written by @racket[port] to @racket[size],
assuming that the port is associated to a file whose size can be set.

The new file size can be either larger or smaller than its current
size, but ``truncate'' in this function's name reflects that it is
normally used to decrease the size of a file, since writing to a file
or using @racket[file-position] can extend a file's size.}
