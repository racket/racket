#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "linecol"]{Counting Positions, Lines, and Columns}

@section-index["line numbers"]
@section-index["column numbers"]
@section-index["port positions"]

By default, Racket keeps track of the @deftech{position} in a port as
the number of bytes that have been read from or written to any port
(independent of the read/write position, which is accessed or changed
with @racket[file-position]). Optionally, however, Racket can track
the position in terms of characters (after UTF-8 decoding), instead of
bytes, and it can track @deftech{line locations} and @deftech{column
locations}; this optional tracking must be specifically enabled for a
port via @racket[port-count-lines!] or the
@racket[port-count-lines-enabled] parameter. Position, line, and
column locations for a port are used by @racket[read-syntax]. 
Position and line locations are numbered
from @math{1}; column locations are numbered from @math{0}.

When counting lines, Racket treats linefeed, return, and
return-linefeed combinations as a line terminator and as a single
position (on all platforms). Each tab advances the column count to one
before the next multiple of @math{8}. When a sequence of bytes in the
range 128 to 253 forms a UTF-8 encoding of a character, the
position/column is incremented once for each byte, and
then decremented appropriately when a complete encoding sequence is
discovered. See also @secref["ports"] for more information on UTF-8
decoding for ports.

A position is known for any port as long as its value can be expressed
as a fixnum (which is more than enough tracking for realistic
applications in, say, syntax-error reporting).  If the position for a
port exceeds the value of the largest fixnum, then the position for
the port becomes unknown, and line and column tacking is disabled.
Return-linefeed combinations are treated as a single character
position only when line and column counting is enabled.

@tech{Custom ports} can define their own counting functions, which are
not subject to the rules above, except that the counting functions are
invoked only when tracking is specifically enabled with
@racket[port-count-lines!].

@;------------------------------------------------------------------------

@defproc[(port-count-lines! [port port?]) void?]{

Turns on line and column counting for a port. Counting can be turned
on at any time, though generally it is turned on before any data is
read from or written to a port. At the point that line counting is
turned on, @racket[port-next-location] typically starts reporting as
its last result (one more than) the number of characters read since
line counting was enabled, instead of (one more than) bytes read since
the port was opened.

When a port is created, if the value of the
@racket[port-count-lines-enabled] parameter is true, then line
counting is automatically enabled for the port. Line counting cannot
be disabled for a port after it is enabled.}


@defproc[(port-next-location [port port?]) 
         (values (or/c exact-positive-integer? #f)
                 (or/c exact-nonnegative-integer? #f)
                 (or/c exact-positive-integer? #f))]{

Returns three values: an integer or @racket[#f] for the line number of
the next read/written item, an integer or @racket[#f] for the next
item's column, and an integer or @racket[#f] for the next item's
position. The next column and position normally increase as bytes are
read from or written to the port, but if line/character counting is
enabled for @racket[port], the column and position results can
decrease after reading or writing a byte that ends a UTF-8 encoding
sequence.

If line counting is not enabled for a port, than the first two results
are @racket[#f], and the last result is one more than the number of
bytes read so far. At the point when line counting is enabled, the
first two results typically become non-@racket[#f], and last result
starts reporting characters instead of bytes, typically starting from
the point when line counting is enabled.

Even with line counting enabled, a port may return @racket[#f] values
if it somehow cannot keep track of lines, columns, or positions.}


@defproc[(set-port-next-location! [port port?]
                                  [line (or/c exact-positive-integer? #f)]
                                  [column (or/c exact-nonnegative-integer? #f)]
                                  [position (or/c exact-positive-integer? #f)])
         void?]{

Sets the next line, column, and position for @racket[port]. If line
counting has not been enabled for @racket[port] or if @racket[port] is
a @tech{custom port} that defines its own counting function, then 
@racket[set-port-next-location!] has no effect.}


@defboolparam[port-count-lines-enabled on?]{

A @tech{parameter} that determines whether line counting is enabled
automatically for newly created ports. The default value is
@racket[#f].}
