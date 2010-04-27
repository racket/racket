#lang scribble/doc
@(require "mz.ss")

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
column locations for a port are used by @racket[read-syntax] and
@racket[read-honu-syntax]. Position and line locations are numbered
from @math{1}; column locations are numbered from @math{0}.

When counting lines, Racket treats linefeed, return, and
return-linefeed combinations as a line terminator and as a single
position (on all platforms). Each tab advances the column count to one
before the next multiple of @math{8}. When a sequence of bytes in the
range 128 to 253 forms a UTF-8 encoding of a character, the
position/column is incremented is incremented once for each byte, and
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

@;------------------------------------------------------------------------

@defproc[(port-count-lines! [port port?]) void?]{

Turns on line and column counting for a port. Counting can be turned
on at any time, though generally it is turned on before any data is
read from or written to a port. When a port is created, if the value
of the @racket[port-count-lines-enabled] parameter is true, then line
counting is automatically enabled for the port. Line counting cannot
be disabled for a port after it is enabled.}

@defproc[(port-next-location [port port?]) 
         (values (or/c exact-positive-integer? #f)
                 (or/c exact-nonnegative-integer? #f)
                 (or/c exact-positive-integer? #f))]{

Returns three values: an integer or @racket[#f] for the line number of
the next read/written item, an integer or @racket[#f] for the next
item's column, and an integer or @racket[#f] for the next item's
position. The next column and position normally increases as bytes are
read from or written to the port, but if line/character counting is
enabled for @racket[port], the column and position results can
decrease after reading or writing a byte that ends a UTF-8 encoding
sequence.}

@defboolparam[port-count-lines-enabled on?]{

A parameter that determines whether line counting is enabled
automatically for newly created ports. The default value is
@racket[#f].}

