#lang scribble/doc
@(require "mz.ss"
          (for-label racket/port))

@title[#:tag "port-lib"]{More Port Constructors, Procedures, and Events}

@note-lib[racket/port]

@; ----------------------------------------------------------------------

@section{Port String and List Conversions}

@defproc[(port->list [r (input-port? . -> . any/c) read] [in input-port? (current-input-port)])
         (listof any/c)]{
Returns a list whose elements are produced by calling @scheme[r]
on @scheme[in] until it produces @scheme[eof].}

@defproc[(port->string [in input-port? (current-input-port)]) string?]{

Reads all characters from @scheme[in] and returns them as a string.}

@defproc[(port->bytes [in input-port? (current-input-port)]) bytes?]{

Reads all bytes from @scheme[in] and returns them as a @tech{byte string}.}

@defproc[(port->lines [in input-port? (current-input-port)]
                      [#:line-mode line-mode (or/c 'linefeed 'return 'return-linefeed 'any 'any-one) 'any])
         (listof string?)]{

Read all characters from @scheme[in], breaking them into lines. The
@scheme[line-mode] argument is the same as the second argument to
@scheme[read-line], but the default is @scheme['any] instead of
@scheme['linefeed].}

@defproc[(port->bytes-lines [in input-port? (current-input-port)]
                            [#:line-mode line-mode (or/c 'linefeed 'return 'return-linefeed 'any 'any-one) 'any])
         (listof bytes?)]{

Like @scheme[port->lines], but reading bytes and collecting them into
lines like @scheme[read-bytes-line].}

@defproc[(display-lines [lst list?]
                        [out output-port? (current-output-port)]
                        [#:separator separator any/c #"\n"])
         void?]{

Use @scheme[display] to each each element of @scheme[lst] to @scheme[out], adding
@scheme[separator] after each element.}

@defproc[(call-with-output-string [proc (output-port? . -> . any)]) string?]{

Calls @scheme[proc] with an output port that accumulates all output
into a string, and returns the string.

The port passed to @scheme[proc] is like the one created by
@scheme[open-output-string], except that it is wrapped via
@scheme[dup-output-port], so that @scheme[proc] cannot access the
port's content using @scheme[get-output-string]. If control jumps back
into @scheme[proc], the port continues to accumulate new data, and
@scheme[call-with-output-string] returns both the old data and newly
accumulated data.}

@defproc[(call-with-output-bytes [proc (output-port? . -> . any)]) bytes?]{

Like @scheme[call-with-output-string], but returns accumulated results
in a @tech{byte string} instead of a string. Furthermore, the port's
content is emptied when @scheme[call-with-output-bytes] returns, so
that if control jumps back into @scheme[proc] and returns a second
time, only the newly accumulated bytes are returned.}

@defproc[(with-output-to-string [proc (-> any)]) string?]{

Equivalent to

@schemeblock[(call-with-output-string
              (lambda (p) (parameterize ([current-output-port p])
                            (proc))))]}

@defproc[(with-output-to-bytes [proc (-> any)]) bytes?]{

Equivalent to

@schemeblock[(call-with-output-bytes
              (lambda (p) (parameterize ([current-output-port p])
                            (proc))))]}

@defproc[(call-with-input-string [str string?][proc (input-port? . -> . any)]) any]{

Equivalent to @scheme[(proc (open-input-string str))].}

@defproc[(call-with-input-bytes [bstr bytes?][proc (input-port? . -> . any)]) any]{

Equivalent to @scheme[(proc (open-input-bytes bstr))].}

@defproc[(with-input-from-string [str string?][proc (-> any)]) any]{

Equivalent to

@schemeblock[(parameterize ([current-input-port (open-input-string str)])
               (proc))]}

@defproc[(with-input-from-bytes [bstr bytes?][proc (-> any)]) any]{

Equivalent to

@schemeblock[(parameterize ([current-input-port (open-input-bytes str)])
               (proc))]}


@; ----------------------------------------------------------------------

@section{Creating Ports}

@defproc[(input-port-append [close-at-eof? any/c][in input-port?] ...) input-port?]{

Takes any number of input ports and returns an input port. Reading
from the input port draws bytes (and special non-byte values) from the
given input ports in order. If @scheme[close-at-eof?] is true, then
each port is closed when an end-of-file is encountered from the port,
or when the result input port is closed. Otherwise, data not read from
the returned input port remains available for reading in its original
input port.

See also @scheme[merge-input], which interleaves data from multiple
input ports as it becomes available.}


@defproc[(make-input-port/read-to-peek 
          [name any/c]
          [read-in (bytes? 
                    . -> . (or/c exact-nonnegative-integer?
                                 eof-object?
                                 procedure?
                                 evt?))]
          [fast-peek (or/c #f
                           (bytes? exact-nonnegative-integer?
                            (bytes? exact-nonnegative-integer?
                             . -> . (or/c exact-nonnegative-integer?
                                          eof-object?
                                          procedure?
                                          evt?
                                          #f))
                            . -> . (or/c exact-nonnegative-integer?
                                         eof-object?
                                         procedure?
                                         evt?
                                         #f)))]
          [close (-> any)]
          [get-location (or/c 
                         (->
                          (values
                           (or/c exact-positive-integer? #f)
                           (or/c exact-nonnegative-integer? #f)
                           (or/c exact-positive-integer? #f)))
                         #f)
                        #f]
          [count-lines! (-> any) void]
          [init-position exact-positive-integer? 1]
          [buffer-mode (or/c (case-> ((or/c 'block 'none) . -> . any)
                                     (-> (or/c 'block 'none #f)))
                             #f)
                       #f]
          [buffering? any/c #f]
          [on-consume (or/c ((or/c exact-nonnegative-integer? eof-object? 
                                   procedure? evt?) 
                             . -> . any)
                            #f)
                      #f])
         input-port?]{

Similar to @scheme[make-input-port], but if the given @scheme[read-in]
returns an event, the event's value must be @scheme[0].  The resulting
port's peek operation is implemented automatically (in terms of
@scheme[read-in]) in a way that can handle special non-byte
values. The progress-event and commit operations are also implemented
automatically. The resulting port is thread-safe, but not kill-safe
(i.e., if a thread is terminated or suspended while using the port,
the port may become damaged).

The @scheme[read-in], @scheme[close], @scheme[get-lcoation],
@scheme[count-lines!], @scheme[init-position], and
@scheme[buffer-mode] procedures are the same as for
@scheme[make-input-port].

The @scheme[fast-peek] argument can be either @scheme[#f] or a
procedure of three arguments: a byte string to receive a peek, a skip
count, and a procedure of two arguments. The @scheme[fast-peek]
procedure can either implement the requested peek, or it can dispatch
to its third argument to implement the peek. The @scheme[fast-peek] is
not used when a peek request has an associated progress event.

The @scheme[buffering?] argument determines whether @scheme[read-in]
can be called to read more characters than are immediately demanded by
the user of the new port. If @scheme[buffer] mode is not @scheme[#f],
then @scheme[buffering?] determines the initial buffer mode, and
@scheme[buffering?] is enabled after a buffering change only if the
new mode is @scheme['block].

If @scheme[on-consumed] is not @scheme[#f], it is called when data is
read from the port, as opposed to merely peeked. The argument to
@scheme[on-consume] is the result value of the port's reading
procedure, so it can be an integer or any result from
@scheme[read-in].}


@defproc[(make-limited-input-port [in input-port?]
                                  [limit exact-nonnegative-integer?]
                                  [close-orig? any/c #t])
         input-port?]{

Returns a port whose content is drawn from @scheme[in], but where an
end-of-file is reported after @scheme[limit] bytes (and non-byte
special values) are read.  If @scheme[close-orig?] is true, then the
original port is closed if the returned port is closed.

Bytes are consumed from @scheme[in] only when they are consumed from
the returned port. In particular, peeking into the returned port peeks
into the original port.

If @scheme[in] is used directly while the resulting port is also used,
then the @scheme[limit] bytes provided by the port need not be
contiguous parts of the original port's stream.}



@defproc[(make-pipe-with-specials [limit exact-nonnegative-integer? #f]
                                  [in-name any/c 'pipe]
                                  [out-name any/c 'pipe]) 
         (values input-port? output-port?)]{

Returns two ports: an input port and an output port. The ports behave
like those returned by @scheme[make-pipe], except that the ports
support non-byte values written with procedures such as
@scheme[write-special] and read with procedures such as
@scheme[get-byte-or-special].

The @scheme[limit] argument determines the maximum capacity of the
pipe in bytes, but this limit is disabled if special values are
written to the pipe before @scheme[limit] is reached. The limit is
re-enabled after the special value is read from the pipe.

The optional @scheme[in-name] and @scheme[out-name] arguments
determine the names of the result ports.}


@defproc[(merge-input [a-in input-port?]
                      [b-in input-port?]
                      [buffer-limit (or/c exact-nonnegative-integer? #f) 4096])
         input-port?]{

Accepts two input ports and returns a new input port. The new port
merges the data from two original ports, so data can be read from the
new port whenever it is available from either original port. The data
from the original ports are interleaved. When an end-of-file has been
read from an original port, it no longer contributes characters to the
new port. After an end-of-file has been read from both original ports,
the new port returns end-of-file. Closing the merged port does not
close the original ports.

The optional @scheme[buffer-limit] argument limits the number of bytes
to be buffered from @scheme[a-in] and @scheme[b-in], so that the merge
process does not advance arbitrarily beyond the rate of consumption of
the merged data. A @scheme[#f] value disables the limit. As for
@scheme[make-pipe-with-specials], @scheme[buffer-limit] does not apply
when a special value is produced by one of the input ports before the
limit is reached.

See also @scheme[input-port-append], which concatenates input streams
instead of interleaving them.}


@defproc[(open-output-nowhere [name any/c 'nowhere][special-ok? any/c #t])
         output-port?]{

Creates and returns an output port that discards all output sent to it
(without blocking). The @scheme[name] argument is used as the port's
name. If the @scheme[special-ok?]  argument is true, then the
resulting port supports @scheme[write-special], otherwise it does not.}


@defproc[(peeking-input-port [in input-port?]
                             [name any/c (object-name in)]
                             [skip exact-nonnegative-integer? 0])
         input-port]{

Returns an input port whose content is determined by peeking into
@scheme[in]. In other words, the resulting port contains an internal
skip count, and each read of the port peeks into @scheme[in] with the
internal skip count, and then increments the skip count according to
the amount of data successfully peeked.

The optional @scheme[name] argument is the name of the resulting
port. The @scheme[skip] argument is the port initial skip count, and
it defaults to @scheme[0].}



@defproc[(reencode-input-port [in input-port?]
                              [encoding string?]
                              [error-bytes (or/c #f bytes?)]
                              [close? any/c #t]
                              [name any/c (object-name in)]
                              [convert-newlines? any/c #f]
                              [enc-error (string? input-port? . -> . any) 
                                         (lambda (msg port) (error ...))])
         input-port?]{

Produces an input port that draws bytes from @scheme[in], but converts
the byte stream using @scheme[(bytes-open-converter encoding-str
"UTF-8")]. In addition, if @scheme[convert-newlines?] is true, then
decoded sequences that correspond to UTF-8 encodings of @scheme["\r\n"],
@scheme["\r\x85"], @scheme["\r"], @scheme["\x85"], and @scheme["\u2028"]
are all converted to the UTF-8 encoding of @scheme["\n"].
 
If @scheme[error-bytes] is provided and not @scheme[#f], then the
given byte sequence is used in place of bytes from @scheme[in] that
trigger conversion errors.  Otherwise, if a conversion is encountered,
@scheme[enc-error] is called, which must raise an exception.

If @scheme[close?] is true, then closing the result input port also
closes @scheme[in]. The @scheme[name] argument is used as the name of
the result input port.

In non-buffered mode, the resulting input port attempts to draw bytes
from @scheme[in] only as needed to satisfy requests. Toward that end,
the input port assumes that at least @math{n} bytes must be read to
satisfy a request for @math{n} bytes. (This is true even if the port
has already drawn some bytes, as long as those bytes form an
incomplete encoding sequence.)}


@defproc[(reencode-output-port [out output-port?]
                               [encoding string?]
                               [error-bytes (or/c #f bytes?)]
                               [close? any/c #t]
                               [name any/c (object-name out)]
                               [newline-bytes (or/c #f bytes?) #f]
                               [enc-error (string? output-port? . -> . any) 
                                          (lambda (msg port) (error ...))])
         output-port?]{

Produces an output port that directs bytes to @scheme[out], but
converts its byte stream using @scheme[(bytes-open-converter "UTF-8"
encoding-str)]. In addition, if @scheme[newline-bytes] is not
@scheme[#f], then byets written to the port that are the UTF-8
encoding of @scheme["\n"] are first converted to
@scheme[newline-bytes] (before applying the convert from UTF-8 to
@scheme[encoding-str]).
 
If @scheme[error-bytes] is provided and not @scheme[#f], then the
given byte sequence is used in place of bytes send to the output port
that trigger conversion errors. Otherwise, @scheme[enc-error] is
called, which must raise an exception.

If @scheme[close?] is true, then closing the result output port also
closes @scheme[out]. The @scheme[name] argument is used as the name of
the result output port.

The resulting port supports buffering, and the initial buffer mode is
@scheme[(or (file-stream-buffer-mode out) 'block)]. In @scheme['block]
mode, the port's buffer is flushed only when it is full or a flush is
requested explicitly. In @scheme['line] mode, the buffer is flushed
whenever a newline or carriage-return byte is written to the port. In
@scheme['none] mode, the port's buffer is flushed after every write.
Implicit flushes for @scheme['line] or @scheme['none] leave bytes in
the buffer when they are part of an incomplete encoding sequence.

The resulting output port does not support atomic writes. An explicit
flush or special-write to the output port can hang if the most
recently written bytes form an incomplete encoding sequence.}


@defproc[(dup-input-port [in input-port?]
                         [close? any/c #f])
         input-port?]{

Returns an input port that draws directly from @scheme[in]. Closing
the resulting port closes @scheme[in] only if @scheme[close?] is
@scheme[#t].

The new port is initialized with the @tech{port read handler} of
@scheme[in], but setting the handler on the result port does not
affect reading directly from @scheme[in].}


@defproc[(dup-output-port [out output-port?]
                          [close? any/c #f])
         output-port?]{

Returns an output port that propagates data directly to
@scheme[out]. Closing the resulting port closes @scheme[out] only if
@scheme[close?] is @scheme[#t].

The new port is initialized with the @tech{port display handler} and
@tech{port write handler} of @scheme[out], but setting the handlers on
the result port does not affect writing directly to @scheme[out].}



@defproc[(relocate-input-port [in input-port?]
                              [line (or/c exact-positive-integer? #f)]
                              [column (or/c exact-nonnegative-integer? #f)]
                              [position exact-positive-integer?]
                              [close? any/c #t])
         input-port?]{

Produces an input port that is equivalent to @scheme[in] except in how
it reports location information. The resulting port's content starts
with the remaining content of @scheme[in], and it starts at the given
line, column, and position. A @scheme[#f] for the line or column means
that the line and column will always be reported as @scheme[#f].

The @scheme[line] and @scheme[column] values are used only if line
counting is enabled for @scheme[in] and for the resulting port,
typically through @scheme[port-count-lines!]. The @scheme[column]
value determines the column for the first line (i.e., the one numbered
@scheme[line]), and later lines start at column @scheme[0]. The given
@scheme[position] is used even if line counting is not enabled.

When line counting is on for the resulting port, reading from
@scheme[in] instead of the resulting port increments location reports
from the resulting port. Otherwise, the resulting port's position does
not increment when data is read from @scheme[in].

If @scheme[close?] is true, then closing the resulting port also
closes @scheme[in]. If @scheme[close?] is @scheme[#f], then closing
the resulting port does not close @scheme[in].}


@defproc[(relocate-output-port [out output-port?]
                               [line (or/c exact-positive-integer? #f)]
                               [column (or/c exact-nonnegative-integer? #f)]
                               [position exact-positive-integer?]
                               [close? any/c #t])
         output-port?]{

Like @scheme[relocate-input-port], but for output ports.}


@defproc[(transplant-input-port [in input-port?]
                                [get-location (or/c 
                                               (->
                                                (values
                                                 (or/c exact-positive-integer? #f)
                                                 (or/c exact-nonnegative-integer? #f)
                                                 (or/c exact-positive-integer? #f)))
                                               #f)]
                                [init-pos exact-positive-integer?]
                                [close? any/c #t]
                                [count-lines! (-> any) void])
          input-port?]{

Like @scheme[relocate-input-port], except that arbitrary position
information can be produced (when line counting is enabled) via
@scheme[get-location], which used as for @scheme[make-input-port]. If
@scheme[get-location] is @scheme[#f], then the port counts lines in
the usual way starting from @scheme[init-pos], independent of
locations reported by @scheme[in].

If @scheme[count-lines!] is supplied, it is called when line counting
is enabled for the resulting port. The default is @scheme[void].}

@defproc[(transplant-output-port [in input-port?]
                                 [get-location (or/c 
                                                (->
                                                 (values
                                                  (or/c exact-positive-integer? #f)
                                                  (or/c exact-nonnegative-integer? #f)
                                                  (or/c exact-positive-integer? #f)))
                                                #f)]
                                 [init-pos exact-positive-integer?]
                                 [close? any/c #t]
                                 [count-lines! (-> any) void])
          output-port?]{

Like @scheme[transplant-input-port], but for output ports.}

@; ----------------------------------------------------------------------

@section{Port Events}


@defproc[(eof-evt [in input-port?]) evt?]

Returns a @tech{synchronizable event} is that is ready when
@scheme[in] produces an @scheme[eof]. If @scheme[in] produces a
mid-stream @scheme[eof], the @scheme[eof] is consumed by the event
only if the event is chosen in a synchronization.}


@defproc[(read-bytes-evt [k exact-nonnegative-integer?][in input-port?]) 
         evt?]{

Returns a @tech{synchronizable event} is that is ready when @scheme[k]
bytes can be read from @scheme[in], or when an end-of-file is
encountered in @scheme[in]. If @scheme[k] is @scheme[0], then the
event is ready immediately with @scheme[""]. For non-zero @scheme[k],
if no bytes are available before an end-of-file, the event's result is
@scheme[eof]. Otherwise the event's result is a byte string of up to
@scheme[k] bytes, which contains as many bytes as are available (up to
@scheme[k]) before an available end-of-file. (The result is a byte
string on less than @scheme[k] bytes only when an end-of-file is
encountered.)

Bytes are read from the port if and only if the event is chosen in a
synchronization, and the returned bytes always represent contiguous
bytes in the port's stream.

The event can be synchronized multiple times---event
concurrently---and each synchronization corresponds to a distinct read
request.

The @scheme[in] must support progress events, and it must not produce
a special non-byte value during the read attempt.}


@defproc[(read-bytes!-evt [bstr (and/c bytes? (not/c immutable?))]
                          [in input-port?]) 
         evt?]

Like @scheme[read-bytes-evt], except that the read bytes are placed
into @scheme[bstr], and the number of bytes to read corresponds to
@scheme[(bytes-length bstr)]. The event's result is either
@scheme[eof] or the number of read bytes.

The @scheme[bstr] may be mutated any time after the first
synchronization attempt on the event. If the event is not synchronized
multiple times concurrently, @scheme[bstr-bytes] is never mutated by
the event after it is chosen in a synchronization (no matter how many
synchronization attempts preceded the choice). Thus, the event may be
sensibly used multiple times until a successful choice, but should not
be used in multiple concurrent synchronizations.}


@defproc[(read-bytes-avail!-evt [bstr (and/c bytes? (not/c immutable?))][in input-port?]) 
         evt?]{

Like @scheme[read-bytes!-evt], except that the event reads only as
many bytes as are immediately available, after at least one byte or
one @scheme[eof] becomes available.}


@defproc[(read-string-evt [k exact-nonnegative-integer?][in input-port?]) 
         evt?]{

Like @scheme[read-bytes-evt], but for character strings instead of
byte strings.}


@defproc[(read-string!-evt [str (and/c string? (not/c immutable?))]
                           [in input-port?]) 
         evt?]{

Like @scheme[read-bytes!-evt], but for a character string instead of
a byte string.}


@defproc[(read-line-evt [in input-port?]
                        [mode (or/c 'linefeed 'return 'return-linefeed 'any 'any-one)])
         evt?]{

Returns a @tech{synchronizable event} that is ready when a line of
characters or end-of-file can be read from @scheme[inport]. The
meaning of @scheme[mode] is the same as for @scheme[read-line]. The
event result is the read line of characters (not including the line
separator).

A line is read from the port if and only if the event is chosen in a
synchronization, and the returned line always represents contiguous
bytes in the port's stream.}


@defproc[(read-bytes-line-evt [in input-port?]
                              [mode (or/c 'linefeed 'return 'return-linefeed 'any 'any-one)])
         evt?]{
 
Like @scheme[read-line-evt], but returns a byte string instead of a
string.}

@defproc*[([(peek-bytes-evt [k exact-nonnegative-integer?][skip exact-nonnegative-integer?]
                            [progress evt?][in input-port?]) evt?]
           [(peek-bytes!-evt [bstr (and/c bytes? (not/c immutable?))][skip exact-nonnegative-integer?]
                             [progress (or/c evt? #f)][in input-port?]) evt?]
           [(peek-bytes-avail!-evt [bstr (and/c bytes? (not/c immutable?))][skip exact-nonnegative-integer?]
                                   [progress (or/c evt? #f)][in input-port?]) evt?]
           [(peek-string-evt [k exact-nonnegative-integer?][in input-port?]) evt?]
           [(peek-string!-evt [str (and/c string? (not/c immutable?))][in input-port?]) evt?])]{

Like the @scheme[read-...-evt] functions, but for peeking. The
@scheme[skip] argument indicates the number of bytes to skip, and
@scheme[progress] indicates an event that effectively cancels the peek
(so that the event never becomes ready). The @scheme[progress]
argument can be @scheme[#f], in which case the event is never
cancelled.}


@defproc[(regexp-match-evt [pattern (or/c string? bytes? regexp? byte-regexp?)]
                           [in input-port?]) any]

Returns a @tech{synchronizable event} that is ready when
@scheme[pattern] matches the stream of bytes/characters from
@scheme[in]; see also @scheme[regexp-match]. The event's value is the
result of the match, in the same form as the result of
@scheme[regexp-match].

If @scheme[pattern] does not require a start-of-stream match, then
bytes skipped to complete the match are read and discarded when the
event is chosen in a synchronization.

Bytes are read from the port if and only if the event is chosen in a
synchronization, and the returned match always represents contiguous
bytes in the port's stream. If not-yet-available bytes from the port
might contribute to the match, the event is not ready.  Similarly, if
@scheme[pattern] begins with a start-of-stream @litchar{^} and the
@scheme[pattern] does not initially match, then the event cannot
become ready until bytes have been read from the port.

The event can be synchronized multiple times---even concurrently---and
each synchronization corresponds to a distinct match request.

The @scheme[in] port must support progress events. If @scheme[in]
returns a special non-byte value during the match attempt, it is
treated like @scheme[eof].}

@; ----------------------------------------------------------------------

@section{Copying Streams}

@defproc[(convert-stream [from-encoding string?]
                         [in input-port?]
                         [from-encoding string?]
                         [out output-port?])
         void?]{

Reads data from @scheme[in], converts it using
@scheme[(bytes-open-converter from-encoding-string
to-encoding-string)] and writes the converted bytes to
@scheme[out]. The @scheme[convert-stream] procedure returns after
reaching @scheme[eof] in @scheme[in].

If opening the converter fails, the @exnraise[exn:fail]. Similarly, if
a conversion error occurs at any point while reading @scheme[in], then
@exnraise[exn:fail].}


@defproc[(copy-port [in input-port?][out output-port?] ...+) void?]{

Reads data from @scheme[in] and writes it back out to @scheme[out],
returning when @scheme[in] produces @scheme[eof].  The copy is
efficient, and it is without significant buffer delays (i.e., a byte
that becomes available on @scheme[in] is immediately transferred to
@scheme[out], even if future reads on @scheme[in] must block). If
@scheme[in] produces a special non-byte value, it is transferred to
@scheme[out] using @scheme[write-special].

This function is often called from a ``background'' thread to
continuously pump data from one stream to another.

If multiple @scheme[out]s are provided, case data from @scheme[in] is
written to every @scheme[out]. The different @scheme[out]s block
output to each other, because each block of data read from @scheme[in]
is written completely to one @scheme[out] before moving to the next
@scheme[out]. The @scheme[out]s are written in the provided order, so
non-blocking ports (e.g., to a file) should be placed first in the
argument list.}

