#lang scribble/doc
@(require "mz.rkt")

@title{Byte and String Output}

@defproc[(write-char [char character?] [out output-port? (current-output-port)])
         void?]{

Writes a single character to @racket[out]; more precisely, the bytes
that are the UTF-8 encoding of @racket[char] are written to
@racket[out].}

@defproc[(write-byte [byte any/c] [out output-port? (current-output-port)]) 
         void?]{

Writes a single byte to @racket[out].}

@defproc[(newline [out output-port? (current-output-port)])
         void?]{

The same as @racket[(write-char #\newline out)].}

@defproc[(write-string [str string?]
                       [out output-port? (current-output-port)]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos exact-nonnegative-integer? (string-length str)])
         exact-nonnegative-integer?]{

Writes characters to @racket[out] from @racket[str] starting from
index @racket[start-pos] (inclusive) up to @racket[end-pos]
(exclusive). Like @racket[substring], the @exnraise[exn:fail:contract]
if @racket[start-pos] or @racket[end-pos] is out-of-range for
@racket[str].

The result is the number of characters written to @racket[out], which
is always @racket[(- end-pos start-pos)].}

@defproc[(write-bytes [bstr bytes?]
                      [out output-port? (current-output-port)]
                      [start-pos exact-nonnegative-integer? 0]
                      [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         exact-nonnegative-integer?]{

Like @racket[write-string], but writes bytes instead of characters.}

@defproc[(write-bytes-avail [bstr bytes?]
                            [out output-port? (current-output-port)]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         exact-nonnegative-integer?]{

Like @racket[write-bytes], but returns without blocking after writing
as many bytes as it can immediately flush. It blocks only if no bytes
can be flushed immediately. The result is the number of bytes written
and flushed to @racket[out]; if @racket[start-pos] is the same as
@racket[end-pos], then the result can be @racket[0] (indicating a
successful flush of any buffered data), otherwise the result is between
@racket[1] and @racket[(- end-pos
start-pos)], inclusive.

The @racket[write-bytes-avail] procedure never drops bytes; if
@racket[write-bytes-avail] successfully writes some bytes and then
encounters an error, it suppresses the error and returns the number of
written bytes.  (The error will be triggered by future writes.) If an
error is encountered before any bytes have been written, an exception
is raised.}

@defproc[(write-bytes-avail* [bstr bytes?]
                             [out output-port? (current-output-port)]
                             [start-pos exact-nonnegative-integer? 0]
                             [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-nonnegative-integer? #f)]{

Like @racket[write-bytes-avail], but never blocks, returns @racket[#f]
if the port contains buffered data that cannot be written immediately,
and returns @racket[0] if the port's internal buffer (if any) is
flushed but no additional bytes can be written immediately.}

@defproc[(write-bytes-avail/enable-break [bstr bytes?]
                                         [out output-port? (current-output-port)]
                                         [start-pos exact-nonnegative-integer? 0]
                                         [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         exact-nonnegative-integer?]{

Like @racket[write-bytes-avail], except that breaks are enabled during
the write. The procedure provides a guarantee about the interaction of
writing and breaks: if breaking is disabled when
@racket[write-bytes-avail/enable-break] is called, and if the
@racket[exn:break] exception is raised as a result of the call, then
no bytes will have been written to @racket[out].  See also
@secref["breakhandler"].}

@defproc[(write-special [v any/c] [out output-port? (current-output-port)]) boolean?]{

Writes @racket[v] directly to @racket[out] if the port supports
special writes, or raises @racket[exn:fail:contract] if the port does
not support special write. The result is always @racket[#t],
indicating that the write succeeded.}

@defproc[(write-special-avail* [v any/c] [out output-port? (current-output-port)]) boolean?]{

Like @racket[write-special], but without blocking. If @racket[v]
cannot be written immediately, the result is @racket[#f] without
writing @racket[v], otherwise the result is @racket[#t] and @racket[v]
is written.}

@defproc[(write-bytes-avail-evt [bstr bytes?]
                                [out output-port? (current-output-port)]
                                [start-pos exact-nonnegative-integer? 0]
                                [end-pos exact-nonnegative-integer? (bytes-length bstr)]) 
         evt?]{

Similar to @racket[write-bytes-avail], but instead of writing bytes
immediately, it returns a synchronizable event (see
@secref["sync"]).  The @racket[out] must support atomic writes, as
indicated by @racket[port-writes-atomic?].

Synchronizing on the object starts a write from @racket[bstr], and the
event becomes ready when bytes are written (unbuffered) to the
port. If @racket[start-pos] and @racket[end-pos] are the same, then
the synchronization result is @racket[0] when the port's internal
buffer (if any) is flushed, otherwise the result is a positive exact
integer. If the event is not selected in a synchronization, then no
bytes will have been written to @racket[out].}

@defproc[(write-special-evt [v any/c] [out output-port? (current-output-port)]) evt?]{

Similar to @racket[write-special], but instead of writing the special
value immediately, it returns a synchronizable event (see
@secref["sync"]).  The @racket[out] must support atomic writes, as
indicated by @racket[port-writes-atomic?].

Synchronizing on the object starts a write of the special value, and
the event becomes ready when the value is written (unbuffered) to the
port. If the event is not selected in a synchronization, then no value
will have been written to @racket[out].}

@defproc[(port-writes-atomic? [out output-port?]) boolean?]{

Returns @racket[#t] if @racket[write-bytes-avail/enable-break] can
provide an exclusive-or guarantee (break or write, but not both) for
@racket[out], and if the port can be used with procedures like
@racket[write-bytes-avail-evt]. Racket's file-stream ports, pipes,
string ports, and TCP ports all support atomic writes; ports created
with @racket[make-output-port] (see @secref["customport"]) may
support atomic writes.}

@defproc[(port-writes-special? [out output-port?]) boolean?]{

Returns @racket[#t] if procedures like @racket[write-special] can
write arbitrary values to the port. Racket's file-stream ports,
pipes, string ports, and TCP ports all reject special values, but
ports created with @racket[make-output-port] (see
@secref["customport"]) may support them.}
