#lang scribble/doc
@(require "mz.ss")

@title{Byte and String Output}

@defproc[(write-char [char character?][out output-port? (current-output-port)])
         void?]{

Writes a single character to @scheme[out]; more precisely, the bytes
that are the UTF-8 encoding of @scheme[char] are written to
@scheme[out].}

@defproc[(write-byte [byte any/c][out output-port? (current-output-port)]) 
         void?]{

Writes a single byte to @scheme[out].}

@defproc[(newline [out output-port? (current-output-port)])
         void?]{

The same as @scheme[(write-char #\newline out)].}

@defproc[(write-string [str string?]
                       [out output-port? (current-output-port)]
                       [start-pos nonnegative-exact-integer? 0]
                       [end-pos nonnegative-exact-integer? (string-length str)])
         void?]{

Writes characters to @scheme[out] from @scheme[str] starting from
index @scheme[start-pos] (inclusive) up to @scheme[end-pos]
(exclusive). Like @scheme[substring], the @exnraise[exn:fail:contract]
if @scheme[start-pos] or @scheme[end-pos] is out-of-range for
@scheme[str].

The result is the number of characters written to @scheme[out], which
is always @scheme[(- end-pos start-pos)].}

@defproc[(write-bytes [bstr bytes?]
                      [out output-port? (current-output-port)]
                      [start-pos nonnegative-exact-integer? 0]
                      [end-pos nonnegative-exact-integer? (bytes-length bstr)])
         void?]{

Like @scheme[write-string], but writes bytes instead of characters.}

@defproc[(write-bytes-avail [bstr bytes?]
                            [out output-port? (current-output-port)]
                            [start-pos nonnegative-exact-integer? 0]
                            [end-pos nonnegative-exact-integer? (bytes-length bstr)])
         nonnegative-exact-integer?]{

Like @scheme[write-bytes], but returns without blocking after writing
as many bytes as it can immediately flush. It blocks only if no bytes
can be flushed immediately. The result is the number of bytes written
and flushed to @scheme[out]; if @scheme[start-pos] is the same as
@scheme[end-pos], then the result can be @scheme[0] (indicating a
successful flush of any buffered data), otherwise the result is at
least @scheme[1] but possibly less than @scheme[(- end-pos
start-pos)].

The @scheme[write-bytes-avail] procedure never drops bytes; if
@scheme[write-bytes-avail] successfully writes some bytes and then
encounters an error, it suppresses the error and returns the number of
written bytes.  (The error will be triggered by future writes.) If an
error is encountered before any bytes have been written, an exception
is raised.}

@defproc[(write-bytes-avail* [bstr bytes?]
                             [out output-port? (current-output-port)]
                             [start-pos nonnegative-exact-integer? 0]
                             [end-pos nonnegative-exact-integer? (bytes-length bstr)])
         (or/c nonnegative-exact-integer? false/c)]{

Like @scheme[write-bytes-avail], but never blocks, returns @scheme[#f]
if the port contains buffered data that cannot be written immediately,
and returns @scheme[0] if the port's internal buffer (if any) is
flushed but no additional bytes can be written immediately.}

@defproc[(write-bytes-avail/enable-break [bstr bytes?]
                                         [out output-port? (current-output-port)]
                                         [start-pos nonnegative-exact-integer? 0]
                                         [end-pos nonnegative-exact-integer? (bytes-length bstr)])
         nonnegative-exact-integer?]{

Like @scheme[write-bytes-avail], except that breaks are enabled during
the write. The procedure provides a guarantee about the interaction of
writing and breaks: if breaking is disabled when
@scheme[write-bytes-avail/enable-break] is called, and if the
@scheme[exn:break] exception is raised as a result of the call, then
no bytes will have been written to @scheme[out].  See also
@secref["breakhandler"].}

@defproc[(write-special [v any/c][out output-port? (current-output-port)]) boolean?]{

Writes @scheme[v] directly to @scheme[out] if the port supports
special writes, or raises @scheme[exn:fail:contract] if the port does
not support special write. The result is always @scheme[#t],
indicating that the write succeeded.}

@defproc[(write-special-avail* [v any/c][out output-port? (current-output-port)]) boolean?]{

Like @scheme[write-special], but without blocking. If @scheme[v]
cannot be written immediately, the result is @scheme[#f] without
writing @scheme[v], otherwise the result is @scheme[#t] and @scheme[v]
is written.}

@defproc[(write-bytes-avail-evt [bstr bytes?]
                                [out output-port? (current-output-port)]
                                [start-pos nonnegative-exact-integer? 0]
                                [end-pos nonnegative-exact-integer? (bytes-length bstr)]) 
         evt?]{

Similar to @scheme[write-bytes-avail], but instead of writing bytes
immediately, it returns a synchronizable event (see
@secref["sync"]).  The @scheme[out] must support atomic writes, as
indicated by @scheme[port-writes-atomic?].

Synchronizing on the object starts a write from @scheme[bstr], and the
event becomes ready when bytes are written (unbuffered) to the
port. If @scheme[start-pos] and @scheme[end-pos] are the same, then
the synchronization result is @scheme[0] when the port's internal
buffer (if any) is flushed, otherwise the result is a positive exact
integer. If the event is not selected in a synchronization, then no
bytes will have been written to @scheme[out].}

@defproc[(write-special-evt [v any/c][out output-port? (current-output-port)]) evt?]{

Similar to @scheme[write-special], but instead of writing the special
value immediately, it returns a synchronizable event (see
@secref["sync"]).  The @scheme[out] must support atomic writes, as
indicated by @scheme[port-writes-atomic?].

Synchronizing on the object starts a write of the special value, and
the event becomes ready when the value is written (unbuffered) to the
port. If the event is not selected in a synchronization, then no value
will have been written to @scheme[out].}

@defproc[(port-writes-atomic? [out output-port?]) boolean?]{

Returns @scheme[#t] if @scheme[write-bytes-avail/enable-break] can
provide an exclusive-or guarantee (break or write, but not both) for
@scheme[out], and if the port can be used with procedures like
@scheme[write-bytes-avail-evt]. Scheme's file-stream ports, pipes,
string ports, and TCP ports all support atomic writes; ports created
with @scheme[make-output-port] (see @secref["customport"]) may
support atomic writes.}

@defproc[(port-writes-special? [out output-port?]) boolean?]{

Returns @scheme[#t] if procedures like @scheme[write-special] can
write arbitrary values to the port. Scheme's file-stream ports,
pipes, string ports, and TCP ports all reject special values, but
ports created with @scheme[make-output-port] (see
@secref["customport"]) may support them.}
