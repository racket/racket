#lang scribble/doc
@(require "mz.ss")

@title{Byte and String Input}

@defproc[(read-char [in input-port? (current-input-port)]) 
         (or/c character? eof-object?)]{

Reads a single character from @scheme[in]---which may involve reading
several bytes to UTF-8-decode them into a character (see
@secref["ports"]); a minimal number of bytes are read/peeked to
perform the decoding. If no bytes are available before an end-of-file,
then @scheme[eof] is returned.}


@defproc[(read-byte [in input-port? (current-input-port)]) 
         (or/c byte? eof-object?)]{

Reads a single byte from @scheme[in]. If no bytes are available before
an end-of-file, then @scheme[eof] is returned.}


@defproc[(read-line [in input-port? (current-input-port)]
                    [mode (or/c 'linefeed 'return 'return-linefeed 'any 'any-one) 'linefeed])
         (or/c string? eof-object?)]{

Returns a string containing the next line of bytes from @scheme[in].

Characters are read from @scheme[in] until a line separator or an
end-of-file is read. The line separator is not included in the result
string (but it is removed from the port's stream). If no characters
are read before an end-of-file is encountered, @scheme[eof] is
returned.

The @scheme[mode] argument determines the line separator(s). It
must be one of the following symbols:

 @itemize[

  @item{@indexed-scheme['linefeed] breaks lines on linefeed characters.}

  @item{@indexed-scheme['return] breaks lines on return characters.}

  @item{@indexed-scheme['return-linefeed] breaks lines on
  return-linefeed combinations. If a return character is not followed
  by a linefeed character, it is included in the result string;
  similarly, a linefeed that is not preceded by a return is included
  in the result string.}

  @item{@indexed-scheme['any] breaks lines on any of a return
  character, linefeed character, or return-linefeed combination. If a
  return character is followed by a linefeed character, the two are
  treated as a combination.}

  @item{@indexed-scheme['any-one] breaks lines on either a return or
  linefeed character, without recognizing return-linefeed
  combinations.}

]

Return and linefeed characters are detected after the conversions that
are automatically performed when reading a file in text mode. For
example, reading a file in text mode under Windows automatically
changes return-linefeed combinations to a linefeed. Thus, when a file
is opened in text mode, @scheme['linefeed] is usually the appropriate
@scheme[read-line] mode.}

@defproc[(read-bytes-line [in input-port? (current-input-port)] 
                    [mode (or/c 'linefeed 'return 'return-linefeed 'any 'any-one) 'linefeed])
         (or/c bytes? eof-object?)]{
Like @scheme[read-line], but reads bytes and produces a byte string.}

@defproc[(read-string [amt exact-nonnegative-integer?]
                      [in input-port? (current-input-port)])
         (or/c string? eof-object?)]{

@margin-note{To read an entire port as a string use @scheme[port->string].}

Returns a string containing the next @scheme[amt] characters from
@scheme[in].

If @scheme[amt] is @scheme[0], then the empty string is
returned. Otherwise, if fewer than @scheme[amt] characters are
available before an end-of-file is encountered, then the returned
string will contain only those characters before the end-of-file; that
is, the returned string's length will be less than @scheme[amt]. (A
temporary string of size @scheme[amt] is allocated while reading the
input, even if the size of the result is less than @scheme[amt]
characters.) If no characters are available before an end-of-file,
then @scheme[eof] is returned.

If an error occurs during reading, some characters may be lost; that
is, if @scheme[read-string] successfully reads some characters before
encountering an error, the characters are dropped.}

@defproc[(read-bytes [amt exact-nonnegative-integer?]
                     [in input-port? (current-input-port)])
         (or/c bytes? eof-object?)]{
@margin-note{To read an entire port as bytes use @scheme[port->bytes].}
Like @scheme[read-string], but reads bytes and produces a byte string.}

@defproc[(read-string! [str (and/c string? (not/c immutable?))]
                       [in input-port? (current-input-port)]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos exact-nonnegative-integer? (string-length str)])
         (or/c exact-positive-integer? eof-object?)]{

Reads characters from @scheme[in] like @scheme[read-string], but puts
them into @scheme[str] starting from index @scheme[start-pos]
(inclusive) up to @scheme[end-pos] (exclusive). Like
@scheme[substring], the @exnraise[exn:fail:contract] if
@scheme[start-pos] or @scheme[end-pos] is out-of-range for
@scheme[str].

If the difference between @scheme[start-pos] and @scheme[end-pos] is
@scheme[0], then @scheme[0] is returned and @scheme[str] is not
modified. If no bytes are available before an end-of-file, then
@scheme[eof] is returned. Otherwise, the return value is the number of
characters read. If @math{m} characters are read and
@math{m<@scheme[end-pos]-@scheme[start-pos]}, then @scheme[str] is
not modified at indices @math{@scheme[start-pos]+m} though
@scheme[end-pos].}

@defproc[(read-bytes! [bstr bytes?]
                      [in input-port? (current-input-port)]
                      [start-pos exact-nonnegative-integer? 0]
                      [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-positive-integer? eof-object?)]{
Like @scheme[read-string!], but reads bytes, puts them into a byte
string, and returns the number of bytes read.}

@defproc[(read-bytes-avail! [bstr bytes?]
                            [in input-port? (current-input-port)]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-positive-integer? eof-object? procedure?)]{

Like @scheme[read-bytes!], but it returns without blocking after
reading immediately-available bytes, and it may return a procedure for
a ``special'' result. The @scheme[read-bytes-avail!] procedure blocks
only if no bytes (or specials) are yet available. Also unlike
@scheme[read-bytes!], @scheme[read-bytes-avail!] never drops bytes; if
@scheme[read-bytes-avail!] successfully reads some bytes and then
encounters an error, it suppresses the error (treating it roughly like
an end-of-file) and returns the read bytes.  (The error will be
triggered by future reads.) If an error is encountered before any
bytes have been read, an exception is raised.

When @scheme[in] produces a special value, as described in
@secref["customport"], the result is a procedure of four
arguments. The four arguments correspond to the location of the
special value within the port, as described in
@secref["customport"]. If the procedure is called more than once
with valid arguments, the @exnraise[exn:fail:contract]. If
@scheme[read-bytes-avail] returns a special-producing procedure, then
it does not place characters in @scheme[bstr]. Similarly,
@scheme[read-bytes-avail] places only as many bytes into @scheme[bstr]
as are available before a special value in the port's stream.}

@defproc[(read-bytes-avail!* [bstr bytes?]
                             [in input-port? (current-input-port)]
                             [start-pos exact-nonnegative-integer? 0]
                             [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-nonnegative-integer? eof-object? procedure?)]{

Like @scheme[read-bytes-avail!], but returns @scheme[0] immediately if
no bytes (or specials) are available for reading and the end-of-file
is not reached.}

@defproc[(read-bytes-avail!/enable-break [bstr bytes?]
                                         [in input-port? (current-input-port)]
                                         [start-pos exact-nonnegative-integer? 0]
                                         [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-positive-integer? eof-object? procedure?)]{

Like @scheme[read-bytes-avail!], but breaks are enabled during the
read (see also @secref["breakhandler"]). If breaking is disabled
when @scheme[read-bytes-avail!/enable-break] is called, and if the
@scheme[exn:break] exception is raised as a result of the call, then
no bytes will have been read from @scheme[in].}


@defproc[(peek-string [amt exact-nonnegative-integer?]
                      [skip-bytes-amt exact-nonnegative-integer?]
                      [in input-port? (current-input-port)])
         (or/c string? eof-object?)]{

Similar to @scheme[read-string], except that the returned characters
are preserved in the port for future reads. (More precisely, undecoded
bytes are left for future reads.) The @scheme[skip-bytes-amt] argument
indicates a number of bytes (@italic{not} characters) in the input
stream to skip before collecting characters to return; thus, in total,
the next @scheme[skip-bytes-amt] bytes plus @scheme[amt] characters
are inspected.

For most kinds of ports, inspecting @scheme[skip-bytes-amt] bytes and
@scheme[amt] characters requires at least
@math{@scheme[skip-bytes-amt]+@scheme[amt]} bytes of memory overhead
associated with the port, at least until the bytes/characters are
read. No such overhead is required when peeking into a string port
(see @secref["stringport"]), a pipe port (see
@secref["pipeports"]), or a custom port with a specific peek
procedure (depending on how the peek procedure is implemented; see
@secref["customport"]).

If a port produces @scheme[eof] mid-stream, peek skips beyond the
@scheme[eof] always produce @scheme[eof] until the @scheme[eof] is
read.}

@defproc[(peek-bytes [amt exact-nonnegative-integer?]
                     [skip-bytes-amt exact-nonnegative-integer?]
                     [in input-port? (current-input-port)])
         (or/c bytes? eof-object?)]{
Like @scheme[peek-string], but peeks bytes and produces a byte string.}

@defproc[(peek-string! [str (and/c string? (not/c immutable?))]
                       [skip-bytes-amt exact-nonnegative-integer?]
                       [in input-port? (current-input-port)]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos exact-nonnegative-integer? (string-length str)])
         (or/c exact-positive-integer? eof-object?)]{
Like @scheme[read-string!], but for peeking, and with a
@scheme[skip-bytes-amt] argument like @scheme[peek-string].}

@defproc[(peek-bytes! [bstr (and/c bytes? (not/c immutable?))]
                      [skip-bytes-amt exact-nonnegative-integer?]
                      [in input-port? (current-input-port)]
                      [start-pos exact-nonnegative-integer? 0]
                      [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-positive-integer? eof-object?)]{
Like @scheme[peek-string!], but peeks bytes, puts them into a byte
string, and returns the number of bytes read.}

@defproc[(peek-bytes-avail! [bstr (and/c bytes? (not/c immutable?))]
                            [skip-bytes-amt exact-nonnegative-integer?]
                            [progress (or/c evt? #f) #f]
                            [in input-port? (current-input-port)]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-nonnegative-integer? eof-object? procedure?)]{

Like @scheme[read-bytes-avail!], but for peeking, and with two extra
arguments. The @scheme[skip-bytes-amt] argument is as in
@scheme[peek-bytes].  The @scheme[progress] argument must be either
@scheme[#f] or an event produced by
@scheme[port-progress-evt] for @scheme[in].

To peek, @scheme[peek-bytes-avail!] blocks until finding an
end-of-file, at least one byte (or special) past the skipped bytes, or
until a non-@scheme[#f] @scheme[progress] becomes ready. Furthermore,
if @scheme[progress] is ready before bytes are peeked, no bytes are
peeked or skipped, and @scheme[progress] may cut short the skipping
process if it becomes available during the peek attempt.

The result of @scheme[peek-bytes-avail!] is @scheme[0] only in the
case that @scheme[progress] becomes ready before bytes are peeked.}

@defproc[(peek-bytes-avail!* [bstr (and/c bytes? (not/c immutable?))]
                             [skip-bytes-amt exact-nonnegative-integer?]
                             [progress (or/c evt? #f) #f]
                             [in input-port? (current-input-port)]
                             [start-pos exact-nonnegative-integer? 0]
                             [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-nonnegative-integer? eof-object? procedure?)]{

Like @scheme[read-bytes-avail!*], but for peeking, and with
@scheme[skip-bytes-amt] and @scheme[progress] arguments like
@scheme[peek-bytes-avail!]. Since this procedure never blocks, it may
return before even @scheme[skip-amt] bytes are available from the
port.}

@defproc[(peek-bytes-avail!/enable-break [bstr (and/c bytes? (not/c immutable?))]
                                         [skip-bytes-amt exact-nonnegative-integer?]
                                         [progress (or/c evt? #f) #f]
                                         [in input-port? (current-input-port)]
                                         [start-pos exact-nonnegative-integer? 0]
                                         [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-nonnegative-integer? eof-object? procedure?)]{
Like @scheme[read-bytes-avail!/enable-break], but for peeking, and
with @scheme[skip-bytes-amt] and @scheme[progress] arguments like
@scheme[peek-bytes-avail!].}


@defproc[(read-char-or-special [in input-port? (current-input-port)])
         (or/c character? eof-object? any/c)]{

Like @scheme[read-char], but that if the input port returns a non-byte
value (through a value-generating procedure in a custom port; see
@secref["customport"] and @secref["special-comments"] for
details), the non-byte value is returned.}

@defproc[(read-byte-or-special [in input-port? (current-input-port)])
         (or/c byte? eof-object? any/c)]{

Like @scheme[read-char-or-special], but reads and returns a byte
instead of a character.}

@defproc[(peek-char [in input-port? (current-input-port)]
                    [skip-bytes-amt exact-nonnegative-integer? 0])
         (or/c character? eof-object?)]{

Like @scheme[read-char], but peeks instead of reading, and skipping
@scheme[skip-bytes-amt] bytes (not characters) at the start of the
port.}

@defproc[(peek-byte [in input-port? (current-input-port)]
                    [skip-bytes-amt exact-nonnegative-integer? 0])
         (or/c byte? eof-object?)]{

Like @scheme[peek-char], but reads and returns a byte instead of a
character.}

@defproc[(peek-char-or-special [in input-port? (current-input-port)]
                               [skip-bytes-amt exact-nonnegative-integer? 0])
         (or/c character? eof-object? any/c)]{

Like @scheme[peek-char], but if the input port returns a non-byte
value after @scheme[skip-bytes-amt] byte positions, it is returned.}

@defproc[(peek-byte-or-special [in input-port? (current-input-port)]
                               [skip-bytes-amt exact-nonnegative-integer? 0]
                               [progress (or/c evt? #f) #f])
         (or/c character? eof-object? any/c)]{

Like @scheme[peek-char-or-special], but reads and returns a byte
instead of a character, and it supports a @scheme[progress] argument
like @scheme[peek-bytes-avail!].}

@defproc[(port-progress-evt [in input-port? (current-input-port)])
         evt?]{

Returns an event that becomes ready after any subsequent read from
@scheme[in], or after @scheme[in] is closed. After the event becomes
ready, it remains ready.  If progress events are unavailable for
@scheme[in] (as reported by @scheme[port-provides-progress-evts?]) the
@exnraise[exn:fail:contract].}

@defproc[(port-provides-progress-evts? [in input-port?]) boolean]{

Returns @scheme[#t] if @scheme[port-progress-evt] can return an event
for @scheme[in]. All built-in kinds of ports support progress events,
but ports created with @scheme[make-input-port] (see
@secref["customport"]) may not.}
 
@defproc[(port-commit-peeked [amt exact-nonnegative-integer?]
                             [progress evt?]
                             [evt evt?]
                             [in input-port? (current-input-port)])
         boolean?]{

Attempts to commit as read the first @scheme[amt] previously peeked
bytes, non-byte specials, and @scheme[eof]s from @scheme[in], or the
first @scheme[eof] or special value peeked from
@scheme[in]. (Only mid-stream @scheme[eof]s can be
committed. A @scheme[eof] when the port is exhausted does not
correspond to data in the stream.)

The read commits only if @scheme[progress] does not become ready first
(i.e., if no other process reads from @scheme[in] first), and only if
@scheme[evt] is chosen by a @scheme[sync] within
@scheme[port-commit-peeked] (in which case the event result is
ignored); the @scheme[evt] must be either a channel-put event,
channel, semaphore, semaphore-peek event, always event, or never
event. Suspending the thread that calls @scheme[port-commit-peeked]
may or may not prevent the commit from proceeding.

The result from @scheme[port-commit-peeked] is @scheme[#t] if data is
committed, and @scheme[#f] otherwise.

If no data has been peeked from @scheme[in] and @scheme[progress] is
not ready, then @exnraise[exn:fail:contract].  If fewer than
@scheme[amt] items have been peeked at the current start of
@scheme[in]'s stream, then only the peeked items are committed as
read.  If @scheme[in]'s stream currently starts at an @scheme[eof] or
a non-byte special value, then only the @scheme[eof] or special value
is committed as read.
 
If @scheme[progress] is not a result of @scheme[port-progress-evt]
applied to @scheme[in], then @exnraise[exn:fail:contract].}

@defproc[(byte-ready? [in input-port? (current-input-port)])
         boolean?]{

Returns @scheme[#t] if @scheme[(read-byte in)] would not block (at the
time that @scheme[byte-ready?] was called, at least).  Equivalent to
@scheme[(and (sync/timeout 0 in) #t)].}

@defproc[(char-ready? [in input-port? (current-input-port)])
         boolean?]{

Returns @scheme[#t] if @scheme[(read-char in)] would not block (at the
time that @scheme[char-ready?] was called, at least). Depending on the
initial bytes of the stream, multiple bytes may be needed to form a
UTF-8 encoding.}
