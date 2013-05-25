#lang scribble/doc
@(require "mz.rkt")

@(define si-eval (make-base-eval))


@title{Byte and String Input}

@defproc[(read-char [in input-port? (current-input-port)]) 
         (or/c char? eof-object?)]{

Reads a single character from @racket[in]---which may involve reading
several bytes to UTF-8-decode them into a character (see
@secref["ports"]); a minimal number of bytes are read/@tech{peek}ed to
perform the decoding. If no bytes are available before an end-of-file,
then @racket[eof] is returned.}

@examples[#:eval si-eval
(let ([ip (open-input-string "S2")])
  (print (read-char ip)) 
  (newline)
  (print (read-char ip))
  (newline)
  (print (read-char ip)))

(let ([ip (open-input-bytes #"\316\273")])
  @code:comment{The byte string contains UTF-8-encoded content:}
  (print (read-char ip)))
]


@defproc[(read-byte [in input-port? (current-input-port)]) 
         (or/c byte? eof-object?)]{

Reads a single byte from @racket[in]. If no bytes are available before
an end-of-file, then @racket[eof] is returned.}


@examples[#:eval si-eval
(let ([ip (open-input-string "a")])
  @code:comment{The two values in the following list should be the same.}
  (list (read-byte ip) (char->integer #\a)))

(let ([ip (open-input-string (string #\u03bb))])
  @code:comment{This string has a two byte-encoding.}
  (list (read-byte ip) (read-byte ip) (read-byte ip)))
]


@defproc[(read-line [in input-port? (current-input-port)]
                    [mode (or/c 'linefeed 'return 'return-linefeed 'any 'any-one) 'linefeed])
         (or/c string? eof-object?)]{

Returns a string containing the next line of bytes from @racket[in].

Characters are read from @racket[in] until a line separator or an
end-of-file is read. The line separator is not included in the result
string (but it is removed from the port's stream). If no characters
are read before an end-of-file is encountered, @racket[eof] is
returned.

The @racket[mode] argument determines the line separator(s). It
must be one of the following symbols:

 @itemize[

  @item{@indexed-racket['linefeed] breaks lines on linefeed characters.}

  @item{@indexed-racket['return] breaks lines on return characters.}

  @item{@indexed-racket['return-linefeed] breaks lines on
  return-linefeed combinations. If a return character is not followed
  by a linefeed character, it is included in the result string;
  similarly, a linefeed that is not preceded by a return is included
  in the result string.}

  @item{@indexed-racket['any] breaks lines on any of a return
  character, linefeed character, or return-linefeed combination. If a
  return character is followed by a linefeed character, the two are
  treated as a combination.}

  @item{@indexed-racket['any-one] breaks lines on either a return or
  linefeed character, without recognizing return-linefeed
  combinations.}

]

Return and linefeed characters are detected after the conversions that
are automatically performed when reading a file in text mode. For
example, reading a file in text mode on Windows automatically
changes return-linefeed combinations to a linefeed. Thus, when a file
is opened in text mode, @racket['linefeed] is usually the appropriate
@racket[read-line] mode.}

@examples[#:eval si-eval
(let ([ip (open-input-string "x\ny\n")])
  (read-line ip))

(let ([ip (open-input-string "x\ny\n")])
  (read-line ip 'return))

(let ([ip (open-input-string "x\ry\r")])
  (read-line ip 'return))

(let ([ip (open-input-string "x\r\ny\r\n")])
  (read-line ip 'return-linefeed))

(let ([ip (open-input-string "x\r\ny\nz")])
  (list (read-line ip 'any) (read-line ip 'any)))

(let ([ip (open-input-string "x\r\ny\nz")])
  (list (read-line ip 'any-one) (read-line ip 'any-one)))
]


@defproc[(read-bytes-line [in input-port? (current-input-port)] 
                    [mode (or/c 'linefeed 'return 'return-linefeed 'any 'any-one) 'linefeed])
         (or/c bytes? eof-object?)]{
Like @racket[read-line], but reads bytes and produces a byte string.}

@defproc[(read-string [amt exact-nonnegative-integer?]
                      [in input-port? (current-input-port)])
         (or/c string? eof-object?)]{

@margin-note{To read an entire port as a string, use @racket[port->string].}

Returns a string containing the next @racket[amt] characters from
@racket[in].

If @racket[amt] is @racket[0], then the empty string is
returned. Otherwise, if fewer than @racket[amt] characters are
available before an end-of-file is encountered, then the returned
string will contain only those characters before the end-of-file; that
is, the returned string's length will be less than @racket[amt]. (A
temporary string of size @racket[amt] is allocated while reading the
input, even if the size of the result is less than @racket[amt]
characters.) If no characters are available before an end-of-file,
then @racket[eof] is returned.

If an error occurs during reading, some characters may be lost; that
is, if @racket[read-string] successfully reads some characters before
encountering an error, the characters are dropped.}

@examples[#:eval si-eval
(let ([ip (open-input-string "supercalifragilisticexpialidocious")])
  (read-string 5 ip))
]

@defproc[(read-bytes [amt exact-nonnegative-integer?]
                     [in input-port? (current-input-port)])
         (or/c bytes? eof-object?)]{
@margin-note{To read an entire port as bytes, use @racket[port->bytes].}
Like @racket[read-string], but reads bytes and produces a byte string.}

@examples[#:eval si-eval
(let ([ip (open-input-bytes 
                  (bytes 6 
                         115 101 99 114 101
                         116))])
  (define length (read-byte ip))
  (bytes->string/utf-8 (read-bytes length ip)))
]

@defproc[(read-string! [str (and/c string? (not/c immutable?))]
                       [in input-port? (current-input-port)]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos exact-nonnegative-integer? (string-length str)])
         (or/c exact-positive-integer? eof-object?)]{

Reads characters from @racket[in] like @racket[read-string], but puts
them into @racket[str] starting from index @racket[start-pos]
(inclusive) up to @racket[end-pos] (exclusive). Like
@racket[substring], the @exnraise[exn:fail:contract] if
@racket[start-pos] or @racket[end-pos] is out-of-range for
@racket[str].

If the difference between @racket[start-pos] and @racket[end-pos] is
@racket[0], then @racket[0] is returned and @racket[str] is not
modified. If no bytes are available before an end-of-file, then
@racket[eof] is returned. Otherwise, the return value is the number of
characters read. If @math{m} characters are read and
@math{m<@racket[end-pos]-@racket[start-pos]}, then @racket[str] is
not modified at indices @math{@racket[start-pos]+m} through
@racket[end-pos].}

@examples[#:eval si-eval
(let ([buffer (make-string 10 #\_)]
      [ip (open-input-string "cketRa")])
  (printf "~s\n" buffer)
  (read-string! buffer ip 2 6)
  (printf "~s\n" buffer)
  (read-string! buffer ip 0 2)
  (printf "~s\n" buffer))
]

@defproc[(read-bytes! [bstr bytes?]
                      [in input-port? (current-input-port)]
                      [start-pos exact-nonnegative-integer? 0]
                      [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-positive-integer? eof-object?)]{
Like @racket[read-string!], but reads bytes, puts them into a byte
string, and returns the number of bytes read.

@examples[
(let ([buffer (make-bytes 10 (char->integer #\_))]
      [ip (open-input-string "cketRa")])
  (printf "~s\n" buffer)
  (read-bytes! buffer ip 2 6)
  (printf "~s\n" buffer)
  (read-bytes! buffer ip 0 2)
  (printf "~s\n" buffer))
]
}

@defproc[(read-bytes-avail! [bstr bytes?]
                            [in input-port? (current-input-port)]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-positive-integer? eof-object? procedure?)]{

Like @racket[read-bytes!], but returns without blocking after having
read the immediately available bytes, and it may return a procedure for
a ``special'' result. The @racket[read-bytes-avail!] procedure blocks
only if no bytes (or specials) are yet available. Also unlike
@racket[read-bytes!], @racket[read-bytes-avail!] never drops bytes; if
@racket[read-bytes-avail!] successfully reads some bytes and then
encounters an error, it suppresses the error (treating it roughly like
an end-of-file) and returns the read bytes.  (The error will be
triggered by future reads.) If an error is encountered before any
bytes have been read, an exception is raised.

When @racket[in] produces a special value, as described in
@secref["customport"], the result is a procedure of four
arguments. The four arguments correspond to the location of the
special value within the port, as described in
@secref["customport"]. If the procedure is called more than once
with valid arguments, the @exnraise[exn:fail:contract]. If
@racket[read-bytes-avail] returns a special-producing procedure, then
it does not place characters in @racket[bstr]. Similarly,
@racket[read-bytes-avail] places only as many bytes into @racket[bstr]
as are available before a special value in the port's stream.}

@defproc[(read-bytes-avail!* [bstr bytes?]
                             [in input-port? (current-input-port)]
                             [start-pos exact-nonnegative-integer? 0]
                             [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-nonnegative-integer? eof-object? procedure?)]{

Like @racket[read-bytes-avail!], but returns @racket[0] immediately if
no bytes (or specials) are available for reading and the end-of-file
is not reached.}

@defproc[(read-bytes-avail!/enable-break [bstr bytes?]
                                         [in input-port? (current-input-port)]
                                         [start-pos exact-nonnegative-integer? 0]
                                         [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-positive-integer? eof-object? procedure?)]{

Like @racket[read-bytes-avail!], but breaks are enabled during the
read (see also @secref["breakhandler"]). If breaking is disabled
when @racket[read-bytes-avail!/enable-break] is called, and if the
@racket[exn:break] exception is raised as a result of the call, then
no bytes will have been read from @racket[in].}


@defproc[(peek-string [amt exact-nonnegative-integer?]
                      [skip-bytes-amt exact-nonnegative-integer?]
                      [in input-port? (current-input-port)])
         (or/c string? eof-object?)]{

Similar to @racket[read-string], except that the returned characters
are @tech{peek}ed: preserved in the port for future reads and @tech{peeks}. (More precisely, undecoded
bytes are left for future reads and peeks.) The @racket[skip-bytes-amt] argument
indicates a number of bytes (@italic{not} characters) in the input
stream to skip before collecting characters to return; thus, in total,
the next @racket[skip-bytes-amt] bytes plus @racket[amt] characters
are inspected.

For most kinds of ports, inspecting @racket[skip-bytes-amt] bytes and
@racket[amt] characters requires at least
@math{@racket[skip-bytes-amt]+@racket[amt]} bytes of memory overhead
associated with the port, at least until the bytes/characters are
read. No such overhead is required when peeking into a string port
(see @secref["stringport"]), a pipe port (see
@secref["pipeports"]), or a custom port with a specific peek
procedure (depending on how the peek procedure is implemented; see
@secref["customport"]).

If a port produces @racket[eof] mid-stream, attempts to skip beyond the
@racket[eof] for a @tech{peek} always produce @racket[eof] until the @racket[eof] is
read.}

@defproc[(peek-bytes [amt exact-nonnegative-integer?]
                     [skip-bytes-amt exact-nonnegative-integer?]
                     [in input-port? (current-input-port)])
         (or/c bytes? eof-object?)]{
Like @racket[peek-string], but @tech{peeks} bytes and produces a byte string.}

@defproc[(peek-string! [str (and/c string? (not/c immutable?))]
                       [skip-bytes-amt exact-nonnegative-integer?]
                       [in input-port? (current-input-port)]
                       [start-pos exact-nonnegative-integer? 0]
                       [end-pos exact-nonnegative-integer? (string-length str)])
         (or/c exact-positive-integer? eof-object?)]{
Like @racket[read-string!], but for @tech{peek}ing, and with a
@racket[skip-bytes-amt] argument like @racket[peek-string].}

@defproc[(peek-bytes! [bstr (and/c bytes? (not/c immutable?))]
                      [skip-bytes-amt exact-nonnegative-integer?]
                      [in input-port? (current-input-port)]
                      [start-pos exact-nonnegative-integer? 0]
                      [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-positive-integer? eof-object?)]{
Like @racket[peek-string!], but @tech{peeks} bytes, puts them into a byte
string, and returns the number of bytes read.}

@defproc[(peek-bytes-avail! [bstr (and/c bytes? (not/c immutable?))]
                            [skip-bytes-amt exact-nonnegative-integer?]
                            [progress (or/c progress-evt? #f) #f]
                            [in input-port? (current-input-port)]
                            [start-pos exact-nonnegative-integer? 0]
                            [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-nonnegative-integer? eof-object? procedure?)]{

Like @racket[read-bytes-avail!], but for @tech{peek}ing, and with two extra
arguments. The @racket[skip-bytes-amt] argument is as in
@racket[peek-bytes].  The @racket[progress] argument must be either
@racket[#f] or an event produced by
@racket[port-progress-evt] for @racket[in].

To @tech{peek}, @racket[peek-bytes-avail!] blocks until finding an
end-of-file, at least one byte (or special) past the skipped bytes, or
until a non-@racket[#f] @racket[progress] becomes ready. Furthermore,
if @racket[progress] is ready before bytes are peeked, no bytes are
peeked or skipped, and @racket[progress] may cut short the skipping
process if it becomes available during the peek attempt. Furthermore,
@racket[progress] is checked even before determining whether the port
is still open.

The result of @racket[peek-bytes-avail!] is @racket[0] only in the
case that @racket[progress] becomes ready before bytes are peeked.}

@defproc[(peek-bytes-avail!* [bstr (and/c bytes? (not/c immutable?))]
                             [skip-bytes-amt exact-nonnegative-integer?]
                             [progress (or/c progress-evt? #f) #f]
                             [in input-port? (current-input-port)]
                             [start-pos exact-nonnegative-integer? 0]
                             [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-nonnegative-integer? eof-object? procedure?)]{

Like @racket[read-bytes-avail!*], but for @tech{peek}ing, and with
@racket[skip-bytes-amt] and @racket[progress] arguments like
@racket[peek-bytes-avail!]. Since this procedure never blocks, it may
return before even @racket[skip-amt] bytes are available from the
port.}

@defproc[(peek-bytes-avail!/enable-break [bstr (and/c bytes? (not/c immutable?))]
                                         [skip-bytes-amt exact-nonnegative-integer?]
                                         [progress (or/c progress-evt? #f) #f]
                                         [in input-port? (current-input-port)]
                                         [start-pos exact-nonnegative-integer? 0]
                                         [end-pos exact-nonnegative-integer? (bytes-length bstr)])
         (or/c exact-nonnegative-integer? eof-object? procedure?)]{
Like @racket[read-bytes-avail!/enable-break], but for @tech{peek}ing, and
with @racket[skip-bytes-amt] and @racket[progress] arguments like
@racket[peek-bytes-avail!].}


@defproc[(read-char-or-special [in input-port? (current-input-port)])
         (or/c char? eof-object? any/c)]{

Like @racket[read-char], but if the input port returns a @tech{special}
value (through a value-generating procedure in a custom port; see
@secref["customport"] and @secref["special-comments"] for
details), then the @tech{special} value is returned.}

@defproc[(read-byte-or-special [in input-port? (current-input-port)])
         (or/c byte? eof-object? any/c)]{

Like @racket[read-char-or-special], but reads and returns a byte
instead of a character.}

@defproc[(peek-char [in input-port? (current-input-port)]
                    [skip-bytes-amt exact-nonnegative-integer? 0])
         (or/c char? eof-object?)]{

Like @racket[read-char], but @tech{peeks} instead of reading, and skips
@racket[skip-bytes-amt] bytes (not characters) at the start of the
port.}

@defproc[(peek-byte [in input-port? (current-input-port)]
                    [skip-bytes-amt exact-nonnegative-integer? 0])
         (or/c byte? eof-object?)]{

Like @racket[peek-char], but @tech{peeks} and returns a byte instead of a
character.}

@defproc[(peek-char-or-special [in input-port? (current-input-port)]
                               [skip-bytes-amt exact-nonnegative-integer? 0])
         (or/c char? eof-object? any/c)]{

Like @racket[peek-char], but if the input port returns a non-byte
value after @racket[skip-bytes-amt] byte positions, then it is returned.}

@defproc[(peek-byte-or-special [in input-port? (current-input-port)]
                               [skip-bytes-amt exact-nonnegative-integer? 0]
                               [progress (or/c progress-evt? #f) #f])
         (or/c byte? eof-object? any/c)]{

Like @racket[peek-char-or-special], but @tech{peeks} and returns a byte
instead of a character, and it supports a @racket[progress] argument
like @racket[peek-bytes-avail!].}


@defproc[(port-progress-evt [in (and/c input-port? port-provides-progress-evts?)
                                (current-input-port)])
         progress-evt?]{

Returns a @tech{synchronizable event} (see @secref["sync"]) that
becomes @tech{ready for synchronization} after any subsequent read
from @racket[in] or after @racket[in] is closed. After the event
becomes ready, it remains ready. @ResultItself{progress event}.}


@defproc[(port-provides-progress-evts? [in input-port?]) boolean]{

Returns @racket[#t] if @racket[port-progress-evt] can return an event
for @racket[in]. All built-in kinds of ports support progress events,
but ports created with @racket[make-input-port] (see
@secref["customport"]) may not.}

 
@defproc[(port-commit-peeked [amt exact-nonnegative-integer?]
                             [progress progress-evt?]
                             [evt evt?]
                             [in input-port? (current-input-port)])
         boolean?]{

Attempts to @tech{commit} as read the first @racket[amt] previously @tech{peek}ed
bytes, non-byte specials, and @racket[eof]s from @racket[in], or the
first @racket[eof] or special value peeked from
@racket[in]. Mid-stream @racket[eof]s can be
committed, but an @racket[eof] when the port is exhausted does not
necessarily commit, since it does not correspond to data in the stream.

The read commits only if @racket[progress] does not become ready first
(i.e., if no other process reads from @racket[in] first), and only if
@racket[evt] is chosen by a @racket[sync] within
@racket[port-commit-peeked] (in which case the event result is
ignored); the @racket[evt] must be either a channel-put event,
channel, semaphore, semaphore-peek event, always event, or never
event. Suspending the thread that calls @racket[port-commit-peeked]
may or may not prevent the commit from proceeding.

The result from @racket[port-commit-peeked] is @racket[#t] if data has been
committed, and @racket[#f] otherwise.

If no data has been peeked from @racket[in] and @racket[progress] is
not ready, then @exnraise[exn:fail:contract].  If fewer than
@racket[amt] items have been peeked at the current start of
@racket[in]'s stream, then only the peeked items are committed as
read.  If @racket[in]'s stream currently starts at an @racket[eof] or
a non-byte special value, then only the @racket[eof] or special value
is committed as read.
 
If @racket[progress] is not a result of @racket[port-progress-evt]
applied to @racket[in], then @exnraise[exn:fail:contract].}


@defproc[(byte-ready? [in input-port? (current-input-port)])
         boolean?]{

Returns @racket[#t] if @racket[(read-byte in)] would not block (at the
time that @racket[byte-ready?] was called, at least).  Equivalent to
@racket[(and (sync/timeout 0 in) #t)].}


@defproc[(char-ready? [in input-port? (current-input-port)])
         boolean?]{

Returns @racket[#t] if @racket[(read-char in)] would not block (at the
time that @racket[char-ready?] was called, at least). Depending on the
initial bytes of the stream, multiple bytes may be needed to form a
UTF-8 encoding.}


@defproc*[([(progress-evt? [v any/c]) boolean?]
           [(progress-evt? [evt progress-evt?] [in input-port?]) boolean?])]{

With one argument, returns @racket[#t] is @racket[v] is a progress evt
for some input port, @racket[#f] otherwise.

With two arguments, returns @racket[#t] if @racket[evt] is a progress
event for @racket[in], @racket[#f] otherwise.}


@close-eval[si-eval]
