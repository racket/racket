#lang scribble/doc
@(require "utils.rkt" 
          (only-in scribble/decode make-splice)
          scribble/racket
          (for-label ffi/unsafe/string-list))

@title[#:tag "string-list"]{String List Types}

@defmodule[ffi/unsafe/string-list]{The
@racketmodname[ffi/unsafe/string-list] module provides types for
converting between string and bytes string lists and foreign arrays of
strings. This library is particularly useful with @CS[], since
types like @racket[(_list i _string)] cannot work for passing an
array of strings to a foreign call.}

@history[#:added "8.1.0.5"]

@deftogether[(
@defthing[_string-list ctype?]
@defthing[_string-list/utf-8 ctype?]
@defthing[_string-list/locale ctype?]
@defthing[_string-list/latin-1 ctype?]
@defthing[_string-list/utf-16 ctype?]
@defthing[_string-list/ucs-4 ctype?]
@defthing[_bytes-list ctype?]
@defthing[_bytes-list/nul-terminated ctype?]
)]{

Types that are similar to @racket[(_list i _string)],
@racket[(_list i _string/utf-8)],
@racket[(_list i _string/locale)],
@racket[(_list i _string/latin-1)],
@racket[(_list i _string/latin-1)],
@racket[(_list i _string/utf-16)],
@racket[(_list i _string/ucs-4)],
@racket[(_list i _bytes)], and
@racket[(_list i _bytes/nul-terminated)], but they work as foreign-call
arguments for @CS[] (as well as @BC[]).

These types convert a lists of strings or bytes to a single block of
@racket['atomic-interior] memory (see @racket[malloc]) that starts
with a NULL-terminated array of pointers into itself. Each string or
bytes string is word-aligned within the atomic block and terminated by
a suitable sequence of NUL bytes (except for @racket[_bytes-list], which
does not include a terminator).

The @racket[_string-list] type works only when the
@racket[default-_string-type] parameter has one of the following
values:
@racket[_string/utf-8], @racket[_string*/utf-8],
@racket[_string/latin-1], @racket[_string*/latin-1],
@racket[_string/locale], @racket[_string*/locale],
@racket[_string/utf-16], or
@racket[_string/ucs-4].

When used as a foreign-call result type and similar positions, these
types expect the foreign representation to include a NULL terminator
for the array of pointers as well as a NUL terminator for each string
or byte string.}
