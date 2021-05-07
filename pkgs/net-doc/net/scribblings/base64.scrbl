#lang scribble/doc
@(require "common.rkt" (for-label net/base64 net/base64-unit net/base64-sig))

@title[#:tag "base64"]{Base 64: Encoding and Decoding}

@defmodule[net/base64]{The @racketmodname[net/base64] library provides
utilities for Base 64 (MIME-standard) encoding and decoding.}

@section[#:tag "base64-procs"]{Functions}

@defproc[(base64-encode [bstr bytes?] [newline any/c #"\r\n"]) bytes?]{

Consumes a byte string and returns its Base 64 encoding as a new byte
string.  The returned string is broken into 72-byte lines separated by
@racket[newline], which defaults to a CRLF combination, and the
result always ends with a @racket[newline] unless the
input is empty.

Although @racket[newline] is intended to be a byte string, it can be
any value (possibly with a performance penalty), and it is converted
to a byte string using @racket[display].}


@defproc[(base64-decode [bstr bytes?]) bytes?]{

Consumes a byte string and returns its Base 64 decoding as a new byte
string.}


@defproc[(base64-encode-stream [in input-port?]
                               [out output-port?]
                               [newline any/c #"\n"])
         void?]{

Reads bytes from @racket[in] and writes the encoded result to
@racket[out], breaking the output into 72-character lines separated by
@racket[newline], and ending with @racket[newline] unless
the input stream is empty. Note that the default @racket[newline]
is just @racket[#"\n"], not @racket[#"\r\n"]. The procedure returns when
it encounters an end-of-file from @racket[in].

Although @racket[newline] is intended to be a byte string, it can be
any value, and it is written using @racket[display].}

@defproc[(base64-decode-stream [in input-port?]
                               [out output-port?])
         void?]{

Reads a Base 64 encoding from @racket[in] and writes the decoded
result to @racket[out]. The procedure returns when it encounters an
end-of-file or Base 64 terminator @litchar{=} from @racket[in].}

@; ----------------------------------------

@section{Base64 Unit}

@margin-note{@racket[base64@] and @racket[base64^] are deprecated.
They exist for backward-compatibility and will likely be removed in
the future. New code should use the @racketmodname[net/base64] module.}

@defmodule[net/base64-unit]

@defthing[base64@ unit?]{

Imports nothing, exports @racket[base64^].}

@; ----------------------------------------

@section{Base64 Signature}

@defmodule[net/base64-sig]

@defsignature[base64^ ()]{}

Includes everything exported by the @racketmodname[net/base64] module.
