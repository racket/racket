#lang scribble/doc
@(require "common.rkt" (for-label net/base64 net/base64-unit net/base64-sig))

@title[#:tag "base64"]{Base 64: Encoding and Decoding}

@defmodule[net/base64]{The @racketmodname[net/base64] library provides
utilities for Base 64 (MIME-standard) encoding and decoding.}

@section[#:tag "base64-procs"]{Functions}

@defproc[(base64-encode [bstr bytes?] [newline-bstr bytes? #"\r\n"]) bytes?]{

Consumes a byte string and returns its Base 64 encoding as a new byte
string.  The returned string is broken into 72-byte lines separated by
@racket[newline-bstr], which defaults to a CRLF combination, and the
result always ends with a @racket[newline-bstr] unless the
input is empty.}


@defproc[(base64-decode [bstr bytes?]) bytes?]{

Consumes a byte string and returns its Base 64 decoding as a new byte
string.}


@defproc[(base64-encode-stream [in input-port?]
                               [out output-port?]
                               [newline-bstr bytes? #"\n"])
         void?]{

Reads bytes from @racket[in] and writes the encoded result to
@racket[out], breaking the output into 72-character lines separated by
@racket[newline-bstr], and ending with @racket[newline-bstr] unless
the input stream is empty. Note that the default @racket[newline-bstr]
is just @racket[#"\n"], not @racket[#"\r\n"]. The procedure returns when
it encounters an end-of-file from @racket[in].}

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
