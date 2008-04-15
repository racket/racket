#lang scribble/doc
@(require "common.ss"
          (for-label net/base64
                     net/base64-unit
                     net/base64-sig))

@title[#:tag "base64"]{Base 64: Encoding and Decoding}

@defmodule[net/base64]{The @schememodname[net/base64] library provides
utilities for Base 64 (mime-standard) encoding and decoding.}

@section[#:tag "base64-procs"]{Functions}

@defproc[(base64-encode [bstr bytes?]) bytes?]{

Consumes a byte string and returns its Base 64 encoding as a new byte
string.  The returned string is broken into 72-byte lines separated by
CRLF combinations, and always ends with a CRLF combination unless the
input is empty.}


@defproc[(base64-decode [bstr bytes?]) bytes?]{

Consumes a byte string and returns its Base 64 decoding as a new byte
string.}


@defproc[(base64-encode-stream [in input-port?]
                               [out output-port?]
                               [newline-bstr bytes? #"\n"])
         void?]{

Reads bytes from @scheme[in] and writes the encoded result to
@scheme[out], breaking the output into 72-character lines separated by
@scheme[newline-bstr], and ending with @scheme[newline-bstr] unless
the input stream is empty. Note that the default @scheme[newline-bstr]
is just @scheme[#"\n"], not @scheme[#"\r\n"]. The procedure returns when
it encounters an end-of-file from @scheme[in].}

@defproc[(base64-decode-stream [in input-port?]
                               [out output-port?])
         void?]{

Reads a Base 64 encoding from @scheme[in] and writes the decoded
result to @scheme[out]. The procedure returns when it encounters an
end-of-file or Base 64 terminator @litchar{=} from @scheme[in].}

@; ----------------------------------------

@section{Base64 Unit}

@defmodule[net/base64-unit]

@defthing[base64@ unit?]{

Imports nothing, exports @scheme[base64^].}

@; ----------------------------------------

@section{Base64 Signature}

@defmodule[net/base64-sig]

@defsignature[base64^ ()]{}

Includes everything exported by the @schememodname[net/base64] module.
