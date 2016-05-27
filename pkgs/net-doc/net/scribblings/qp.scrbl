#lang scribble/doc
@(require "common.rkt" (for-label net/qp net/qp-unit net/qp-sig))

@title[#:tag "qp"]{Quoted-Printable: Encoding and Decoding}

@defmodule[net/qp]{The @racketmodname[net/qp] library provides
utilities for quoted-printable (mime-standard) encoding and decoding
from RFC 2045 section 6.7.

The library was written by Francisco Solsona.}

@section[#:tag "qp-procs"]{Functions}

@defproc[(qp-encode [bstr bytes?]) bytes?]{

Consumes a byte string and returns its quoted printable representation
as a new string. The encoded string uses @racket[#"\r\n"] where
necessary to create shorter lines.}


@defproc[(qp-decode [bstr bytes?]) bytes?]{

Consumes a byte string and returns its un-quoted printable
representation as a new string. Non-soft line breaks are preserved in
whatever form they exist (CR, LR, or CRLF) in the input string.}


@defproc[(qp-encode-stream [in input-port?]
                           [out output-port?]
                           [newline-bstr bytes? #"\n"])
         void?]{

Reads characters from @racket[in] and writes the quoted printable
encoded result to @racket[out].

The @racket[newline-bstr] argument is used for soft line-breaks (after
@litchar{=}). Note that the default @racket[newline-bstr] is just
@racket[#"\n"], not @racket[#"\r\n"].

Other line breaks are preserved in whatever form they exist (CR, LR,
or CRLF) in the input stream.}


@defproc[(qp-decode-stream [in input-port?]
                               [out output-port?])
         void?]{

Reads characters from @racket[in] and writes de-quoted-printable
result to @racket[out].  Non-soft line breaks are preserved in
whatever form they exist (CR, LR, or CRLF) in the input stream.}

@; ----------------------------------------

@section[#:tag "qp-exn"]{Exceptions}

@deftogether[(
@defstruct[qp-error ()]
@defstruct[(qp-wrong-input qp-error) ()]
@defstruct[(qp-wrong-line-size qp-error) ()]
)]{

None of these are used anymore, but the bindings are preserved for
backward compatibility.}

@; ----------------------------------------

@section{Quoted-Printable Unit}

@margin-note{@racket[qp@] and @racket[qp^] are deprecated.
They exist for backward-compatibility and will likely be removed in
the future. New code should use the @racketmodname[net/qp] module.}

@defmodule[net/qp-unit]

@defthing[qp@ unit?]{

Imports nothing, exports @racket[qp^].}

@; ----------------------------------------

@section{-Printable Signature}

@defmodule[net/qp-sig]

@defsignature[qp^ ()]{}

Includes everything exported by the @racketmodname[net/qp] module.
