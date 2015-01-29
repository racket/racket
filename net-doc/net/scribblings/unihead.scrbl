#lang scribble/doc
@(require "common.rkt"
          scribble/eval
          scribble/bnf
          (for-label net/unihead))

@(define unihead-eval (make-base-eval))
@interaction-eval[#:eval unihead-eval (require net/unihead)]

@(define l1-seq @elem{@litchar{=?ISO-8859-1?Q?}...@litchar{?=}})
@(define uni-seq @elem{@litchar{=?UTF-8?B?}...@litchar{?=}})

@(define enc-format @elem{@litchar{=?}@nonterm{encoding}@litchar{?}@nonterm{transport}@litchar{?}...@litchar{?=}})

@title[#:tag "unihead"]{Header Field Encoding}

@defmodule[net/unihead]{The @racketmodname[net/unihead] module
provides utilities for encoding and decoding header fields using the
@|enc-format| format.}

@defproc[(encode-for-header [s string?]) string?]{

Encodes @racket[s] for use in a header.

If @racket[s] contains only ASCII characters, then the result string
will have the same content as the given string. If @racket[s] contains
only Latin-1 characters, then on each CRLF-delimited line, the
space-delimited sequence containing all non-ASCII characters in
@racket[s] is encoded with a @|l1-seq| sequence. If @racket[s]
contains non-Latin-1 characters, then on each CRLF-delimited line, a
space-delimited sequence containing all non-ASCII characters in
@racket[s] is encoded with a @|uni-seq| sequence.

@examples[
#:eval unihead-eval
(encode-for-header "English")
(encode-for-header "français")
(encode-for-header "→" )
(encode-for-header "→\r\nboth → and français here")
]}

@defproc[(decode-for-header [s string?]) string?]{

Decodes header fields that use the @|enc-format| encoding format. The
specified @nonterm{encoding} is generalized via
@racket[generalize-encoding] before decoding content.

@examples[
#:eval unihead-eval
(decode-for-header "English")
(decode-for-header "=?UTF-8?B?4oaS?= =?ISO-8859-1?Q?fran=E7ais?=")
]}


@defproc[(generalize-encoding [s (or string? bytes?)]) (or string? bytes?)]{

Generalizes the encoding name @racket[s] to compensate for typical
mailer bugs: Latin-1 and ASCII encodings are geenralized to
WINDOWS-1252; GB and GB2312 are generalized to GBK; and KS_C_5601-1987
is generalized to CP949.}


@close-eval[unihead-eval]
