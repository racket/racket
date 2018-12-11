#lang scribble/doc
@(require "common.rkt" scribble/eval (for-label file/sha1))

@(define sha1-eval (make-base-eval))
@interaction-eval[#:eval sha1-eval (require file/sha1)]

@title[#:tag "sha1b"]{SHA1 Message Digest}

@defmodule[file/sha1]

See @racketmodname[openssl/sha1] for a faster implementation.

@defproc[(sha1 [in (or/c bytes? input-port?)]
               [start exact-nonnegative-integer? 0]
               [end (or/c #f exact-nonnegative-integer?) #f])
         string?]{

Returns a 40-character string that represents the SHA-1 hash (in
hexadecimal notation) of the content from @racket[in]. The
@racket[in], @racket[start], and @racket[end] arguments are treated
the same as @racket[sha1-bytes] from @racketmodname[racket/base].

The @racket[sha1] function composes @racket[bytes->hex-string] with
@racket[sha1-bytes].

@examples[
#:eval sha1-eval
(sha1 (open-input-bytes #"abc"))
]

@history[#:changed "7.0.0.5" @elem{Allowed a byte string as @racket[in]
                                   and added the @racket[start] and
                                   @racket[end] arguments.}]}

@defproc[#:link-target? #f
         (sha1-bytes [in (or/c bytes? input-port?)]
                     [start exact-nonnegative-integer? 0]
                     [end (or/c #f exact-nonnegative-integer?) #f])
         bytes?]{

The same as @racket[sha1-bytes] from @racketmodname[racket/base],
returns a 20-byte byte string that represents the SHA-1 hash of the
content from @racket[in].

@examples[
#:eval sha1-eval
(sha1-bytes (open-input-bytes #"abc"))
]

@history[#:changed "7.0.0.5" @elem{Allowed a byte string as @racket[in]
                                   and added the @racket[start] and
                                   @racket[end] arguments.}]}

@defproc[(bytes->hex-string [bstr bytes?]) string?]{

Converts the given byte string to a string representation, where each
byte in @racket[bstr] is converted to its two-digit hexadecimal
representation in the resulting string.

@examples[
#:eval sha1-eval
(bytes->hex-string #"turtles")
]}

@defproc[(hex-string->bytes [str string?]) bytes?]{

Converts the given string to a byte string, where each pair of characters in
@racket[str] is converted to a single byte in the result.

@examples[
#:eval sha1-eval
(hex-string->bytes "70")
(hex-string->bytes "Af")
]}

@close-eval[sha1-eval]
