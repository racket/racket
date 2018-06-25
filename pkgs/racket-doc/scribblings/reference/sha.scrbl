#lang scribble/doc
@(require "mz.rkt" (for-label file/sha1))

@(define sha-eval (make-base-eval))
@examples[#:hidden #:eval sha-eval (require file/sha1)]

@title[#:tag "sha"]{Cryptographic Hashing}

@deftogether[(
@defproc[(sha1-bytes [in (or/c bytes? input-port?)]
                     [start exact-nonnegative-integer? 0]
                     [end (or/c #f exact-nonnegative-integer?) #f])
         bytes?]
@defproc[(sha224-bytes [in (or/c bytes? input-port?)]
                       [start exact-nonnegative-integer? 0]
                       [end (or/c #f exact-nonnegative-integer?) #f])
         bytes?]
@defproc[(sha256-bytes [in (or/c bytes? input-port?)]
                       [start exact-nonnegative-integer? 0]
                       [end (or/c #f exact-nonnegative-integer?) #f])
         bytes?]
)]{

Computes the SHA-1, SHA-224, or SHA-256 hash of a byte sequence and
returns the hash as a byte string with 20 bytes, 28 bytes, or 32
bytes, respectively.

The @racket[start] and @racket[end] arguments determine the range of
bytes of the input that are used to compute the hash. An @racket[end]
value of @racket[#f] corresponds to the end of the byte string or an
end-of-file position for an input port. When @racket[in] is a byte
string, the @racket[start] and @racket[end] values (when non
@racket[#f]) must be no greater than the length of the byte string,
and @racket[start] must be no greater than @racket[end]. When
@racket[in] is an input port, @racket[start] must be no greater than
@racket[end]; if @racket[in] supplies less than @racket[start] or
@racket[end] bytes before an end-of-file, then @racket[start] and/or
@racket[end] is effectively changed to the number of supplied bytes
(so that an empty or truncated byte sequence is hashed). When
@racket[in] is an input port and @racket[end] is a number, then at
most @racket[end] bytes are read from the input port.

For security purposes, favor @racket[sha224-bytes] and
@racket[sha256-bytes] (which are part of the SHA-2 family) over
@racket[sha1-bytes].

Use @racket[bytes->hex-string] from @racketmodname[file/sha1] to
convert a byte string hash to a human-readable string.

@mz-examples[
#:eval sha-eval
(sha1-bytes #"abc")
(require file/sha1)
(bytes->hex-string (sha1-bytes #"abc"))
(bytes->hex-string (sha224-bytes #"abc"))
(bytes->hex-string (sha224-bytes (open-input-string "xabcy") 1 4))
]

@history[#:added "7.0.0.5"]}

@; ----------------------------------------------------------------------

@close-eval[sha-eval]
