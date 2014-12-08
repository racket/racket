#lang scribble/doc
@(require "common.rkt" scribble/eval (for-label file/md5))

@(define md5-eval (make-base-eval))
@interaction-eval[#:eval md5-eval (require file/md5)]

@title[#:tag "md5"]{MD5 Message Digest}

@defmodule[file/md5]

See @racketmodname[openssl/md5] for a faster implementation with a
slightly different interface.

@defproc[(md5 [in (or/c input-port? bytes? string?)]
              [hex-encode? boolean? #t]) bytes?]{

If @racket[hex-encode?] is @racket[#t], produces a byte string containing 32 hexadecimal digits (lowercase)
that is the MD5 hash of the given input stream or byte string. Otherwise produces the 16 byte long byte string
that is the MD5 hash of the given input stream or byte string.

@examples[
#:eval md5-eval
(md5 #"abc")
(md5 #"abc" #f)
]}


@close-eval[md5-eval]
