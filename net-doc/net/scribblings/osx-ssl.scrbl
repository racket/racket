#lang scribble/doc
@(require "common.rkt"
          (for-label net/osx-ssl
                     openssl))

@title[#:tag "osx-ssl"]{OS X Native SSL: Secure Communication}

@defmodule[net/osx-ssl]{The @racketmodname[net/osx-ssl] module
offers a fraction of the functionality of @racketmodname[openssl] and
works only on OS X, but it has the advantage that it works before
OpenSSL libraries are installed.}

@history[#:added "6.3.0.12"]

@defproc[(osx-ssl-connect [hostname string?]
                          [port-no (integer-in 1 65535)]
                          [client-protocol
                           (or/c 'secure 'auto
                                 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12)
                           'auto])
         (values input-port?
                 (and/c output-port? osx-ssl-output-port?))]{

Like @racket[ssl-connect], but without support for client contexts.}


@defproc[(osx-ssl-abandon-port [in osx-ssl-output-port?]) void?]{

Analogous to @racket[ssl-abandon-port].}


@defproc[(osx-ssl-output-port? [v any/c]) boolean?]{

Returns @racket[#t] of @racket[v] is an SSL output port produced by
@racket[osx-ssl-connect].}


@defproc[(osx-old-openssl?) boolean?]{

Returns @racket[#t] if the OpenSSL library currently accessed by
@racketmodname[openssl] is too old, in which case
@racket[osx-ssl-connect] should be preferred.}
