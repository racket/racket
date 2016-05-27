#lang scribble/doc
@(require "common.rkt"
          (for-label net/win32-ssl
                     openssl))

@title[#:tag "win32-ssl"]{Windows Native SSL: Secure Communication}

@defmodule[net/win32-ssl]{The @racketmodname[net/win32-ssl] module
offers a fraction of the functionality of @racketmodname[openssl] and
works only on Windows, but it has the advantage that it works before
OpenSSL libraries are installed.}

@defproc[(win32-ssl-connect [hostname string?]
                            [port-no (integer-in 1 65535)]
                            [client-protocol
                             (or/c 'secure 'auto
                                   'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12)
                             'auto])
         (values (and/c input-port? win32-ssl-port?)
                 (and/c output-port? win32-ssl-port?))]{

Like @racket[ssl-connect], but without support for client contexts.}


@defproc[(win32-ssl-abandon-port [in (and/c win32-ssl-port? output-port?)]) void?]{

Analogous to @racket[ssl-abandon-port].}


@defproc[(ports->win32-ssl-ports
           [input-port input-port?]
	   [output-port output-port?]
	   [#:encrypt protocol (or/c 'secure 'auto
                                     'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12)]
           [#:hostname hostname (or/c #f string?) #f])
         (values (and/c input-port? win32-ssl-port?)
                 (and/c output-port? win32-ssl-port?))]{

Analogous to @racket[ports->ssl-ports].

If hostname verification is enabled via @racket['secure], the peer
server's certificate is checked against @racket[hostname].

@history[#:changed "6.3.0.12" @elem{Added the @racket[#:hostname] argument.}]}


@defproc[(win32-ssl-port? [v any/c]) boolean?]{

Returns @racket[#t] of @racket[v] is an SSL port produced by
@racket[win32-ssl-connect] or
@racket[ports->win32-ssl-ports].}


@defthing[win32-ssl-available? boolean?]{

A boolean value that reports whether the Windows native SSL library was
successfully loaded. Calling @racket[win32-ssl-connect], @|etc| when this
value is @racket[#f] will raise an exception.}
