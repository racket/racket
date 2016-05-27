#lang scribble/doc
@(require "common.rkt" (for-label net/ssl-tcp-unit net/tcp-sig))

@title[#:tag "ssl-tcp-unit"]{SSL Unit: @racket[tcp^] via SSL}

@defmodule[net/ssl-tcp-unit]{The @racketmodname[net/ssl-tcp-unit]
library provides a function for creating a @racket[tcp^]
implementation with @racketmodname[openssl] functionality.}

@defproc[(make-ssl-tcp@ [server-cert-file (or/c path-string? false/c)]
                        [server-key-file (or/c path-string? false/c)]
                        [server-root-cert-files (or/c (listof path-string?) false/c)]
                        [server-suggest-auth-file path-string?]
                        [client-cert-file (or/c path-string? false/c)]
                        [client-key-file (or/c path-string? false/c)]
                        [client-root-cert-files (listof path-string?)])
         unit?]{

Returns a unit that implements @racket[tcp^] using the SSL functions
from @racketmodname[openssl]. The arguments to @racket[make-ssl-tcp@]
control the certificates and keys uses by server and client
connections:

@itemize[

    @item{@racket[server-cert-file] --- a PEM file for a server's
     certificate; @racket[#f] means no certificate (which is unlikely
     to work with any SSL client)}

    @item{@racket[server-key-file] --- a private key PEM to go with
    @racket[server-cert-file]; @racket[#f] means no key (which is likely
    renders a certificate useless)}

    @item{@racket[server-root-cert-files] --- a list of PEM files for
     trusted root certificates; @racket[#f] disables verification of
     peer client certificates}

    @item{@racket[server-suggest-auth-file] --- PEM file for root
     certificates to be suggested to peer clients that must supply
     certificates}

    @item{@racket[client-cert-file] --- a PEM file for a client's
     certificate; @racket[#f] means no certificate (which is usually
     fine)}

    @item{@racket[client-key-file] --- a private key PEM to go with
     @racket[client-cert-file]; @racket[#f] means no key (which is likely
     renders a certificate useless)}

    @item{@racket[client-root-cert-files] --- a list of PEM files for
     trusted root certificates; @racket[#f] disables verification of
     peer server certificates}

]}
