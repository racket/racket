#lang scribble/doc
@(require "common.ss"
          (for-label net/ssl-tcp-unit
                     net/tcp-sig))

@title[#:tag "ssl-tcp-unit"]{SSL Unit: @scheme[tcp^] via SSL}

@defmodule[net/ssl-tcp-unit]{The @schememodname[net/ssl-tcp-unit]
library provides a function for creating a @scheme[tcp^]
implementation with @schememodname[openssl] functionality.}

@defproc[(make-ssl-tcp@ [server-cert-file (or/c path-string? false/c)]
                        [server-key-file (or/c path-string? false/c)]
                        [server-root-cert-files (or/c (listof path-string?) false/c)]
                        [server-suggest-auth-file path-string?]
                        [client-cert-file (or/c path-string? false/c)]
                        [client-key-file (or/c path-string? false/c)]
                        [client-root-cert-files (listof path-string?)])
         unit?]{

Returns a unit that implements @scheme[tcp^] using the SSL functions
from @schememodname[openssl]. The arguments to @scheme[make-ssl-tcp@]
control the certificates and keys uses by server and client
connections:

@itemize[

    @item{@scheme[server-cert-file] --- a PEM file for a server's
     certificate; @scheme[#f] means no certificate (which is unlikely
     to work with any SSL client)}

    @item{@scheme[server-key-file] --- a private key PEM to go with
    @scheme[server-cert-file]; @scheme[#f] means no key (which is likely
    renders a certificate useless)}

    @item{@scheme[server-root-cert-files] --- a list of PEM files for
     trusted root certificates; @scheme[#f] disables verification of
     peer client certificates}

    @item{@scheme[server-suggest-auth-file] --- PEM file for root
     certificates to be suggested to peer clients that must supply
     certificates}

    @item{@scheme[client-cert-file] --- a PEM file for a client's
     certificate; @scheme[#f] means no certificate (which is usually
     fine)}

    @item{@scheme[client-key-file] --- a private key PEM to go with
     @scheme[client-cert-file]; @scheme[#f] means no key (which is likely
     renders a certificate useless)}

    @item{@scheme[client-root-cert-files] --- a list of PEM files for
     trusted root certificates; @scheme[#f] disables verification of
     peer server certificates}

]}