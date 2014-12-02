#lang scribble/doc
@(require "common.rkt" (for-label net/dns net/dns-unit net/dns-sig))

@title[#:tag "dns"]{DNS: Domain Name Service Queries}

@defmodule[net/dns]{The @racketmodname[net/dns] library provides
utilities for looking up hostnames.

Thanks to Eduardo Cavazos and Jason Crowe for repairs and
improvements.}

@; ----------------------------------------

@section[#:tag "dns-proc"]{Functions}

@defproc[(dns-get-address [nameserver string?]
                          [address string?]
                          [#:ipv6? ipv6? any/c #f])
         string?]{

Consults the specified nameserver (normally a numerical address like
@racket["128.42.1.30"]) to obtain a numerical address for the given
Internet address.

The query record sent to the DNS server includes the "recursive" bit,
but @racket[dns-get-address] also implements a recursive search itself
in case the server does not provide this optional feature.

If @racket[ipv6?] is a true value, then the numerical address
that is returned will be an IPv6 address. If no AAAA record exists,
an error will be raised.
}


@defproc[(dns-get-name [nameserver string?]
                       [address string?])
         string?]{

Consults the specified nameserver (normally a numerical address like
@racket["128.42.1.30"]) to obtain a name for the given numerical
address.}


@defproc[(dns-get-mail-exchanger [nameserver string?]
                                 [address string?])
         string?]{

Consults the specified nameserver to obtain the address for a mail
exchanger the given mail host address. For example, the mail exchanger
for @racket["ollie.cs.rice.edu"] might be @racket["cs.rice.edu"].}



@defproc[(dns-find-nameserver) (or/c string? false/c)]{

Attempts to find the address of a nameserver on the present system.
On Unix and Mac OS X, this procedure parses @filepath{/etc/resolv.conf} to
extract the first nameserver address. On Windows, it runs
@exec{nslookup.exe}.}

@; ----------------------------------------

@section{DNS Unit}

@margin-note{@racket[dns@] and @racket[dns^] are deprecated.
They exist for backward-compatibility and will likely be removed in
the future. New code should use the @racketmodname[net/dns] module.}

@defmodule[net/dns-unit]

@defthing[dns@ unit?]{

Imports nothing, exports @racket[dns^].}

@; ----------------------------------------

@section{DNS Signature}

@defmodule[net/dns-sig]

@defsignature[dns^ ()]{}

Includes everything exported by the @racketmodname[net/dns] module.
