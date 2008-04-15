#lang scribble/doc
@(require "common.ss"
          (for-label net/dns
                     net/dns-unit
                     net/dns-sig))

@title[#:tag "dns"]{DNS: Domain Name Service Queries}

@defmodule[net/dns]{The @schememodname[net/dns] library provides
utilities for looking up hostnames.

Thanks to Eduardo Cavazos and Jason Crowe for repairs and
improvements.}

@; ----------------------------------------

@section[#:tag "dns-proc"]{Functions}

@defproc[(dns-get-address [nameserver string?]
                          [address string?])
         string?]{

Consults the specified nameserver (normally a numerical address like
@scheme["128.42.1.30"]) to obtain a numerical address for the given
Internet address.

The query record sent to the DNS server includes the "recursive" bit,
but @scheme[dns-get-address] also implements a recursive search itself
in case the server does not provide this optional feature.}


@defproc[(dns-get-name [nameserver string?]
                       [address string?])
         string?]{

Consults the specified nameserver (normally a numerical address like
@scheme["128.42.1.30"]) to obtain a name for the given numerical
address.}


@defproc[(dns-get-mail-exchanger [nameserver string?]
                                 [address string?])
         string?]{

Consults the specified nameserver to obtain the address for a mail
exchanger the given mail host address. For example, the mail exchanger
for @scheme["ollie.cs.rice.edu"] might be @scheme["cs.rice.edu"].}



@defproc[(dns-find-nameserver) (or/c string? false/c)]{

Attempts to find the address of a nameserver on the present system.
Under Unix, this procedure parses @filepath{/etc/resolv.conf} to
extract the first nameserver address. Under Windows, it runs
@exec{nslookup.exe}.}

@; ----------------------------------------

@section{DNS Unit}

@defmodule[net/dns-unit]

@defthing[dns@ unit?]{

Imports nothing, exports @scheme[dns^].}

@; ----------------------------------------

@section{DNS Signature}

@defmodule[net/dns-sig]

@defsignature[dns^ ()]{}

Includes everything exported by the @schememodname[net/dns] module.
