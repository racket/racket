#lang scribble/doc
@(require "common.rkt" scribble/eval (for-label net/dns net/dns-unit net/dns-sig))

@(define dns-evaluator (make-base-eval))
@(dns-evaluator '(require net/dns))

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


@deftogether[(
@defproc[(dns-get-srv [nameserver string?]
		      [name string?]
		      [service string?]
		      [proto string? "tcp"])
         (listof srv-rr?)]
@defstruct*[srv-rr ([priority (integer-in 0 65535)]
		    [weight (integer-in 0 65535)]
		    [port (integer-in 0 65535)]
		    [target string?]) #:prefab]
)]{

@margin-note{An SRV record is a particular kind of DNS resource record
that maps an abstract service name onto a hostname and port
combination. For more information, see
@hyperlink["https://en.wikipedia.org/wiki/SRV_record"]{the Wikipedia
page on SRV records}.}

Consults the specified nameserver (normally a numerical address like
@racket["128.42.1.30"]) to retrieve the SRV records corresponding to
the given name, service, and protocol. Returns a list of
@racket[srv-rr] structs if any corresponding SRV records are found;
otherwise, returns @racket['()].

If @racket[service] is @racket["X"], @racket[proto] is @racket["Y"],
and @racket[name] is @racket["example.com"], then this will retrieve
any SRV records at the domain name @tt{_X._Y.example.com}.

The query record sent to the DNS server includes the "recursive" bit,
but @racket[dns-get-srv] also implements a recursive search itself
in case the server does not provide this optional feature.

@examples[#:eval dns-evaluator
	  (eval:alts (dns-get-srv (dns-find-nameserver) "racket-lang.org" "xmpp-client")
		     (list (srv-rr 0 0 5222 "xmpp.racket-lang.org")))
	  (eval:alts (dns-get-srv (dns-find-nameserver) "racket-lang.org" "nonexistent-protocol")
		     (list))
	  (eval:alts (dns-get-srv (dns-find-nameserver) "racket-lang.org" "xmpp-client" "tcp")
		     (list (srv-rr 0 0 5222 "xmpp.racket-lang.org")))
	  (eval:alts (dns-get-srv (dns-find-nameserver) "racket-lang.org" "xmpp-client" "udp")
		     (list))
	  ]

@history[#:added "6.4.0.8"]
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

Includes @racket[dns-get-address], @racket[dns-get-name],
@racket[dns-get-mail-exchanger] and @racket[dns-find-nameserver].
