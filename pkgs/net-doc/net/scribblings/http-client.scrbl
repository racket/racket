#lang scribble/doc
@(require "common.rkt" scribble/bnf
          (for-label net/http-client
                     net/win32-ssl
                     racket/tcp
                     racket/list
                     openssl))

@title[#:tag "http-client"]{HTTP Client}

@defmodule[net/http-client]{The @racketmodname[net/http-client] library provides
utilities to use the HTTP protocol.}

@defproc[(http-conn? [x any/c])
         boolean?]{

Identifies an HTTP connection.
                   
}

@defproc[(http-conn-live? [x any/c])
         boolean?]{

Identifies an HTTP connection that is "live", i.e. one that is still
connected to the server.

}

@defproc[(http-conn-liveable? [x any/c])
         boolean?]{

Identifies an HTTP connection that can be made "live", i.e. one for which
@racket[http-conn-send!] is valid. Either the HTTP connection is already
@racket[http-conn-live?], or it can @tech{auto-reconnect}.

}

@defproc[(http-conn)
         http-conn?]{

Returns a fresh HTTP connection.

}

@defproc[(http-conn-open! [hc http-conn?] [host (or/c bytes? string?)]
                          [#:ssl? ssl?  base-ssl?-tnl/c #f]
                          [#:port port (between/c 1 65535) (if ssl? 443 80)]
                          [#:auto-reconnect? auto-reconnect? boolean? #f])
         void?]{

Uses @racket[hc] to connect to @racket[host] on port @racket[port]
using SSL if @racket[ssl?] is not @racket[#f] (using @racket[ssl?] as
an argument to @racket[ssl-connect] to, for example, check
certificates.) If @racket[auto-reconnect?] is @racket[#t], then the HTTP
connection is going to try to @deftech{auto-reconnect} for subsequent requests.
I.e., if the connection is closed when performing @racket[http-conn-send!] or
@racket[http-conn-recv!], then @racket[http-conn-enliven!] is going to be
called on it.

If @racket[hc] is live, the connection is closed.

}

@defproc[(http-conn-open [host (or/c bytes? string?)]
                         [#:ssl? ssl?  base-ssl?-tnl/c #f]
                         [#:port port (between/c 1 65535) (if ssl? 443 80)]
                         [#:auto-reconnect? auto-reconnect? boolean? #f])
         http-conn?]{

Calls @racket[http-conn-open!] with a fresh connection, which is returned.

}

@defproc[(http-conn-close! [hc http-conn?])
         void?]{

Closes @racket[hc] if it is live.

}

@defproc[(http-conn-abandon! [hc http-conn?])
         void?]{

Closes the output side of @racket[hc], if it is live.

}

@defproc[(http-conn-enliven! [hc http-conn?])
         void?]{

Reconnects @racket[hc] to the server, if it is @emph{not} live but it is
configured to @tech{auto-reconnect}.

}

@defproc[(http-conn-send! [hc http-conn-liveable?] [uri (or/c bytes? string?)]
                          [#:version version (or/c bytes? string?) #"1.1"]
                          [#:method method (or/c bytes? string? symbol?) #"GET"]
                          [#:close? close? boolean? #f]
                          [#:headers headers (listof (or/c bytes? string?)) empty]
                          [#:content-decode decodes (listof symbol?) '(gzip)]
                          [#:data data (or/c false/c bytes? string? data-procedure/c) #f])
         void?]{

Sends an HTTP request to @racket[hc] to the URI @racket[uri] using
HTTP version @racket[version], the method @racket[method], and the
additional headers given in @racket[headers] and the additional data
@racket[data]. If @racket[method] is @racket[#"HEAD"] (or
@racket["HEAD"] or @racket['HEAD]), provide the same @racket[method]
when calling @racket[http-conn-recv!] to avoid attempting to receive content.

If @racket[data] is a procedure, it will be called once with a
procedure of one argument, which is a string or
byte string to be written to the request body using
chunked transfer encoding.

If @racket[headers] does not contain an @litchar{Accept-Encoding}
header, then a header indicating that encodings from @racket[decodes]
are accepted is automatically added.

If @racket[close?] is @racket[#t] and @racket[headers] does not
contain a @litchar{Connection} header, then a @litchar{Connection:
close} header will be added.

This function does not support requests that expect
@litchar{100 (Continue)} responses.

}

@defproc[(http-conn-recv! [hc http-conn-liveable?]
                          [#:content-decode decodes (listof symbol?) '(gzip)]
                          [#:method method (or/c bytes? string? symbol?) #"GET"]
                          [#:close? close? boolean? #f])
         (values bytes? (listof bytes?) input-port?)]{

Parses an HTTP response from @racket[hc] for the method
@racket[method] while decoding the encodings listed in
@racket[decodes].

Returns the status line, a list of headers, and an port which contains
the contents of the response. The port's content must be consumed
before the connection is used further.

If @racket[close?] is @racket[#t], then the connection will be closed
following the response parsing. If @racket[close?] is @racket[#f],
then the connection is only closed if the server instructs the client
to do so.

@history[#:changed "6.1.1.6" @elem{Added the @racket[#:method] argument.}]}


@defproc[(http-conn-sendrecv! [hc http-conn-liveable?] [uri (or/c bytes? string?)]
                              [#:version version (or/c bytes? string?) #"1.1"]
                              [#:method method (or/c bytes? string? symbol?) #"GET"]
                              [#:headers headers (listof (or/c bytes? string?)) empty]
                              [#:data data (or/c false/c bytes? string? data-procedure/c) #f]
                              [#:content-decode decodes (listof symbol?) '(gzip)]
                              [#:close? close? boolean? #f])
         (values bytes? (listof bytes?) input-port?)]{

Calls @racket[http-conn-send!] and @racket[http-conn-recv!] in sequence.

}

@defproc[(http-sendrecv [host (or/c bytes? string?)] [uri (or/c bytes? string?)]
                        [#:ssl? ssl? base-ssl?-tnl/c #f]
                        [#:port port (between/c 1 65535) (if ssl? 443 80)]
                        [#:version version (or/c bytes? string?) #"1.1"]                          
                        [#:method method (or/c bytes? string? symbol?) #"GET"]
                        [#:headers headers (listof (or/c bytes? string?)) empty]
                        [#:data data (or/c false/c bytes? string? data-procedure/c) #f]
                        [#:content-decode decodes (listof symbol?) '(gzip)])
         (values bytes? (listof bytes?) input-port?)]{

Calls @racket[http-conn-send!] and @racket[http-conn-recv!] in
sequence on a fresh HTTP connection produced by
@racket[http-conn-open].

The HTTP connection is not returned, so it is always closed after one
response, which is why there is no @racket[#:closed?] argument like
@racket[http-conn-recv!].

}

@defproc[(http-conn-CONNECT-tunnel [proxy-host (or/c bytes? string?)]
                                   [proxy-port (between/c 1 65535)]
                                   [target-host (or/c bytes? string?)]
                                   [target-port (between/c 1 65535)]
                                   [#:ssl? ssl? base-ssl?/c #f])
         (values base-ssl?/c input-port? output-port? (-> port? void?))]{
Creates an HTTP connection to @racket[proxy-host] (on port @racket[proxy-port])
 and invokes the HTTP ``CONNECT'' method to provide a tunnel to
 @racket[target-host] (on port @racket[target-port]).

 The SSL context or symbol, if any, provided in @racket[ssl?]
 is applied to the gateway ports using @racket[ports->ssl-ports] (or @racket[ports->win32-ssl-ports]).

 The function returns four values:
 @itemize[
 @item{If @racket[ssl?] was @racket[#f] then @racket[#f]. Otherwise an @racket[ssl-client-context?]
       that has been negotiated with the target.
       
   If @racket[ssl?] was a protocol symbol, then a new @racket[ssl-client-context?] is created,
   otherwise the current value of @racket[ssl?] is used}
 @item{An @racket[input-port?] from the tunnelled service}
 @item{An @racket[output-port?] to the tunnelled service}
 @item{An abandon function, which when applied either returned port, will abandon it, in a manner
   similar to @racket[tcp-abandon-port]}
 ]
 The SSL context or symbol, if any, provided in @racket[ssl?]
 is applied to the gateway ports using @racket[ports->ssl-ports] (or @racket[ports->win32-ssl-ports])
 and the negotiated client context is returned
}

@defthing[data-procedure/c chaperone-contract?]{

Contract for a procedure that accepts a procedure of one
argument, which is a string or byte string:
@racket[(-> (-> (or/c bytes? string?) void?) any)].

}

@defthing[base-ssl?/c contract?]{
 Base contract for the definition of the SSL context (passed in @racket[ssl?]) of an
 @racket[http-conn-CONNECT-tunnel]:
 
 @racket[(or/c boolean? ssl-client-context? symbol?)].

 If @racket[ssl?] is not @racket[#f] then @racket[ssl?] is used as an argument to
 @racket[ssl-connect] to, for example, check certificates.
}

@defthing[base-ssl?-tnl/c contract?]{
 Contract for a @racket[base-ssl?/c] that might have been applied to a tunnel.
 It is either a @racket[base-ssl?/c], or a @racket[base-ssl?/c] consed onto a list of an
 @racket[input-port?], @racket[output-port?], and an abandon function
 (e.g. @racket[tcp-abandon-port]):
 
 @racket[(or/c base-ssl?/c (list/c base-ssl?/c input-port? output-port? (-> port? void?)))]
}

@section[#:tag "faq"]{Troubleshooting and Tips}

@subsection{How do I send properly formatted POST form requests?}

You should send a @litchar{Content-Type} header with the value
@litchar{application/x-www-form-urlencoded} and send the data
formatted by @racketmodname[net/uri-codec]'s
@racket[form-urlencoded-encode] function. For example,

@(require (for-label net/uri-codec))
@racketblock[
(http-conn-send!
   hc "/login"
   #:method "POST"
   #:data
   (alist->form-urlencoded
    (list (cons 'username "Ryu")
          (cons 'password "Sheng Long")))
   #:headers (list "Content-Type: application/x-www-form-urlencoded"))             
]
