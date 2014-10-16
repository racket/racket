#lang scribble/doc
@(require "common.rkt" scribble/bnf
          (for-label net/http-client
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

Identifies an HTTP connection that is "live", i.e. one for which
@racket[http-conn-send!] is valid.

}

@defproc[(http-conn)
         http-conn?]{

Returns a fresh HTTP connection.

}

@defproc[(http-conn-open! [hc http-conn?] [host (or/c bytes? string?)]
                          [#:ssl? ssl? (or/c boolean? ssl-client-context? symbol?) #f]
                          [#:port port (between/c 1 65535) (if ssl? 443 80)])
         void?]{

Uses @racket[hc] to connect to @racket[host] on port @racket[port]
using SSL if @racket[ssl?] is not @racket[#f] (using @racket[ssl?] as
an argument to @racket[ssl-connect] to, for example, check
certificates.)

If @racket[hc] is live, the connection is closed.

}

@defproc[(http-conn-open [host (or/c bytes? string?)]
                         [#:ssl? ssl? (or/c boolean? ssl-client-context? symbol?) #f]
                         [#:port port (between/c 1 65535) (if ssl? 443 80)])
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

@defproc[(http-conn-send! [hc http-conn-live?] [uri (or/c bytes? string?)]
                          [#:version version (or/c bytes? string?) #"1.1"]
                          [#:method method (or/c bytes? string? symbol?) #"GET"]
                          [#:close? close? boolean? #f]
                          [#:headers headers (listof (or/c bytes? string?)) empty]
                          [#:content-decode decodes (listof symbol?) '(gzip)]
                          [#:data data (or/c false/c bytes? string? data-procedure/c) #f])
         void?]{

Sends an HTTP request to @racket[hc] to the URI @racket[uri] using
HTTP version @racket[version] the method @racket[method] and the
additional headers given in @racket[headers] and the additional data
@racket[data].

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

@defproc[(http-conn-recv! [hc http-conn-live?]
                          [#:content-decode decodes (listof symbol?) '(gzip)]
                          [#:close? close? boolean? #f])
         (values bytes? (listof bytes?) input-port?)]{

Parses an HTTP response from @racket[hc], while decoding the encodings
listed in @racket[decodes].

Returns the status line, a list of headers, and an port which contains
the contents of the response.

If @racket[close?] is @racket[#t], then the connection will be closed
following the response parsing. If @racket[close?] is @racket[#f],
then the connection is only closed if the server instructs the client
to do so.

}

@defproc[(http-conn-sendrecv! [hc http-conn-live?] [uri (or/c bytes? string?)]
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
                        [#:ssl? ssl? (or/c boolean? ssl-client-context? symbol?) #f]
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

@defthing[data-procedure/c chaperone-contract?]{

Contract for a procedure that accepts a procedure of one
argument, which is a string or byte string:
@racket[(-> (-> (or/c bytes? string?) void?) any)].

}

@section[#:tag "faq"]{Troubleshooting and Tips}

@subsection{How do I send properly formatted POST form requests?}

You should send a @litchar{Content-Type} header with the value
@litchar{application/x-www-form-urlencoded} and send the data
formatted by @racketmodname[net/uri-codec]'s
@racket[form-urlencoded-encode] function. For example,

@(require (for-label net/uri-codec))
@racketblock[
(http-send!
   hc "/login"
   #:method "POST"
   #:data
   (alist->form-urlencoded
    (list (cons 'username "Ryu")
          (cons 'password "Sheng Long")))
   #:headers (list "Content-Type: application/x-www-form-urlencoded"))             
]
