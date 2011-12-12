#lang scribble/doc
@(require "web-server.rkt"
          (for-label web-server/http
                     net/url
                     web-server/servlet/setup
                     web-server/configuration/responders
                     web-server/private/servlet
                     racket/date
                     web-server/private/util
                     web-server/private/connection-manager
                     unstable/contract)
          (for-syntax racket/base))

@title[#:tag "dispatchers"
       #:style 'toc]{Dispatchers}

Since the @web-server is really just a particular configuration of a
dispatching server, there are several dispatchers that are defined
to support the @|web-server|. Other dispatching servers may find these useful. In particular, if you want
a peculiar processing pipeline for your @web-server installation, refer to this
documentation.

@local-table-of-contents[]

@; ------------------------------------------------------------
@section[#:tag "dispatch"]{General}
@(require (for-label web-server/dispatchers/dispatch))

@defmodule[web-server/dispatchers/dispatch]{

This module provides a few functions for dispatchers in general.

@defthing[dispatcher/c contract?]{
 Equivalent to @racket[(connection? request? . -> . void)].
}

@defproc[(dispatcher-interface-version/c (any any/c)) boolean?]{
 Equivalent to @racket[(symbols 'v1)]
}

@defstruct[exn:dispatcher ()]{
 An exception thrown to indicate that a dispatcher does not apply to a particular
 request.
}

@defproc[(next-dispatcher) void]{
 Raises a @racket[exn:dispatcher]
}

As the @racket[dispatcher/c] contract suggests, a dispatcher is a function that takes a connection
and request object and does something to them. Mostly likely it will generate
some response and output it on the connection, but it may do something
different. For example, it may apply some test to the request object, perhaps
checking for a valid source IP address, and error if the test is not passed, and call @racket[next-dispatcher]
otherwise.

Consider the following example dispatcher, that captures the essence of URL rewriting:
@racketblock[
 (code:comment "(url? -> url?) dispatcher/c -> dispatcher/c")
 (lambda (rule inner)
   (lambda (conn req)
     (code:comment "Call the inner dispatcher...")
     (inner conn
            (code:comment "with a new request object...")
            (struct-copy request req
                         (code:comment "with a new URL!")
                         [request-uri (rule (request-uri req))]))))
]

}

@; ------------------------------------------------------------
@section[#:tag "filesystem-map"]{Mapping URLs to Paths}
@(require (for-label web-server/dispatchers/filesystem-map))

@defmodule[web-server/dispatchers/filesystem-map]{

This module provides a means of mapping
URLs to paths on the filesystem.

@defthing[url->path/c contract?]{
 This contract is equivalent to @racket[((url?) . ->* . (path? (listof path-piece?)))].
 The returned @racket[path?] is the path on disk. The list is the list of
 path elements that correspond to the path of the URL.}

@defproc[(make-url->path (base path-string?))
         url->path/c]{
 The @racket[url->path/c] returned by this procedure considers the root
 URL to be @racket[base]. It ensures that @racket[".."]s in the URL
 do not escape the @racket[base] and removes them silently otherwise.}

@defproc[(make-url->valid-path (url->path url->path/c))
         url->path/c]{
 Runs the underlying @racket[url->path], but only returns if the path
 refers to a file that actually exists. If it is does not, then the suffix
 elements of the URL are removed until a file is found. If this never occurs,
 then an error is thrown.

 This is primarily useful for dispatchers that allow path information after
 the name of a service to be used for data, but where the service is represented
 by a file. The most prominent example is obviously servlets.}

@defproc[(filter-url->path [regex regexp?]
                                 [url->path url->path/c])
         url->path/c]{
 Runs the underlying @racket[url->path] but will only return if the
 path, when considered as a string, matches the @racket[regex]. This is
 useful to disallow strange files, like GIFs, from being considered
 servlets when using the servlet dispatchers. It will return a
 @racket[exn:fail:filesystem:exists?] exception if the path does not
 match.
}

}

@; ------------------------------------------------------------
@section[#:tag "dispatch-sequencer"]{Sequencing}
@a-dispatcher[web-server/dispatchers/dispatch-sequencer
              @elem{defines a dispatcher constructor
                 that invokes a sequence of dispatchers until one applies.}]{

@defproc[(make (dispatcher dispatcher/c) ...)
         dispatcher/c]{
 Invokes each @racket[dispatcher], invoking the next if the first
 calls @racket[next-dispatcher]. If no @racket[dispatcher] applies,
 then it calls @racket[next-dispatcher] itself.
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-timeout"]{Timeouts}
@a-dispatcher[web-server/dispatchers/dispatch-timeout
               @elem{defines a dispatcher constructor
                  that changes the timeout on the connection and calls the next
                  dispatcher.}]{

@defproc[(make [new-timeout integer?])
         dispatcher/c]{
 Changes the timeout on the connection with @racket[adjust-connection-timeout!]
 called with @racket[new-timeout].
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-lift"]{Lifting Procedures}
@a-dispatcher[web-server/dispatchers/dispatch-lift
              @elem{defines a dispatcher constructor.}]{

@defproc[(make (proc (request? . -> . response?)))
         dispatcher/c]{
 Constructs a dispatcher that calls @racket[proc] on the request
 object, and outputs the response to the connection.
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-filter"]{Filtering Requests}
@a-dispatcher[web-server/dispatchers/dispatch-filter
              @elem{defines a dispatcher constructor
                 that calls an underlying dispatcher
                 with all requests that pass a predicate.}]{

@defproc[(make (regex regexp?) (inner dispatcher/c))
         dispatcher/c]{
 Calls @racket[inner] if the URL path of the request, converted to
 a string, matches @racket[regex]. Otherwise, calls @racket[next-dispatcher].
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-pathprocedure"]{Procedure Invocation upon Request}
@a-dispatcher[web-server/dispatchers/dispatch-pathprocedure
              @elem{defines a dispatcher constructor
                   for invoking a particular procedure when a request is given to a particular
                   URL path.}]{

@defproc[(make (path string?) (proc (request? . -> . response?)))
         dispatcher/c]{
 Checks if the request URL path as a string is equal to @racket[path]
 and if so, calls @racket[proc] for a response.
}

This is used in the standard @web-server pipeline to provide
a URL that refreshes the password file, servlet cache, etc.}

@; ------------------------------------------------------------
@section[#:tag "dispatch-log"]{Logging}
@a-dispatcher[web-server/dispatchers/dispatch-log
              @elem{defines a dispatcher constructor
                    for transparent logging of requests.}]{

@defthing[format-req/c contract?]{
 Equivalent to @racket[(request? . -> . string?)].
}

@defthing[paren-format format-req/c]{
 Formats a request by:
 @racketblock[
  (format 
   "~s\n"
   (list 'from (request-client-ip req)
         'to (request-host-ip req)
         'for (url->string (request-uri req)) 'at
         (date->string
          (seconds->date (current-seconds)) #t)))
  ]}

@defthing[extended-format format-req/c]{
 Formats a request by:
 @racketblock[
  (format 
   "~s\n"
   `((client-ip ,(request-client-ip req))
     (host-ip ,(request-host-ip req))
     (referer 
      ,(let ([R (headers-assq* 
                 #"Referer"
                 (request-headers/raw req))])
         (if R
             (header-value R)
             #f)))
     (uri ,(url->string (request-uri req)))
     (time ,(current-seconds))))
 ]}

@defthing[apache-default-format format-req/c]{

 Formats a request like Apache's default. However, Apache's default
 includes information about the response to a request, which this
 function does not have access to, so it defaults the last two fields
 to 200 and 512.

}

@defthing[log-format/c contract?]{
 Equivalent to @racket[(symbols 'parenthesized-default 'extended 'apache-default)].
}

@defproc[(log-format->format [id log-format/c])
         format-req/c]{
 Maps @racket['parenthesized-default] to @racket[paren-format],
 @racket['extended] to @racket[extended-format], and
 @racket['apache-default] to @racket[apache-default-format].
}

@defproc[(make [#:format format format-req/c paren-format]
               [#:log-path log-path path-string? "log"])
         dispatcher/c]{
 Logs requests to @racket[log-path] by using @racket[format] to format the requests.
 Then invokes @racket[next-dispatcher].
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-passwords"]{Password Protection}
@a-dispatcher[web-server/dispatchers/dispatch-passwords
              @elem{defines a dispatcher constructor
                    that performs HTTP Basic authentication filtering.}]{

@(require (for-label web-server/http
                     net/url
                     web-server/configuration/responders))

@defthing[denied?/c contract?]{
  Equivalent to @racket[(request? . -> . (or/c false/c string?))].  The
  return is the authentication realm as a string if the request is not
  authorized and @racket[#f] if the request @emph{is} authorized.
}

@defproc[(make [denied? denied?/c]
               [#:authentication-responder
                authentication-responder
                (url? header? . -> . response?)
                (gen-authentication-responder "forbidden.html")])
         dispatcher/c]{
  A dispatcher that checks if the request is denied based on
  @racket[denied?]. If so, then @racket[authentication-responder] is
  called with a @racket[header] that requests credentials. If not, then
  @racket[next-dispatcher] is invoked.
}

@defthing[authorized?/c contract?]{
 Equivalent to
 @racket[(string? (or/c false/c bytes?) (or/c false/c bytes?) . -> . (or/c false/c string?))].
 The input is the URI as a string and the username and passwords as
 bytes.  The return is the authentication realm as a string if the user
 is not authorized and @racket[#f] if the request @emph{is} authorized.
}

@defproc[(make-basic-denied?/path [authorized? authorized?/c])
                                  denied?/c]{
Creates a denied procedure from an authorized procedure.
}

@defproc[(password-file->authorized? [password-file path-string?])
         (values (-> void)
                 authorized?/c)]{
 Creates an authorization procedure based on the given password
 file. The first returned value is a procedure that refreshes the
 password cache used by the authorization procedure.

 @racket[password-file] is parsed as:
 @racketblock[(list ([domain : string?]
                     [path : string?] (code:comment "This string is interpreted as a regex")
                     (list [user : symbol?]
                           [pass : string?])
                     ...)
                    ...)]
 For example:
 @racketblock['(("secret stuff" "/secret(/.*)?" (bubba "bbq") (|Billy| "BoB")))]
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-host"]{Virtual Hosts}
@a-dispatcher[web-server/dispatchers/dispatch-host
              @elem{defines a dispatcher constructor
                    that calls a different dispatcher based upon the host requested.}]{

@defproc[(make (lookup-dispatcher (symbol? . -> . dispatcher/c)))
         dispatcher/c]{
 Extracts a host from the URL requested, or the Host HTTP header,
 calls @racket[lookup-dispatcher] with the host, and invokes the
 returned dispatcher. If no host can be extracted, then @racket['none]
 is used.
}}

@; ------------------------------------------------------------
@section[#:tag "dispatch-files"]{Serving Files}
@a-dispatcher[web-server/dispatchers/dispatch-files
             @elem{allows files to be served.
                It defines a dispatcher construction procedure.}]{

@defproc[(make [#:url->path url->path url->path/c]
               [#:path->mime-type path->mime-type (path? . -> . (or/c false/c bytes)?) (lambda (path) #f)]
               [#:indices indices (listof string?) (list "index.html" "index.htm")])
         dispatcher/c]{
 Uses @racket[url->path] to extract a path from the URL in the request
 object. If this path does not exist, then the dispatcher does not apply and 
 @racket[next-dispatcher] is invoked.
 If the path is a directory, then the @racket[indices] are checked in order
 for an index file to serve. In that case, or in the case of a path that is
 a file already, @racket[path->mime-type] is consulted for the MIME
 Type of the path. The file is then
 streamed out the connection object.

 This dispatcher supports HTTP Range GET requests and HEAD requests.}}

@; ------------------------------------------------------------
@include-section["dispatch-servlets.scrbl"]

@; ------------------------------------------------------------
@section[#:tag "dispatch-stat"]{Statistics}
@a-dispatcher[web-server/dispatchers/dispatch-stat
              @elem{provides services related to performance
                    statistics.}]{

@defproc[(make-gc-thread [time integer?])
         thread?]{
 Starts a thread that calls @racket[(collect-garbage)] every @racket[time] seconds.
}
                 
@defproc[(make)
         dispatcher/c]{
 Returns a dispatcher that prints memory usage on every request.
}}

@; ------------------------------------------------------------
@section[#:tag "limit"]{Limiting Requests}
@a-dispatcher[web-server/dispatchers/limit
              @elem{provides a wrapper dispatcher that limits how many requests are serviced at once.}]{

@defproc[(make [limit number?]
               [inner dispatcher/c]
               [#:over-limit over-limit (symbols 'block 'kill-new 'kill-old) 'block])
         dispatcher/c]{
 Returns a dispatcher that defers to @racket[inner] for work, but will forward a maximum of @racket[limit] requests concurrently.
         
 If there are no additional spaces inside the limit and a new request is received, the @racket[over-limit] option determines what is done.
 The default (@racket['block]) causes the new request to block until an old request is finished being handled.
 If @racket[over-limit] is @racket['kill-new], then the new request handler is killed---a form of load-shedding.
 If @racket[over-limit] is @racket['kill-old], then the oldest request handler is killed---prioritizing new connections over old.
 (This setting is a little dangerous because requests might never finish if there is constant load.)
}}

@(require (for-label
           web-server/web-server
           web-server/http
           (prefix-in limit: web-server/dispatchers/limit)
           (prefix-in filter: web-server/dispatchers/dispatch-filter)
           (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)))

Consider this example:
@racketmod[
 racket
 
(require web-server/web-server
         web-server/http
         web-server/http/response
         (prefix-in limit: web-server/dispatchers/limit)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer))

(serve #:dispatch
       (sequencer:make
        (filter:make
         #rx"/limited"
         (limit:make
          5
          (lambda (conn req)
            (output-response/method
             conn
             (response/full
              200 #"Okay"
              (current-seconds) TEXT/HTML-MIME-TYPE
              empty
              (list (string->bytes/utf-8
                     (format "hello world ~a"
                            (sort (build-list 100000 (λ x (random 1000)))
                                  <)))))
             (request-method req)))
          #:over-limit 'block))
        (lambda (conn req)          
          (output-response/method
           conn
           (response/full 200 #"Okay"
                          (current-seconds) TEXT/HTML-MIME-TYPE
                          empty
                          (list #"<html><body>Unlimited</body></html>"))
           (request-method req))))
       #:port 8080)

(do-not-return)
]
