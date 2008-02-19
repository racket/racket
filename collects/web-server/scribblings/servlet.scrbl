#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "servlet"
       #:style 'toc]{Scheme Servlets}

The @web-server allows servlets to be written in Scheme. It
provides the supporting API, described below, for the construction
of these servlets. This API is provided by @filepath{servlet.ss}.

@local-table-of-contents[]

@; ------------------------------------------------------------
@section[#:tag "module-servlets"]{Definition}
@(require (for-label "dummy-servlet.ss")) @; to give a binding context

@declare-exporting[#:use-sources (web-server/scribblings/dummy-servlet)]

A @defterm{servlet} is a module that provides the following:

@defthing[interface-version (one-of/c 'v1 'v2)]{
 A symbol indicating the servlet interface the servlet conforms
 to. This influences the other provided identifiers.
}

@defthing[timeout integer?]{
 Only if @scheme[interface-version] is @scheme['v1].

 This number is used as the @scheme[continuation-timeout] argument to
 a timeout-based continuation manager used for this servlet. (See
 @secref["timeouts.ss"].) (i.e., you do not have a choice of the manager
 for this servlet and will be given a timeout-based manager.)
}

@defthing[manager manager?]{
 Only if @scheme[interface-version] is @scheme['v2].

 The manager for the continuations of this servlet.
}

@defproc[(start [initial-request request?])
         response?]{
 This function is called when an instance of this servlet is started.
 The argument is the HTTP request that initiated the instance.
}

@; ------------------------------------------------------------
@section[#:tag "servlet-structs.ss"]{Contracts}
@(require (for-label web-server/servlet/servlet-structs))

@defmodule[web-server/servlet/servlet-structs]

@filepath{servlet/servlet-structs.ss} provides a number of contracts
for use in servlets.

@defthing[k-url? contract?]{Equivalent to @scheme[string?].}

@defthing[response-generator? contract?]{Equivalent to @scheme[(k-url? . -> . response?)].}

@defthing[url-transform? contract?]{Equivalent to @scheme[(k-url? . -> . k-url?)].}

@defthing[expiration-handler? contract?]{Equivalent to @scheme[(or/c false/c (request? . -> . response?))].}

@defthing[embed/url? contract?]{Equivalent to @scheme[(((request? . -> . any/c)) (expiration-handler?) . opt-> . string?)].}

@; ------------------------------------------------------------
@section[#:tag "request-structs.ss"]{HTTP Requests}
@(require (for-label web-server/private/request-structs))

@defmodule[web-server/private/request-structs]

@; XXX Create http sub-directory
@; XXX Have this include read-request and write-response
@filepath{private/request-structs.ss} provides a number of structures and functions
related to HTTP request data structures.

@defstruct[header ([field bytes?]
                   [value bytes?])]{
 Represents a header of @scheme[field] to @scheme[value].
}

@defproc[(headers-assq [id bytes?] [heads (listof header?)])
         (or/c false/c header?)]{
 Returns the header with a field equal to @scheme[id] from @scheme[heads] or @scheme[#f].
}

@defproc[(headers-assq* [id bytes?] [heads (listof header?)])
         (or/c false/c header?)]{
 Returns the header with a field case-insensitively equal to @scheme[id] from @scheme[heads] or @scheme[#f].
}

@defstruct[binding ([id bytes?])]{Represents a binding of @scheme[id].}

@defstruct[(binding:form binding) ([value bytes?])]{
 Represents a form binding of @scheme[id] to @scheme[value].
}

@defstruct[(binding:file binding) ([filename bytes?]
                                   [content bytes?])]{
 Represents the uploading of the file @scheme[filename] with the id @scheme[id]
 and the content @scheme[content].
}

@defproc[(bindings-assq [binds (listof binding?)])
         (or/c false/c binding?)]{
 Returns the binding with an id equal to @scheme[id] from @scheme[binds] or @scheme[#f].
}

@defstruct[request ([method symbol?]
                    [uri url?]
                    [headers/raw (listof header?)]
                    [bindings/raw (listof binding?)]
                    [post-data/raw (or/c false/c bytes?)]
                    [host-ip string?]
                    [host-port number?]
                    [client-ip string?])]{
 An HTTP @scheme[method] request to @scheme[uri] from @scheme[client-ip]
 to the server at @scheme[host-ip]:@scheme[host-port] with @scheme[headers/raw]
 headers, @scheme[bindings/raw] GET and POST queries and @scheme[post-data/raw]
 POST data.
}

@; ------------------------------------------------------------
@section[#:tag "bindings.ss"]{Request Bindings}
@(require (for-label web-server/servlet/bindings))

@defmodule[web-server/servlet/bindings]

@filepath{servlet/bindings.ss} provides a number of helper functions
for accessing request bindings.

@defproc[(request-bindings [req request?])
         (listof (or/c (cons/c symbol? string?)
                       (cons/c symbol? bytes?)))]{
 Translates the @scheme[request-bindings/raw] of @scheme[req] by
 interpreting @scheme[bytes?] as @scheme[string?]s, except in the case
 of @scheme[binding:file] bindings, which are left as is. Ids are then
 translated into lowercase symbols.
}

@defproc[(request-headers [req request?])
         (listof (cons/c symbol? string?))]{
 Translates the @scheme[request-headers/raw] of @scheme[req] by
 interpreting @scheme[bytes?] as @scheme[string?]s. Ids are then
 translated into lowercase symbols.
}

@defproc[(extract-binding/single [id symbol?]
                                 [binds (listof (cons/c symbol? string?))])
         string?]{
 Returns the single binding associated with @scheme[id] in the a-list @scheme[binds]
 if there is exactly one binding. Otherwise raises @scheme[exn:fail].
}

@defproc[(extract-bindings [id symbol?]
                           [binds (listof (cons/c symbol? string?))])
         (listof string?)]{
 Returns a list of all the bindings of @scheme[id] in the a-list @scheme[binds].
}

@defproc[(exists-binding? [id symbol?]
                          [binds (listof (cons/c symbol? string))])
         boolean?]{
 Returns @scheme[#t] if @scheme[binds] contains a binding for @scheme[id].
 Otherwise, @scheme[#f].
}

These functions, while convenient, could introduce subtle bugs into your
application. Examples: that they are case-insensitive could introduce
a bug; if the data submitted is not in UTF-8 format, then the conversion
to a string will fail; if an attacked submits a form field as if it were
a file, when it is not, then the @scheme[request-bindings] will hold a
@scheme[bytes?] object and your program will error; and, for file uploads
you lose the filename.

@; ------------------------------------------------------------
@section[#:tag "response-structs.ss"]{HTTP Responses}
@(require (for-label web-server/private/response-structs))

@defmodule[web-server/private/response-structs]

@filepath{private/response-structs.ss} provides structures and functions related to
HTTP responses.

@; XXX Only use bytes
@defstruct[response/basic
           ([code number?]
            [message string?]
            [seconds number?]
            [mime bytes?]
            [headers (listof header?)])]{
 A basic HTTP response containing no body. @scheme[code] is the response code,
 @scheme[message] the message, @scheme[seconds] the generation time, @scheme[mime]
 the MIME type of the file, and @scheme[extras] are the extra headers, in addition
 to those produced by the server.
}

@; XXX Rename string? option
@defstruct[(response/full response/basic)
           ([body (listof (or/c string? bytes?))])]{
 As with @scheme[response/basic], except with @scheme[body] as the response
 body.
}

@defstruct[(response/incremental response/basic)
           ([generator ((() (listof (or/c bytes? string?)) . ->* . any) . -> . any)])]{
 As with @scheme[response/basic], except with @scheme[generator] as a function that is
 called to generate the response body, by being given an @scheme[output-response] function
 that outputs the content it is called with.
}

@defproc[(response? [v any/c])
         boolean?]{
 Checks if @scheme[v] is a valid response. A response is either:
 @itemize[
  @item{A @scheme[response/basic] structure.}
  @item{A value matching the contract @scheme[(cons/c (or/c bytes? string?) (listof (or/c bytes? string?)))].}
  @item{A value matching @scheme[xexpr?].}
 ]
}

@defthing[TEXT/HTML-MIME-TYPE bytes?]{Equivalent to @scheme[#"text/html; charset=utf-8"].}

@warning{If you include a Content-Length header in a response that is inaccurate, there WILL be an error in
transmission that the server will not catch.}

@; ------------------------------------------------------------
@section[#:tag "web.ss"]{Web}
@(require (for-label web-server/servlet/web))

@defmodule[web-server/servlet/web]{The
@schememodname[web-server/servlet/web] library provides the primary
functions of interest for the servlet developer.}

@defproc[(send/back [response response?])
         void?]{
 Sends @scheme[response] to the client.
}

@defthing[current-servlet-continuation-expiration-handler parameter?]{
 Holds the @scheme[expiration-handler?] to be used when a continuation
 captured in this context is expired, then looked up.
}

@defproc[(send/suspend [make-response response-generator?]
                       [exp expiration-handler? (current-servlet-continuation-expiration-handler)])
         request?]{
 Captures the current continuation, stores it with @scheme[exp] as the expiration
 handler, and binds it to a URL. @scheme[make-response] is called with this URL and
 is expected to generate a @scheme[response?], which is sent to the client. If the
 continuation URL is invoked, the captured continuation is invoked and the request is
 returned from this call to @scheme[send/suspend].
}

@defproc[(continuation-url? [u url?])
         (or/c false/c (list/c number? number? number?))]{
 Checks if @scheme[u] is a URL that refers to a continuation, if so
 returns the instance id, continuation id, and nonce.
}

@; XXX Move
@defproc[(adjust-timeout! [t number?])
         void?]{
 Calls the servlet's manager's @scheme[adjust-timeout!] function.
}

@defproc[(clear-continuation-table!)
         void?]{
 Calls the servlet's manager's @scheme[clear-continuation-table!] function.
}

@defproc[(send/forward [make-response response-generator?]
                       [exp expiration-handler? (current-servlet-continuation-expiration-handler)])
         request?]{
 Calls @scheme[clear-continuation-table!], then @scheme[send/suspend].
}

@defproc[(send/finish [response response?])
         void?]{
 Calls @scheme[clear-continuation-table!], then @scheme[send/back].
}

@defproc[(send/suspend/dispatch [make-response (embed/url? . -> . response?)])
         any/c]{
 Calls @scheme[make-response] with a function that, when called with a procedure from
 @scheme[request?] to @scheme[any/c] will generate a URL, that when invoked will call
 the function with the @scheme[request?] object and return the result to the caller of
 @scheme[send/suspend/dispatch].
}

@defproc[(redirect/get)
         request?]{
 Calls @scheme[send/suspend] with @scheme[redirect-to].
}

@defproc[(redirect/get/forget)
         request?]{
 Calls @scheme[send/forward] with @scheme[redirect-to].
}

@; XXX Remove
@defproc[(embed-ids [ids (list/c number? number? number?)]
                    [u url?])
         string?]{
 Creates a @scheme[continuation-url?].
}

@; XXX Remove
@defthing[current-url-transform parameter?]{
 Holds a @scheme[url-transform?] function that is called by
 @scheme[send/suspend] to transform the URLs it generates.
}

@; ------------------------------------------------------------
@section[#:tag "helpers.ss"]{Helpers}
@(require (for-label web-server/servlet/helpers))

@defmodule[web-server/servlet/helpers]

@filepath{servlet/helpers.ss} provides functions built on
@filepath{servlet/web.ss} that are useful in many servlets.

@; XXX Move into http/response.ss
@defproc[(redirect-to [uri string?]
                      [perm/temp redirection-status? temporarily]
                      [#:headers headers (listof header?) (list)])
         response?]{
 Generates an HTTP response that redirects the browser to @scheme[uri],
 while including the @scheme[headers] in the response.
}

@defproc[(redirection-status? [v any/c])
         boolean?]{
 Determines if @scheme[v] is one of the following values.
}

@defthing[permanently redirection-status?]{A @scheme[redirection-status?] for permanent redirections.}

@defthing[temporarily redirection-status?]{A @scheme[redirection-status?] for temporary redirections.}

@defthing[see-other redirection-status?]{A @scheme[redirection-status?] for "see-other" redirections.}

@defproc[(with-errors-to-browser [send/finish-or-back (response? . -> . void?)]
                                 [thunk (-> any)])
         any]{
 Calls @scheme[thunk] with an exception handler that generates an HTML error page
 and calls @scheme[send/finish-or-back].
}

@; XXX Depreciate
@; ------------------------------------------------------------
@section[#:tag "servlet-url.ss"]{Servlet URLs}
@(require (for-label web-server/servlet/servlet-url))

@defmodule[web-server/servlet/servlet-url]

@filepath{servlet/servlet-url.ss} provides functions that might be useful to you.
They may eventually provided by another module.

@defproc[(request->servlet-url (req request?))
         servlet-url?]{Generates a value to be passed to the next function.}

@defproc[(servlet-url->url-string/no-continuation [su servlet-url?])
         string?]{
 Returns a URL string without the continuation information in the URL
 that went into @scheme[su]
}

@; XXX Support Digest
@; ------------------------------------------------------------
@section[#:tag "basic-auth.ss"]{Basic Authentication}
@(require (for-label web-server/servlet/basic-auth))

@defmodule[web-server/servlet/basic-auth]

@filepath{servlet/basic-auth.ss} provides a function for helping with
implementation of HTTP Basic Authentication.

@defproc[(extract-user-pass [heads (listof header?)])
         (or/c false/c (cons/c bytes? bytes?))]{
 Returns a pair of the username and password from the authentication
 header in @scheme[heads] if they are present, or @scheme[#f]
}

@; ------------------------------------------------------------
@section[#:tag "web-cells.ss"]{Web Cells}
@(require (for-label web-server/servlet/web-cells))

@defmodule[web-server/servlet/web-cells]{The
@schememodname[web-server/servlet/web-cells] library provides the
interface to web cells.}

A web cell is a kind of state defined relative to the @defterm{frame tree}.
The frame-tree is a mirror of the user's browsing session. Every time a
continuation is invoked, a new frame (called the @defterm{current frame}) is
created as a child of the current frame when the continuation was captured.

You should use web cells if you want an effect to be encapsulated in all
interactions linked from (in a transitive sense) the HTTP response being
generated. For more information on their semantics, consult the paper 
@href-link["http://www.cs.brown.edu/~sk/Publications/Papers/Published/mk-int-safe-state-web/"
"\"Interaction-Safe State for the Web\""].

@; XXX Document with-frame and with-frame-after?

@defproc[(web-cell? [v any/c])
         boolean?]{
 Determines if @scheme[v] is a web-cell.
}

@defproc[(make-web-cell [v any/c])
         web-cell?]{
 Creates a web-cell with a default value of @scheme[v].
}

@defproc[(web-cell-ref [wc web-cell?])
         any/c]{
 Looks up the value of @scheme[wc] found in the nearest
 frame.
}

@defproc[(web-cell-shadow [wc web-cell?]
                          [v any/c])
         void]{
 Binds @scheme[wc] to @scheme[v] in the current frame, shadowing any
 other bindings to @scheme[wc] in the current frame.
}

@include-section["servlet-env.scrbl"]
