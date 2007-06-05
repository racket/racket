#reader(lib "docreader.ss" "scribble")
@require["../web-server.ss"]

@title[#:style 'toc]{Scheme Servlets}

The @file{web-server} allows servlets to be written in Scheme. It
provides the supporting API, described below, for the construction
of these servlets. This API is provided by @file{servlet.ss}.

@local-table-of-contents[]

@; ------------------------------------------------------------
@section[#:tag "module-servlets"]{Definition}

A @defterm{servlet} is a module that provides the following:

@defthing[interface-version symbol?]{
 A symbol indicating the servlet interface the servlet conforms
 to. This influences the other provided identifiers.
}

If @scheme[interface-version] is @scheme['v1], then the servlet 
provides:

@defthing[timeout integer?]{
 This number is used as the @scheme[continuation-timeout] argument to
 a timeout-based continuation manager used for this servlet. (See
 @secref["timeouts.ss"].)
}

@defproc[(start [initial-request request?])
         response?]{
 This function is called when an instance of this servlet is started.
 The argument is the HTTP request that initiated the instance.
}

If @scheme[interface-version] is @scheme['v2], then the servlet 
provides:

@defthing[manager manager?]{
 The manager for the continuations of this servlet.
}

@defproc[(start [initial-request request?])
         response?]{
 This function is called when an instance of this servlet is started.
 The argument is the HTTP request that initiated the instance.
}

@; ------------------------------------------------------------
@section[#:tag "servlet-structs.ss"]{Contracts}

@file{servlet/servlet-structs.ss} provides a number of contracts
for use in servlets.

@defthing[servlet-response? contract?]{Equivalent to @scheme[any/c].}

@; XXX Remove callbacks
@defproc[(xexpr/callback? [v any/c])
         boolean?]{
 Checks if @scheme[v] matches @scheme[xexpr?], except that embedded
 procedures are allowed.
}
                  
@defthing[response-generator? contract?]{Equivalent to @scheme[(k-url? . -> . servlet-response?)].}

@defthing[k-url? (any/c . -> . boolean?)]{Equivalent to @scheme[string?].}

@defthing[url-transform? contract?]{Equivalent to @scheme[(k-url? . -> . k-url?)].}

@defthing[expiration-handler? contract?]{Equivalent to @scheme[(or/c false/c (request? . -> . response?))].}

@defthing[embed/url? contract?]{Equivalent to @scheme[(((request? . -> . any/c)) (expiration-handler?) . opt-> . string?)].}

@; ------------------------------------------------------------
@section[#:tag "request-structs.ss"]{HTTP Requests}

@; XXX Create http sub-directory
@file{private/request-structs.ss} provides a number of structures and functions
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

@defstruct[(binding:form binding) ([id bytes?]
                                   [value bytes?])]{
 Represents a form binding of @scheme[id] to @scheme[value].
}
                                                   
@defstruct[(binding:file binding) ([id bytes?]
                                   [filename bytes?]
                                   [content bytes?])]{
 Represents the uploading of the file @scheme[filename] with the id @scheme[id]
 and the content @scheme[content].
} 

@defproc[(bindings-assq [id bytes?] [binds (listof binding?)])
         (or/c false/c binding?)]{
 Returns the binding with an id equal to @scheme[id] from @scheme[binds] or @scheme[#f].
}
                                 
@; XXX Subtypes of request                                 
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

@file{servlet/bindings.ss} provides a number of helper functions
for accessing request bindings.

@; XXX Move in request-bindings
@; XXX Rename extract-binding
@defproc[(extract-binding/single [id symbol?]
                                 [binds (listof (cons/c symbol? string?))])
         string?]{
 Returns the single binding associated with @scheme[id] in the a-list @scheme[binds]
 if there is exactly one binding. Otherwise errors.
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

@; ------------------------------------------------------------
@section[#:tag "response-structs.ss"]{HTTP Responses}

@file{private/response-structs.ss} provides structures and functions related to
HTTP responses.

@; XXX Rename extras to headers
@; XXX Make extras a listof header?
@defstruct[response/basic
           ([code number?]
            [message string?]
            [seconds number?]
            [mime bytes?]
            [extras (listof (cons/c symbol? string?))])]{
 A basic HTTP response containing no body. @scheme[code] is the response code,
 @scheme[message] the message, @scheme[seconds] the generation time, @scheme[mime]
 the MIME type of the file, and @scheme[extras] are the extra headers, in addition
 to those produced by the server.
} 

@; XXX Rename string? option
@defstruct[(response/full response/basic)
           ([code number?]
            [message string?]
            [seconds number?]
            [mime bytes?]
            [extras (listof (cons/c symbol? string?))]
            [body (listof (or/c string? bytes?))])]{
 As with @scheme[response/basic], except with @scheme[body] as the response
 body.
}
                                                   
@defstruct[(response/incremental response/basic)
           ([code number?]
            [message string?]
            [seconds number?]
            [mime bytes?]
            [extras (listof (cons/c symbol? string?))]
            [generator ((() (listof (or/c bytes? string?)) . ->* . any) . -> . any)])]{
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
                                      
@; ------------------------------------------------------------
@section[#:tag "web.ss"]{Web}

XXX

@; ------------------------------------------------------------
@section[#:tag "helpers.ss"]{Helpers}

XXX

@; XXX Depreciate
@; ------------------------------------------------------------
@section[#:tag "servlet-url.ss"]{Servlet URLs}

XXX

@; ------------------------------------------------------------
@section[#:tag "basic-auth.ss"]{Basic Authentication}

XXX

@; ------------------------------------------------------------
@section[#:tag "web-cells.ss"]{Web Cells}

XXX

