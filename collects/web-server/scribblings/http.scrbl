#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "http"
       #:style 'toc]{HTTP}

@defmodule[web-server/http]

The @web-server implements many HTTP RFCs that are provided by this module.

@local-table-of-contents[]

@; ------------------------------------------------------------
@section[#:tag "request-structs.ss"]{Requests}
@(require (for-label web-server/http/request-structs))

@defmodule[web-server/http/request-structs]{

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
         
 You almost @bold{always} want to use this, rather than @scheme[headers-assq] because Web browsers may send headers with arbitrary casing.
}

@defstruct[binding ([id bytes?])]{Represents a binding of @scheme[id].}

@defstruct[(binding:form binding) ([value bytes?])]{
 Represents a form binding of @scheme[id] to @scheme[value].
}

@defstruct[(binding:file binding) ([filename bytes?]
                                   [headers (listof header?)]
                                   [content bytes?])]{
 Represents the uploading of the file @scheme[filename] with the id @scheme[id]
 and the content @scheme[content], where @scheme[headers] are the additional headers from
 the MIME envelope the file was in. (For example, the @scheme[#"Content-Type"] header may
 be included by some browsers.)
}

@defproc[(bindings-assq [id bytes?]
                        [binds (listof binding?)])
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
 
 You are @bold{unlikely to need to construct} a request struct.
}
                                         
Here is an example typical of what you will find in many applications:
@schemeblock[
(define (get-number req)
  (match
    (bindings-assq 
      #"number"
      (request-bindings/raw req))
    [(? binding:form? b)
     (string->number
      (bytes->string/utf-8
       (binding:form-value b)))]
    [_
     (get-number (request-number))]))
]

}

@; ------------------------------------------------------------
@section[#:tag "bindings.ss"]{Bindings}
@(require (for-label web-server/http/bindings))

@defmodule[web-server/http/bindings]{

These functions, while convenient, could introduce subtle bugs into your
application. Examples: that they are case-insensitive could introduce
a bug; if the data submitted is not in UTF-8 format, then the conversion
to a string will fail; if an attacker submits a form field as if it were
a file, when it is not, then the @scheme[request-bindings] will hold a
@scheme[bytes?] object and your program will error; and, for file uploads
you lose the filename. @bold{Therefore, we recommend against their use, but
they are provided for compatibility with old code.}

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
                  
Here is an example typical of what you will find in many applications:
@schemeblock[
(define (get-number req)
  (string->number
   (extract-binding/single
    'number
    (request-bindings req))))
]

}

@; ------------------------------------------------------------
@section[#:tag "response-structs.ss"]{Responses}
@(require (for-label web-server/http/response-structs))

@defmodule[web-server/http/response-structs]{

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
 
 Example:
 @schemeblock[
  (make-response/basic
   301 "Moved Permanently"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list (make-header #"Location"
                      #"http://www.plt-scheme.org/downloads")))
 ]
}

@defstruct[(response/full response/basic)
           ([body (listof (or/c string? bytes?))])]{
 As with @scheme[response/basic], except with @scheme[body] as the response
 body.

 Example:
 @schemeblock[
  (make-response/full
   301 "Moved Permanently"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list (make-header #"Location"
                      #"http://www.plt-scheme.org/downloads"))
   (list #"<html><body><p>"
         #"Please go to <a href=\""
         #"http://www.plt-scheme.org/downloads"
         #"\">here</a> instead."
         #"</p></body></html>"))
 ]
}

@defstruct[(response/incremental response/basic)
           ([generator ((() (listof (or/c bytes? string?)) . ->* . any) . -> . any)])]{
 As with @scheme[response/basic], except with @scheme[generator] as a function that is
 called to generate the response body, by being given an @scheme[output-response] function
 that outputs the content it is called with.
 
 Here is a short example:
 @schemeblock[
  (make-response/incremental
    200 "OK" (current-seconds)
    #"application/octet-stream"
    (list (make-header #"Content-Disposition"
                       #"attachement; filename=\"file\""))
    (lambda (send/bytes)
      (send/bytes #"Some content")
      (send/bytes)
      (send/bytes #"Even" #"more" #"content!")
      (send/bytes "Now we're done")))
 ]
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

@warning{If you include a Content-Length header in a response that is inaccurate, there @bold{will be an error} in
transmission that the server @bold{will not catch}.}

}

@; ------------------------------------------------------------
@section[#:tag "redirect.ss"]{Redirect}
@(require (for-label web-server/http/redirect))

@defmodule[web-server/http/redirect]{

@defproc[(redirect-to [uri string?]
                      [perm/temp redirection-status? temporarily]
                      [#:headers headers (listof header?) (list)])
         response?]{
 Generates an HTTP response that redirects the browser to @scheme[uri],
 while including the @scheme[headers] in the response.
 
 Example:
 @scheme[(redirect-to "http://www.add-three-numbers.com" permanently)]
}

@defproc[(redirection-status? [v any/c])
         boolean?]{
 Determines if @scheme[v] is one of the following values.
}

@defthing[permanently redirection-status?]{A @scheme[redirection-status?] for permanent redirections.}

@defthing[temporarily redirection-status?]{A @scheme[redirection-status?] for temporary redirections.}

@defthing[see-other redirection-status?]{A @scheme[redirection-status?] for "see-other" redirections.}

}

@; ------------------------------------------------------------
@section[#:tag "basic-auth.ss"]{Basic Authentication}
@(require (for-label web-server/http/basic-auth))

@defmodule[web-server/http/basic-auth]{

An implementation of HTTP Basic Authentication.

@defproc[(extract-user-pass [heads (listof header?)])
         (or/c false/c (cons/c bytes? bytes?))]{
 Returns a pair of the username and password from the authentication
 header in @scheme[heads] if they are present, or @scheme[#f].
 
 Example:
 @scheme[(extract-user-pass (request-headers/raw req))] might return @scheme[(cons #"aladin" #"open sesame")].
}

}