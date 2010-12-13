#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "lang/web"]{Stateless Web Interaction}

@(require (for-label net/url
                     xml
                     web-server/lang/web
                     racket
                     web-server/http))

@defmodule[web-server/lang/web]{

@defproc[(send/suspend/url [response-generator (url? . -> . response?)])
         request?]{
 Captures the current continuation. Serializes it and stuffs it into
 a URL. Calls @racket[response-generator] with this URL and delivers
 the response to the client. If the URL is invoked
 the request is returned to this continuation.
}
                  
@defproc[(send/suspend [response-generator (string? . -> . response?)])
         request?]{
 Like @racket[send/suspend/url] but with a string URL representation.
}

@defproc[(send/suspend/hidden [response-generator (url? xexpr/c . -> . response?)])
         request?]{
 Captures the current continuation. Serializes it and stuffs it into a hidden INPUT
 form element.
 Calls @racket[response-generator] with this URL and form field and delivers
 the response to the client. If the URL is invoked with form data containing
 the hidden form,
 the request is returned to this continuation.
}

@defproc[(send/suspend/url/dispatch [make-response (((request? . -> . any) . -> . url?) . -> . response?)])
         any]{
 Calls @racket[make-response] with a function that, when called with a procedure from
 @racket[request?] to @racket[any/c] will generate a URL, that when invoked will call
 the function with the @racket[request?] object and return the result to the caller of
 @racket[send/suspend/dispatch].
}
               
@defproc[(send/suspend/dispatch [make-response (((request? . -> . any) . -> . string?) . -> . response?)])
         request?]{
 Like @racket[send/suspend/url/dispatch] but with a string URL representation.
}

@deftogether[(
@defproc[(redirect/get) request?]
)]{

See @racketmodname[web-server/servlet/web].}
}
