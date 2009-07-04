#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "lang/web.ss"]{Stateless Web Interaction}

@(require (for-label net/url
                     xml
                     web-server/lang/web
                     scheme
                     web-server/http))

@defmodule[web-server/lang/web]{

@defproc[(send/suspend/url [response-generator (url? . -> . response/c)])
         request?]{
 Captures the current continuation. Serializes it and stuffs it into
 a URL. Calls @scheme[response-generator] with this URL and delivers
 the response to the client. If the URL is invoked
 the request is returned to this continuation.
}
                  
@defproc[(send/suspend [response-generator (string? . -> . response/c)])
         request?]{
 Like @scheme[send/suspend/url] but with a string URL representation.
}

@defproc[(send/suspend/hidden [response-generator (url? xexpr/c . -> . response/c)])
         request?]{
 Captures the current continuation. Serializes it and stuffs it into a hidden INPUT
 form element.
 Calls @scheme[response-generator] with this URL and form field and delivers
 the response to the client. If the URL is invoked with form data containing
 the hidden form,
 the request is returned to this continuation.
}

@defproc[(send/suspend/url/dispatch [make-response (((request? . -> . any) . -> . url?) . -> . response/c)])
         any]{
 Calls @scheme[make-response] with a function that, when called with a procedure from
 @scheme[request?] to @scheme[any/c] will generate a URL, that when invoked will call
 the function with the @scheme[request?] object and return the result to the caller of
 @scheme[send/suspend/dispatch].
}
               
@defproc[(send/suspend/dispatch [make-response (((request? . -> . any) . -> . string?) . -> . response/c)])
         request?]{
 Like @scheme[send/suspend/url/dispatch] but with a string URL representation.
}

@deftogether[(
@defproc[(redirect/get) request?]
)]{

See @schememodname[web-server/servlet/web].}
}
