#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "lang/web.ss"]{Stateless Web Interaction}

@(require (for-label net/url
                     xml
                     scheme/serialize
                     web-server/servlet/servlet-structs
                     web-server/http))

@section{Low Level}

@(require (for-label web-server/lang/abort-resume))
@defmodule[web-server/lang/abort-resume]{

@defproc[(send/suspend [response-generator (continuation? . -> . any)])
         any]{
 Captures the current continuation in a serializable way and calls @scheme[response-generator] with it, returning the result.
}
             
}

@section{High Level}

@(require (for-label web-server/lang/web))
@defmodule[web-server/lang/web]{

@defproc[(send/suspend/url [response-generator (url? . -> . response?)])
         request?]{
 Captures the current continuation. Serializes it and stuffs it into
 a URL. Calls @scheme[response-generator] with this URL and delivers
 the response to the client. If the URL is invoked
 the request is returned to this continuation.
}

@defproc[(send/suspend/hidden [response-generator (url? xexpr? . -> . response?)])
         request?]{
 Captures the current continuation. Serializes it and generates an INPUT
 form that includes the serialization as a hidden form.
 Calls @scheme[response-generator] with this URL and form field and delivers
 the response to the client. If the URL is invoked with form data containing
 the hidden form,
 the request is returned to this continuation.

 Note: The continuation is NOT stuffed.
}

@defproc[(send/suspend/dispatch [make-response (embed/url/c . -> . response?)])
         any/c]{
 Calls @scheme[make-response] with a function that, when called with a procedure from
 @scheme[request?] to @scheme[any/c] will generate a URL, that when invoked will call
 the function with the @scheme[request?] object and return the result to the caller of
 @scheme[send/suspend/dispatch].
}

@deftogether[(
@defproc[(redirect/get) request?]
)]{

See @schememodname[web-server/servlet/web].}
}

@; ------------------------------------------------------------
@section[#:tag "lang/stuff-url.ss"]{Stuff URL}
@(require (for-label web-server/lang/stuff-url))

@defmodule[web-server/lang/stuff-url]{

@filepath{lang/stuff-url.ss} provides an interface for "stuffing"
serializable values into URLs. Currently there is a particular
hard-coded behavior, but we hope to make it more flexible in
the future.

@defproc[(stuff-url [v serializable?]
                    [u url?])
         url?]{
 Returns a URL based on @scheme[u] with @scheme[v] serialized and "stuffed" into it.
 The following steps are applied until the URL is short enough to be accepted by IE.
 @itemize[
  @item{Put the plain-text serialization in the URL.}
  @item{Compress the serialization with @schememodname[file/gzip] into the URL.}
  @item{Compute the MD5 of the compressed seralization and write it to
   @filepath{$HOME/.urls/M} where `M' is the MD5. `M' is then
   placed in the URL}
 ]
}

@defproc[(stuffed-url? [u url?])
         boolean?]{
 Checks if @scheme[u] appears to be produced by @scheme[stuff-url].
}

@defproc[(unstuff-url [u url?])
         serializable?]{
 Extracts the value previously serialized into @scheme[u] by @scheme[stuff-url].
}

In the future, we will offer the facilities to:
@itemize[
 @item{Optionally use the content-addressed storage.}
 @item{Use different hashing algorithms for the CAS.}
 @item{Encrypt the serialized value.}
 @item{Only use the CAS if the URL would be too long. (URLs may only be 1024 characters.)}
]
}
