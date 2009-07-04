#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "url-param.ss"]{URL Param}
@(require (for-label web-server/private/url-param
                     net/url))

@defmodule[web-server/private/url-param]{

The @web-server needs to encode information in URLs. If this data
is stored in the query string, than it will be overridden by browsers that
make GET requests to those URLs with more query data. So, it must be encoded
in URL params. This module provides functions for helping
with this process.

@defproc[(insert-param [u url?]
                       [k string?]
                       [v string?])
         url?]{
 Associates @scheme[k] with @scheme[v] in the final URL param of @scheme[u],
 overwritting any current binding for @scheme[k].
}

@defproc[(extract-param [u url?]
                        [k string?])
         (or/c string? false/c)]{
 Extracts the string associated with @scheme[k] in the final URL param of
 @scheme[u], if there is one, returning @scheme[#f] otherwise.
}

}
