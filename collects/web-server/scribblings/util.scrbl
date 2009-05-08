#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "util.ss"]{Miscellaneous Utilities}
@(require (for-label web-server/private/util
                     net/url
                     scheme/serialize
                     scheme/path))

@defmodule[web-server/private/util]

There are a number of other miscellaneous utilities the @web-server
needs. They are provided by this module.

@section{Contracts}
@defthing[non-empty-string/c contract?]{Contract for non-empty strings.}
@defthing[port-number? contract?]{Equivalent to @scheme[(between/c 1 65535)].}
@defthing[path-element? contract?]{Equivalent to @scheme[(or/c path-string? (symbols 'up 'same))].}

@section{Lists}
@defproc[(list-prefix? [l list?]
                       [r list?])
         boolean?]{
 True if @scheme[l] is a prefix of @scheme[r].
}

@section{URLs}

@defproc[(url-replace-path [proc ((listof path/param?) . -> . (listof path/param?))]
                           [u url?])
         url?]{
 Replaces the URL path of @scheme[u] with @scheme[proc] of the former path.
}

@defproc[(url-path->string [url-path (listof path/param?)])
         string?]{
 Formats @scheme[url-path] as a string with @scheme["/"] as a delimiter
 and no params.
}

@section{Paths}
@defproc[(explode-path* [p path-string?])
         (listof path-element?)]{
 Like @scheme[normalize-path], but does not resolve symlinks.
}

@defproc[(path-without-base [base path-string?]
                            [p path-string?])
         (listof path-element?)]{
 Returns, as a list, the portion of @scheme[p] after @scheme[base],
 assuming @scheme[base] is a prefix of @scheme[p].
}

@defproc[(directory-part [p path-string?])
         path?]{
 Returns the directory part of @scheme[p], returning @scheme[(current-directory)]
 if it is relative.
}

@defproc[(build-path-unless-absolute [base path-string?]
                                     [p path-string?])
         path?]{
 Prepends @scheme[base] to @scheme[p], unless @scheme[p] is absolute.
}

@defproc[(strip-prefix-ups [p (listof path-element?)])
         (listof path-element?)]{
 Removes all the prefix @scheme[".."]s from @scheme[p].
}

@section{Exceptions}

@defproc[(network-error [s symbol?]
                        [fmt string?]
                        [v any/c] ...)
         void]{
 Like @scheme[error], but throws a @scheme[exn:fail:network].
}

@defproc[(exn->string [exn (or/c exn? any/c)])
         string?]{
 Formats @scheme[exn] with @scheme[(error-display-handler)] as a string.
}

@section{Strings}

@defproc[(lowercase-symbol! [sb (or/c string? bytes?)])
         symbol?]{
 Returns @scheme[sb] as a lowercase symbol.
}

@defproc[(read/string [s string?])
         serializable?]{
 @scheme[read]s a value from @scheme[s] and returns it.
}

@defproc[(write/string [v serializable?])
         string?]{
 @scheme[write]s @scheme[v] to a string and returns it.
}

@section{Bytes}

@defproc[(bytes-ci=? [b1 bytes?] [b2 bytes?]) boolean?]{
 Compares two bytes case insensitively.
}
                                                        
@defproc[(read/bytes [b bytes?])
         serializable?]{
 @scheme[read]s a value from @scheme[b] and returns it.
}

@defproc[(write/bytes [v serializable?])
         bytes?]{
 @scheme[write]s @scheme[v] to a bytes and returns it.
}
