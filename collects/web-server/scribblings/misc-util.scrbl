#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "misc-util" #:style 'toc]{Miscellaneous Utilities}

@(require (for-label web-server/private/util
                     racket/path))

@defmodule[web-server/private/util]

@defproc[(bytes-ci=? [b1 bytes?] [b2 bytes?]) boolean?]{
 Compares two bytes case insensitively.
}

@defproc[(url-replace-path [proc ((listof path/param?) . -> . (listof path/param?))]
                           [u url?])
         url?]{
 Replaces the URL path of @racket[u] with @racket[proc] of the former path.
}

@defproc[(url-path->string [url-path (listof path/param?)])
         string?]{
 Formats @racket[url-path] as a string with @racket["/"] as a delimiter
 and no params.
}

@defproc[(explode-path* [p path-string?])
         (listof path-piece?)]{
 Like @racket[normalize-path], but does not resolve symlinks.
}

@defproc[(path-without-base [base path-string?]
                            [p path-string?])
         (listof path-piece?)]{
 Returns, as a list, the portion of @racket[p] after @racket[base],
 assuming @racket[base] is a prefix of @racket[p].
}

@defproc[(directory-part [p path-string?])
         path?]{
 Returns the directory part of @racket[p], returning @racket[(current-directory)]
 if it is relative.
}

@defproc[(build-path-unless-absolute [base path-string?]
                                     [p path-string?])
         path?]{
 Prepends @racket[base] to @racket[p], unless @racket[p] is absolute.
}

@defproc[(network-error [s symbol?]
                        [fmt string?]
                        [v any/c] ...)
         void]{
 Like @racket[error], but throws a @racket[exn:fail:network].
}

@defproc[(exn->string [exn (or/c exn? any/c)])
         string?]{
 Formats @racket[exn] with @racket[(error-display-handler)] as a string.
}
