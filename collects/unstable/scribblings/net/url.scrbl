#lang scribble/doc
@(require scribble/base
          scribble/manual
          "../utils.rkt"
          (for-label unstable/net/url
                     net/url
                     racket/contract
                     racket/base))

@title[#:tag "url"]{URLs}

@defmodule[unstable/net/url]

@unstable-header[]

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
