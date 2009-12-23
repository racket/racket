#lang scribble/doc
@(require scribble/base
          scribble/manual
          "../utils.ss"
          (for-label unstable/net/url
                     net/url
                     scheme/contract
                     scheme/base))

@title[#:tag "url"]{URLs}

@defmodule[unstable/net/url]

@unstable-header[]

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
