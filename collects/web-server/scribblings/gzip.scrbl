#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "gzip"]{GZip}
@(require (for-label web-server/private/gzip
                     file/gzip
                     file/gunzip))

@defmodule[web-server/private/gzip]{

The @web-server provides a thin wrapper around @racketmodname[file/gzip] and @racketmodname[file/gunzip].

@defproc[(gzip/bytes [ib bytes?])
         bytes?]{
 GZips @racket[ib] and returns the result.
}
                
@defproc[(gunzip/bytes [ib bytes?])
         bytes?]{
 GUnzips @racket[ib] and returns the result.
}

}
