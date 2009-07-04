#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "gzip.ss"]{GZip}
@(require (for-label web-server/private/gzip
                     file/gzip
                     file/gunzip))

@defmodule[web-server/private/gzip]{

The @web-server provides a thin wrapper around @schememodname[file/gzip] and @schememodname[file/gunzip].

@defproc[(gzip/bytes [ib bytes?])
         bytes?]{
 GZips @scheme[ib] and returns the result.
}
                
@defproc[(gunzip/bytes [ib bytes?])
         bytes?]{
 GUnzips @scheme[ib] and returns the result.
}

}
