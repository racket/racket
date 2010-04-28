#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "mod-map"]{Serialization Utilities}
@(require (for-label web-server/private/mod-map))

@defmodule[web-server/private/mod-map]{

The @racketmodname[racket/serialize] library provides the
functionality of serializing values. This module
compresses the serialized representation.

@defproc[(compress-serial [sv list?])
         list?]{
 Collapses multiple occurrences of the same module in the module
 map of the serialized representation, @racket[sv].
}

@defproc[(decompress-serial [csv list?])
         list?]{
 Expands multiple occurrences of the same module in the module
 map of the compressed serialized representation, @racket[csv].
}

}
