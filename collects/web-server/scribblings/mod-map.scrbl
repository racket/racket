#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "mod-map.ss"]{Serialization Utilities}
@(require (for-label web-server/private/mod-map))

@defmodule[web-server/private/mod-map]{

The @schememodname[scheme/serialize] library provides the
functionality of serializing values. This module
compresses the serialized representation.

@defproc[(compress-serial [sv list?])
         list?]{
 Collapses multiple occurrences of the same module in the module
 map of the serialized representation, @scheme[sv].
}

@defproc[(decompress-serial [csv list?])
         list?]{
 Expands multiple occurrences of the same module in the module
 map of the compressed serialized representation, @scheme[csv].
}

}
