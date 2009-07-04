#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "mime-types.ss"]{MIME Types}
@(require (for-label web-server/private/mime-types))

@defmodule[web-server/private/mime-types]{

This module provides function for dealing with @filepath{mime.types}
files.

@defproc[(read-mime-types [p path-string?])
         (hash-table/c symbol? bytes?)]{
 Reads the @filepath{mime.types} file from @scheme[p] and constructs a
 hash table mapping extensions to MIME types.
}

@defproc[(make-path->mime-type [p path-string?])
         (path? . -> . bytes?)]{
 Uses a @scheme[read-mime-types] with @scheme[p] and constructs a
 function from paths to their MIME type.
}

}
