#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "cache-table.ss"]{Cache Table}
@(require (for-label web-server/private/cache-table))

@defmodule[web-server/private/cache-table]{

This module provides a set of caching hash table
functions.

@defproc[(make-cache-table)
         cache-table?]{
 Constructs a cache-table.
}

@defproc[(cache-table-lookup! [ct cache-table?]
                              [id symbol?]
                              [mk (-> any/c)])
         any/c]{
 Looks up @scheme[id] in @scheme[ct]. If it is not present, then @scheme[mk] is
 called to construct the value and add it to @scheme[ct].
}

@defproc[(cache-table-clear! [ct cache-table?])
         void?]{
 Clears all entries in @scheme[ct].
}

@defproc[(cache-table? [v any/c])
         boolean?]{
 Determines if @scheme[v] is a cache table.
}

}
