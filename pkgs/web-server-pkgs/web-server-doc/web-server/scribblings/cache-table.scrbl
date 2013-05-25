#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "cache-table"]{Cache Table}
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
 Looks up @racket[id] in @racket[ct]. If it is not present, then @racket[mk] is
 called to construct the value and add it to @racket[ct].
}

@defproc[(cache-table-clear! [ct cache-table?])
         void?]{
 Clears all entries in @racket[ct].
}

@defproc[(cache-table? [v any/c])
         boolean?]{
 Determines if @racket[v] is a cache table.
}

}
