#lang scribble/doc
@(require "common.rkt"
          (for-label file/cache))

@title[#:tag "cache"]{Caching}

@defmodule[file/cache]{The @racketmodname[file/cache] library provides
utilities for managing a local cache of files, such as downloaded
files.  The cache is safe for concurrent use across processes, since
it uses filesystem locks, and it isolates clients from filesystem
failures.}

@defproc[(cache-file     [dest-file path-string?]
                         [#:exists-ok? exists-ok? any/c #f]
                         [key (not/c #f)]
                         [cache-dir path-string?]
                         [fetch (-> any)]
                         [#:notify-cache-use notify-cache-use (string? . -> . any)
                                             void]
                         [#:max-cache-files max-files real? 1024]
                         [#:max-cache-size max-size real? (* 64 1024 1024)]
                         [#:evict-before? evict-before? (hash? hash? . -> . boolean?)
                                          (lambda (a b)
                                            (< (hash-ref a 'modify-seconds)
                                               (hash-ref b 'modify-seconds)))]
                         [#:log-error-string log-error-string (string? . -> . any)
                                             (lambda (s) (log-error s))]
                         [#:log-debug-string log-debug-string (string? . -> . any)
                                             (lambda (s) (log-debug s))])
           void?]{

Looks for a file in @racket[cache-dir] previously cached with
@racket[key], and copies it to @racket[dest-file] (which must not
exist already, unless @racket[exists-ok?] is true) if a cached file
is found. Otherwise, @racket[fetch] is called; if @racket[dest-file]
exists after calling @racket[fetch], it is copied to @racket[cache-dir]
and recorded with @racket[key]. When a cache entry is used,
@racket[notify-cache-use] is called with the name of the cache file.

When a new file is cached, @racket[max-files] (as a file count) and
@racket[max-size] (in bytes) determine whether any previously cached
files should be evicted from the cache. If so, @racket[evict-before?]
determines an order on existing cache entries for eviction; each
argument to @racket[evict-before?] is a hash table with at least the
following keys:

@itemlist[

 @item{@racket['modify-seconds] --- the file's modification date}

 @item{@racket['size] --- the file's size in bytes}

 @item{@racket['key] --- the cache entry's key}

 @item{@racket['name] --- the cache file's name}

]

The @racket[log-error-string] and @racket[log-debug-string] functions
are used to record errors and debugging information.}


@defproc[(cache-remove [key any/c]
                       [cache-dir path-string?]
                       [#:log-error-string log-error-string (string? . -> . any)
                                           (lambda (s) (log-error s))]
                       [#:log-debug-string log-debug-string (string? . -> . any)
                                           (lambda (s) (log-debug s))])
          void?]{

Removes the cache entry matching @racket[key] (if any) from the cache
in @racket[cache-dir], or removes all cached files if @racket[key] is
@racket[#f].

The @racket[log-error-string] and @racket[log-debug-string] functions
are used to record errors and debugging information.}
