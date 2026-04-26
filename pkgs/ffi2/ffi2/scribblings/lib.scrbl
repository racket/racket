#lang scribble/manual
@(require "common.rkt"
          (for-label
           (only-in ffi/unsafe ffi-lib? ffi-lib)))

@title[#:tag "ffi2-lib"]{Foreign Libraries}

@defproc[(ffi2-lib? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @deftech{foreign-library value},
@racket[#f] otherwise.

The @racket[ffi2-lib?] predicate is equivalent to @racket[ffi-lib?]
from @racketmodname[ffi/unsafe].}

@defproc[(ffi2-lib [path (or/c path-string? #f)]
                   [version (or/c string? (listof (or/c string? #f)) #f) #f]
                   [#:get-lib-dirs get-lib-dirs (->/c (listof path?)) get-lib-search-dirs]
                   [#:fail fail (or/c #f (->/c any)) #f]
                   [#:global? global? any/c (eq? 'global (system-type 'so-mode))]
                   [#:custodian custodian (or/c 'place custodian? #f) #f])
         any]{

Equivalent to @racket[ffi-lib] from @racketmodname[ffi/unsafe].

The result is normally a @tech{foreign-library value} value recognized
by @racket[ffi2-lib?], but the result can be anything if @racket[fail]
is called to produce a result.

}

@defproc[(ffi2-lib-ref [name (or/c string? bytes? symbol?)]
                       [lib (or/c ffi2-lib? #f)]
                       [#:fail fail (or/c (->/c any) #f) #f])
         any]{

Gets the address for @racket[name] as exported by the foreign library
@racket[lib]. If @racket[lib] is @racket[#f] or if no such export is
found, the @racket[fail] is called to produce the result, or an
exception is raised if @racket[fail] is @racket[#f].

The result is normally a @tech{pointer} recognized by @racket[ptr_t?],
but the result can be anything if @racket[fail] is called to produce a
result.

}
