#lang scribble/doc
@(require "common.rkt" (for-label syntax/boundmap))

@title[#:tag "boundmap"]{Hashing on @racket[bound-identifier=?] and @racket[free-identifier=?]}

See also @racketmodname[syntax/id-table] for an implementation of
identifier mappings using the @racketmodname[scheme/dict] dictionary
interface.

@defmodule[syntax/boundmap]

@defproc[(make-bound-identifier-mapping) bound-identifier-mapping?]{

Produces a hash-table-like value for storing a mapping from syntax
identifiers to arbitrary values.

The mapping uses @racket[bound-identifier=?] to compare mapping keys,
but also uses a hash table based on symbol equality to make the
mapping efficient in the common case (i.e., where non-equivalent
identifiers are derived from different symbolic names).}


@defproc[(bound-identifier-mapping? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] was produced by
@racket[make-bound-identifier-mapping], @racket[#f] otherwise.}


@defproc[(bound-identifier-mapping-get [bound-map bound-identifier-mapping?]
				       [id identifier?]
				       [failure-thunk any/c (lambda () (raise (make-exn:fail ....)))])
         any]{

Like @racket[hash-table-get] for bound-identifier mappings.}


@defproc[(bound-identifier-mapping-put! [bound-map bound-identifier-mapping?]
                                        [id identifier?]
                                        [v any/c])
         void?]{

Like @racket[hash-table-put!] for bound-identifier mappings.}

@defproc[(bound-identifier-mapping-for-each [bound-map boud-identifier-mapping?]
                                            [proc (identifier? any/c . -> . any)])
         void?]{

Like @racket[hash-table-for-each].}


@defproc[(bound-identifier-mapping-map [bound-map bound-identifier-mapping?] 
                                       [proc (identifier? any/c . -> . any)])
         (listof any?)]{

Like @racket[hash-table-map].}


@defproc[(make-free-identifier-mapping) free-identifier-mapping?]{

Produces a hash-table-like value for storing a mapping from syntax
identifiers to arbitrary values.

The mapping uses @racket[free-identifier=?] to compare mapping keys,
but also uses a hash table based on symbol equality to make the
mapping efficient in the common case (i.e., where non-equivalent
identifiers are derived from different symbolic names at their
definition sites).}


@defproc[(free-identifier-mapping? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] was produced by
@racket[make-free-identifier-mapping], @racket[#f] otherwise.
}


@defproc[(free-identifier-mapping-get [free-map free-identifier-mapping?]
                                      [id identifier?]
                                      [failure-thunk any/c (lambda () (raise (make-exn:fail ....)))])
         any]{

Like @racket[hash-table-get] for free-identifier mappings.}


@defproc[(free-identifier-mapping-put! [free-map free-identifier-mapping?]
                                       [id identifier?]
                                       [v any/c])
         void?]{

Like @racket[hash-table-put!] for free-identifier mappings.}


@defproc[(free-identifier-mapping-for-each [free-map free-identifier-mapping?]
                                           [proc (identifier? any/c . -> . any)])
         void?]{

Like @racket[hash-table-for-each].}


@defproc[(free-identifier-mapping-map [free-map free-identifier-mapping?] 
                                      [proc (identifier? any/c . -> . any)])
         (listof any?)]{

Like @racket[hash-table-map].}


@deftogether[(
@defproc[(make-module-identifier-mapping) module-identifier-mapping?]
@defproc[(module-identifier-mapping? [v any/c]) boolean?]
@defproc[(module-identifier-mapping-get [module-map module-identifier-mapping?]
                                        [id identifier?]
                                        [failure-thunk any/c (lambda () (raise (make-exn:fail ....)))])
         any]
@defproc[(module-identifier-mapping-put! [module-map module-identifier-mapping?]
                                         [id identifier?]
                                         [v any/c])
         void?]
@defproc[(module-identifier-mapping-for-each [module-map module-identifier-mapping?]
                                             [proc (identifier? any/c . -> . any)])
         void?]
@defproc[(module-identifier-mapping-map [module-map module-identifier-mapping?] 
                                        [proc (identifier? any/c . -> . any)])
         (listof any?)]
)]{

The same as @racket[make-free-identifier-mapping], etc.}
