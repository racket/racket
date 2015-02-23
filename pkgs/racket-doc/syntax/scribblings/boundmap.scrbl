#lang scribble/doc
@(require "common.rkt" (for-label syntax/boundmap syntax/id-table))

@title[#:tag "boundmap"]{Hashing on @racket[bound-identifier=?] and @racket[free-identifier=?]}

This library is for backwards-compatibility. Do not use it for new
libraries; use @racketmodname[syntax/id-table] instead.

@defmodule[syntax/boundmap]

@deftogether[[
@defproc[(make-bound-identifier-mapping) bound-identifier-mapping?]
@defproc[(bound-identifier-mapping? [v any/c]) boolean?]
@defproc[(bound-identifier-mapping-get [bound-map bound-identifier-mapping?]
				       [id identifier?]
				       [failure-thunk (-> any)
                                        (lambda () (raise (make-exn:fail ....)))])
         any]
@defproc[(bound-identifier-mapping-put! [bound-map bound-identifier-mapping?]
                                        [id identifier?]
                                        [v any/c])
         void?]
@defproc[(bound-identifier-mapping-for-each [bound-map boud-identifier-mapping?]
                                            [proc (identifier? any/c . -> . any)])
         void?]
@defproc[(bound-identifier-mapping-map [bound-map bound-identifier-mapping?] 
                                       [proc (identifier? any/c . -> . any)])
         (listof any?)]
]]{

Similar to @racket[make-bound-id-table], @racket[bound-id-table?],
@racket[bound-id-table-ref], @racket[bound-id-table-set!],
@racket[bound-id-table-for-each], and @racket[bound-id-table-map],
respectively.
}

@deftogether[[
@defproc[(make-free-identifier-mapping) free-identifier-mapping?]
@defproc[(free-identifier-mapping? [v any/c]) boolean?]
@defproc[(free-identifier-mapping-get [free-map free-identifier-mapping?]
                                      [id identifier?]
                                      [failure-thunk (-> any)
                                       (lambda () (raise (make-exn:fail ....)))])
         any]
@defproc[(free-identifier-mapping-put! [free-map free-identifier-mapping?]
                                       [id identifier?]
                                       [v any/c])
         void?]
@defproc[(free-identifier-mapping-for-each [free-map free-identifier-mapping?]
                                           [proc (identifier? any/c . -> . any)])
         void?]
@defproc[(free-identifier-mapping-map [free-map free-identifier-mapping?] 
                                      [proc (identifier? any/c . -> . any)])
         (listof any?)]
]]{

Similar to @racket[make-free-id-table], @racket[free-id-table?],
@racket[free-id-table-ref], @racket[free-id-table-set!],
@racket[free-id-table-for-each], and @racket[free-id-table-map],
respectively.
}

@deftogether[(
@defproc[(make-module-identifier-mapping) module-identifier-mapping?]
@defproc[(module-identifier-mapping? [v any/c]) boolean?]
@defproc[(module-identifier-mapping-get [module-map module-identifier-mapping?]
                                        [id identifier?]
                                        [failure-thunk (-> any)
                                         (lambda () (raise (make-exn:fail ....)))])
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

The same as @racket[make-free-identifier-mapping], etc.
}
