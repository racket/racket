#lang scribble/manual

@begin[(require "../utils.rkt")
       (require (for-label (only-meta-in 0 [except-in typed/racket for])
                           (only-in racket/base for)
                           racket/list srfi/14 net/url
                           version/check))]


@title{Libraries Provided With Typed Racket}

The @racketmodname[typed/racket] language corresponds to the
@racketmodname[racket] language---that is, any identifier provided
by @racketmodname[racket], such as @racket[modulo] is available by default in
@racketmodname[typed/racket].

@racketmod[typed/racket
(modulo 12 2)
]

The @racketmodname[typed/racket/base] language corresponds to the
@racketmodname[racket/base] language.

Some libraries have counterparts in the @racketidfont{typed}
collection, which provide the same exports as the untyped versions.
Such libraries include @racketmodname[srfi/14],
@racketmodname[net/url], and many others.

@racketmod[typed/racket
(require typed/srfi/14)
(char-set= (string->char-set "hello")
           (string->char-set "olleh"))
]

To participate in making more libraries available, please visit
@link["http://www.ccs.neu.edu/home/samth/adapt/"]{here}.


Other libraries can be used with Typed Racket via
@racket[require/typed].

@racketmod[typed/racket
(require/typed version/check
               [check-version (-> (U Symbol (Listof Any)))])
(check-version)
]
