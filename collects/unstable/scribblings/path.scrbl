#lang scribble/doc
@(require scribble/base
          scribble/manual
          "utils.rkt"
          (for-label unstable/path
                     unstable/contract
                     racket/contract
                     racket/base))

@title[#:tag "path"]{Path}

@defmodule[unstable/path]

@unstable-header[]

@defproc[(explode-path* [p path-string?])
         (listof path-piece?)]{
 Like @racket[normalize-path], but does not resolve symlinks.
}

@defproc[(path-without-base [base path-string?]
                            [p path-string?])
         (listof path-piece?)]{
 Returns, as a list, the portion of @racket[p] after @racket[base],
 assuming @racket[base] is a prefix of @racket[p].
}

@defproc[(directory-part [p path-string?])
         path?]{
 Returns the directory part of @racket[p], returning @racket[(current-directory)]
 if it is relative.
}

@defproc[(build-path-unless-absolute [base path-string?]
                                     [p path-string?])
         path?]{
 Prepends @racket[base] to @racket[p], unless @racket[p] is absolute.
}

@defproc[(strip-prefix-ups [p (listof path-piece?)])
         (listof path-piece?)]{
 Removes all the prefix @racket[".."]s from @racket[p].
}
