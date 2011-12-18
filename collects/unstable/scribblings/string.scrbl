#lang scribble/doc
@(require scribble/base
          scribble/manual
          "utils.rkt"
          (for-label unstable/string
                     racket/contract
                     racket/base))

@title[#:tag "string"]{Strings}
@unstable-header[]

@defmodule[unstable/string]

@addition{Vincent St-Amour}

@defproc[(regexp-filter [pattern (or/c string? bytes? regexp? byte-regexp?)]
                        [lst (listof (or/c string? bytes? path? input-port?))])
         (listof (or/c string? bytes? path? input-port?))]{
Keeps only the elements of @racket[lst] that match @racket[pattern].
}
