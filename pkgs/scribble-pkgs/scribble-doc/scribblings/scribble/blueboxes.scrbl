#lang scribble/doc
@(require scribble/manual "utils.rkt"
          (for-label scribble/core
                     scribble/blueboxes
                     racket/contract))

@title[#:tag "blueboxes"]{Blue Boxes Utilities}

@defmodule[scribble/blueboxes]{
  The @racketmodname[scribble/blueboxes] provides access
  to the content of the ``blue boxes'' that describe
  some module's export (but without any styling).}

@defproc[(fetch-blueboxes-strs [tag tag?]
                               [#:blueboxes-cache blueboxes-cache
                                                  blueboxes-cache?
                                                  (make-blueboxes-cache)])
         (or/c #f (non-empty-listof string?))]{
  Returns a list of strings that show the content of the blue box
  (without any styling information) for the documentation referenced
  by @racket[tag].
  
  The first string in the list describes the export (e.g. @racket["procedure"]
  when @racket[defproc] is used, or @racket["syntax"] when @racket[defform]
  was used to document the export).
}

@defproc[(make-blueboxes-cache [populate? boolean?]) blueboxes-cache?]{
  Constructs a new (mutable) blueboxes cache. 
  
  If @racket[populate?] is @racket[#f], the cache is initially
  unpopulated, in which case it is filled in the first time the cache
  is passed to @racket[fetch-bluebxoes-strs]. Otherwise, the cache is
  initially populated.
}

@defproc[(blueboxes-cache? [v any/c]) boolean?]{
  Determines if @racket[v] is a blueboxes cache.                                                
}
