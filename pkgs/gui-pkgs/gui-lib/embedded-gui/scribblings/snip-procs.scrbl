#lang scribble/doc
@(require "common.rkt")

@title[#:tag "snip-related-functions"]{Snip Functions}

@defproc[(snip-width [snip (is-a?/c snip%)]) real?]{

The width of a snip in the parent pasteboard.}

@defproc[(snip-height [snip (is-a?/c snip%)]) real?]{

The height of a snip in the parent pasteboard.}

@defproc[(snip-min-width [snip (is-a?/c snip%)]) real?]{

The minimum width of the snip}

@defproc[(snip-min-height [snip (is-a?/c snip%)]) real?]{

The minimum height of the snip.}

@defproc[(snip-parent [snip (is-a?/c snip%)]) (is-a?/c pasteboard%)]{

The pasteboard that contains the snip.}

@defproc[(fold-snip [f ((is-a?/c snip%) any/c . -> . any/c)]
                    [init-acc any/c]
                    [snip (is-a?/c snip%)])
          any/c]{

Applies @racket[f] to all snips in the parent of @racket[snip], 
starting with @racket[snip].}

@defproc[(for-each-snip [f ((is-a?/c snip%) . -> . any/c)]
                        [first-snip (is-a?/c snip%)] 
                        [more list?] ...)
         void?]{

Applies the function to each snip in the parent of
@racket[first-snip], starting with @racket[first-snip]. If
@racket[more] lists are supplied, they are used for extra arguments to
@racket[f], just like extra lists provided to @racket[for-each].}

@defproc[(map-snip [f ((is-a?/c snip%) . -> . any/c)]
                   [first-snip (is-a?/c snip%)] 
                   [more list?] ...)
         void?]{

Applies the function to each snip in the parent of
@racket[first-snip], starting with @racket[first-snip], and
accumulates the results into a list. If @racket[more] lists are
supplied, they are used for extra arguments to @racket[f], just like
extra lists provided to @racket[map].}


@defproc[(stretchable-width? [snip (is-a?/c snip%)]) boolean?]{

True if the snip can be resized in the X dimension.}

@defproc[(stretchable-height? [snip (is-a?/c snip%)]) boolean?]{

True if the snip can be resized in the Y dimension.}
