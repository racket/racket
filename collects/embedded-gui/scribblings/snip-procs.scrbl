#lang scribble/doc
@(require "common.ss")

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

Applies @scheme[f] to all snips in the parent of @scheme[snip], 
starting with @scheme[snip].}

@defproc[(for-each-snip [f ((is-a?/c snip%) . -> . any/c)]
                        [first-snip (is-a?/c snip%)] 
                        [more list?] ...)
         void?]{

Applies the function to each snip in the parent of
@scheme[first-snip], starting with @scheme[first-snip]. If
@scheme[more] lists are supplied, they are used for extra arguments to
@scheme[f], just like extra lists provided to @scheme[for-each].}

@defproc[(map-snip [f ((is-a?/c snip%) . -> . any/c)]
                   [first-snip (is-a?/c snip%)] 
                   [more list?] ...)
         void?]{

Applies the function to each snip in the parent of
@scheme[first-snip], starting with @scheme[first-snip], and
accumulates the results into a list. If @scheme[more] lists are
supplied, they are used for extra arguments to @scheme[f], just like
extra lists provided to @scheme[map].}


@defproc[(stretchable-width? [snip (is-a?/c snip%)]) boolean?]{

True if the snip can be resized in the X dimension.}

@defproc[(stretchable-height? [snip (is-a?/c snip%)]) boolean?]{

True if the snip can be resized in the Y dimension.}
