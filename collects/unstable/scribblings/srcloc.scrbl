#lang scribble/manual
@(require scribble/eval "utils.ss" (for-label scheme/base unstable/srcloc))

@(define evaluator (make-base-eval))
@(evaluator '(require unstable/srcloc))

@title[#:tag "srcloc"]{Source Locations}

@defmodule[unstable/srcloc]

@unstable[@author+email["Carl Eastlund" "cce@ccs.neu.edu"]]

This module defines utilities for manipulating representations of source
locations, including both @scheme[srcloc] structures and all the values accepted
by @scheme[datum->syntax]'s third argument: syntax objects, lists, vectors, and
@scheme[#f].

@deftogether[(
@defproc[(source-location? [x any/c]) boolean?]{}
@defproc[(source-location-list? [x any/c]) boolean?]{}
@defproc[(source-location-vector? [x any/c]) boolean?]{}
)]{

These functions recognize valid source location representations.  The first,
@scheme[source-location?], recognizes @scheme[srcloc] structures, syntax
objects, lists, and vectors with appropriate structure, as well as @scheme[#f].
The latter predicates recognize only valid lists and vectors, respectively.

@examples[#:eval evaluator
(source-location? #f)
(source-location? #'here)
(source-location? (make-srcloc 'here 1 0 1 0))
(source-location? (make-srcloc 'bad 1 #f 1 0))
(source-location? (list 'here 1 0 1 0))
(source-location? (list* 'bad 1 0 1 0 'tail))
(source-location? (vector 'here 1 0 1 0))
(source-location? (vector 'bad 0 0 0 0))
]

}

@defproc[(check-source-location! [name symbol?] [x any/c]) void?]{

This procedure checks that its input is a valid source location.  If it is, the
procedure returns @scheme[(void)].  If it is not,
@scheme[check-source-location!] raises a detailed error message in terms of
@scheme[name] and the problem with @scheme[x].

@examples[#:eval evaluator
(check-source-location! 'this-example #f)
(check-source-location! 'this-example #'here)
(check-source-location! 'this-example (make-srcloc 'here 1 0 1 0))
(check-source-location! 'this-example (make-srcloc 'bad 1 #f 1 0))
(check-source-location! 'this-example (list 'here 1 0 1 0))
(check-source-location! 'this-example (list* 'bad 1 0 1 0 'tail))
(check-source-location! 'this-example (vector 'here 1 0 1 0))
(check-source-location! 'this-example (vector 'bad 0 0 0 0))
]

}

@deftogether[(
@defproc[(build-source-location [loc source-location?] ...) srcloc?]{}
@defproc[(build-source-location-list [loc source-location?] ...) source-location-list?]{}
@defproc[(build-source-location-vector [loc source-location?] ...) source-location-vector?]{}
@defproc[(build-source-location-syntax [loc source-location?] ...) syntax?]{}
)]{

These procedures combine multiple (zero or more) source locations, merging
locations within the same source and reporting @scheme[#f] for locations that
span sources.  They also convert the result to the desired representation:
@scheme[srcloc], list, vector, or syntax object, respectively.

@examples[#:eval evaluator
(build-source-location)
(build-source-location-list)
(build-source-location-vector)
(build-source-location-syntax)
(build-source-location #f)
(build-source-location-list #f)
(build-source-location-vector #f)
(build-source-location-syntax #f)
(build-source-location (list 'here 1 2 3 4))
(build-source-location-list (make-srcloc 'here 1 2 3 4))
(build-source-location-vector (make-srcloc 'here 1 2 3 4))
(build-source-location-syntax (make-srcloc 'here 1 2 3 4))
(build-source-location (list 'here 1 2 3 4) (vector 'here 5 6 7 8))
(build-source-location-list (make-srcloc 'here 1 2 3 4) (vector 'here 5 6 7 8))
(build-source-location-vector (make-srcloc 'here 1 2 3 4) (vector 'here 5 6 7 8))
(build-source-location-syntax (make-srcloc 'here 1 2 3 4) (vector 'here 5 6 7 8))
(build-source-location (list 'here 1 2 3 4) (vector 'there 5 6 7 8))
(build-source-location-list (make-srcloc 'here 1 2 3 4) (vector 'there 5 6 7 8))
(build-source-location-vector (make-srcloc 'here 1 2 3 4) (vector 'there 5 6 7 8))
(build-source-location-syntax (make-srcloc 'here 1 2 3 4) (vector 'there 5 6 7 8))
]

}

@deftogether[(
@defproc[(source-location->string [loc source-location?]) string?]{}
@defproc[(source-location->prefix [loc source-location?]) string?]{}
)]{

These procedures convert source locations to strings for use in error messages.
The first produces a string describing the source location; the second appends
@scheme[": "] to the string if it is non-empty.

@examples[#:eval evaluator
(source-location->string (make-srcloc 'here 1 2 3 4))
(source-location->string (make-srcloc 'here #f #f 3 4))
(source-location->string (make-srcloc 'here #f #f #f #f))
(source-location->string (make-srcloc #f 1 2 3 4))
(source-location->string (make-srcloc #f #f #f 3 4))
(source-location->string (make-srcloc #f #f #f #f #f))
(source-location->prefix (make-srcloc 'here 1 2 3 4))
(source-location->prefix (make-srcloc 'here #f #f 3 4))
(source-location->prefix (make-srcloc 'here #f #f #f #f))
(source-location->prefix (make-srcloc #f 1 2 3 4))
(source-location->prefix (make-srcloc #f #f #f 3 4))
(source-location->prefix (make-srcloc #f #f #f #f #f))
]

}
