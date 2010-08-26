#lang scribble/manual
@(require scribble/eval
          (for-label data/interval-map
                     racket/contract
                     racket/dict
                     racket/base))

@title[#:tag "interval-map"]{Interval Maps}

@(define the-eval (make-base-eval))
@(the-eval '(require data/interval-map))
@(the-eval '(require racket/dict))

@defmodule[data/interval-map]

@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

An interval-map is a mutable dictionary-like data structure where
mappings are added by @emph{half-open} intervals and queried by
discrete points. Interval-maps can be used with any total
order. Internally, an interval-map uses a skip-list
(@racketmodname[data/skip-list]) of intervals for efficient query
and update.

Interval-maps implement the dictionary (@racketmodname[racket/dict])
interface to a limited extent. Only @racket[dict-ref] and the
iteraction-based methods (@racket[dict-iterate-first],
@racket[dict-map], etc) are supported. For the iteration-based
methods, the mapping's keys are considered the pairs of the start and
end positions of the mapping's intervals.

@examples[#:eval the-eval
(define r (make-numeric-interval-map))
(interval-map-set! r 1 5 'apple)
(interval-map-set! r 6 10 'pear)
(interval-map-set! r 3 6 'banana)
(dict-map r list)
]

@defproc[(make-interval-map [=? (any/c any/c . -> . any/c)]
                            [<? (any/c any/c . -> . any/c)]
                            [translate (or/c (any/c any/c . -> . (any/c . -> . any/c)) #f) #f])
         interval-map?]{

Makes a new empty interval-map. The interval-map uses @racket[=?] and
@racket[<?] to order the endpoints of intervals.

If @racket[translate] is a procedure, the interval-map supports
contraction and expansion of regions of its domain via
@racket[interval-map-contract!] and @racket[interval-map-expand!]. See
also @racket[make-numeric-interval-map].
}

@defproc[(make-numeric-interval-map)
         interval-map-with-translate?]{

Makes a new empty interval-map suitable for representing numeric
ranges.

Equivalent to
@racketblock[
(make-interval-map = < (lambda (x y) (lambda (z) (+ z (- y x)))))
]
}

@defproc[(interval-map? [v any/c])
         boolean?]{

Returns @racket[#t] if @racket[v] is an interval-map, @racket[#f]
otherwise.
}

@defproc[(interval-map-with-translate? [v any/c])
         boolean?]{

Returns @racket[#t] if @racket[v] is an interval-map constructed with
support for translation of keys, @racket[#f] otherwise.
}

@defproc[(interval-map-ref [interval-map interval-map?]
                           [position any/c]
                           [default any/c (lambda () (error ....))])
         any/c]{

Return the value associated with @racket[position] in
@racket[interval-map]. If no mapping is found, @racket[default] is
applied if it is a procedure, or returned otherwise.
}

@defproc[(interval-map-set! [interval-map interval-map?]
                            [start any/c]
                            [end any/c]
                            [value any/c])
         void?]{

Updates @racket[interval-map], associating every position in
[@racket[start], @racket[end]) with @racket[value].

Existing interval mappings contained in [@racket[start], @racket[end])
are destroyed, and partly overlapping intervals are truncated. See
@racket[interval-map-update*!] for an updating procedure that
preserves distinctions within [@racket[start], @racket[end]).
}

@defproc[(interval-map-update*! [interval-map interval-map?]
                                [start any/c]
                                [end any/c]
                                [updater (any/c . -> . any/c)]
                                [default any/c (lambda () (error ....))])
         void?]{

Updates @racket[interval-map], associating every position in
[@racket[start], @racket[end]) with the result of applying
@racket[updater] to the position's previously associated value, or to
the default value produced by @racket[default] if no mapping exists.

Unlike @racket[interval-map-set!], @racket[interval-map-update*!]
preserves existing distinctions within [@racket[start], @racket[end]).
}

@defproc[(interval-map-remove! [interval-map interval-map?]
                               [start any/c]
                               [end any/c])
         void?]{

Removes the value associated with every position in [@racket[start],
@racket[end]).
}

@defproc[(interval-map-expand! [interval-map interval-map-with-translate?]
                               [start any/c]
                               [end any/c])
         void?]{

Expands @racket[interval-map]'s domain by introducing a gap
[@racket[start], @racket[end]) and adjusting intervals after
@racket[start] using @racket[(_translate start end)].

If @racket[interval-map] was not constructed with a
@racket[_translate] argument, an exception is raised. If
@racket[start] is not less than @racket[end], an exception is raised.
}

@defproc[(interval-map-contract! [interval-map interval-map-with-translate?]
                                 [start any/c]
                                 [end any/c])
         void?]{

Contracts @racket[interval-map]'s domain by removing all mappings on
the interval [@racket[start], @racket[end]) and adjusting intervals
after @racket[end] using @racket[(_translate end start)].

If @racket[interval-map] was not constructed with a
@racket[_translate] argument, an exception is raised. If
@racket[start] is not less than @racket[end], an exception is raised.
}

@defproc[(interval-map-cons*! [interval-map interval-map?]
                              [start any/c]
                              [end any/c]
                              [v any/c]
                              [default any/c null])
         void?]{

Same as the following:
@racketblock[
(interval-map-update*! interval-map start end
                       (lambda (old) (cons v old))
                       default)
]
}

@defproc[(interval-map-iter? [v any/c])
         boolean?]{

Returns @racket[#t] if @racket[v] represents a position in an
interval-map, @racket[#f] otherwise.
}
