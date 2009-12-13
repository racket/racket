#lang scribble/manual
@(require scribble/eval
          "utils.ss"
          (for-label unstable/interval-map
                     scheme/contract
                     scheme/dict
                     scheme/base))

@title[#:tag "interval-map"]{Interval Maps}

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/interval-map))
@(the-eval '(require scheme/dict))

@defmodule[unstable/interval-map]

@unstable[@author+email["Ryan Culpepper" "ryanc@plt-scheme.org"]]

An interval-map is a mutable dictionary-like data structure where
mappings are added by @emph{half-open} intervals and queried by
discrete points. Interval-maps can be used with any total
order. Internally, an interval-map uses a skip-list
(@schememodname[unstable/skip-list]) of intervals for efficient query
and update.

Interval-maps implement the dictionary (@schememodname[scheme/dict])
interface to a limited extent. Only @scheme[dict-ref] and the
iteraction-based methods (@scheme[dict-iterate-first],
@scheme[dict-map], etc) are supported. For the iteration-based
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

Makes a new empty interval-map. The interval-map uses @scheme[=?] and
@scheme[<?] to order the endpoints of intervals.

If @scheme[translate] is a procedure, the interval-map supports
contraction and expansion of regions of its domain via
@scheme[interval-map-contract!] and @scheme[interval-map-expand!]. See
also @scheme[make-numeric-interval-map].
}

@defproc[(make-numeric-interval-map)
         interval-map-with-translate?]{

Makes a new empty interval-map suitable for representing numeric
ranges.

Equivalent to
@schemeblock[
(make-interval-map = < (lambda (x y) (lambda (z) (+ z (- y x)))))
]
}

@defproc[(interval-map? [v any/c])
         boolean?]{

Returns @scheme[#t] if @scheme[v] is an interval-map, @scheme[#f]
otherwise.
}

@defproc[(interval-map-with-translate? [v any/c])
         boolean?]{

Returns @scheme[#t] if @scheme[v] is an interval-map constructed with
support for translation of keys, @scheme[#f] otherwise.
}

@defproc[(interval-map-ref [interval-map interval-map?]
                           [position any/c]
                           [default any/c (lambda () (error ....))])
         any/c]{

Return the value associated with @scheme[position] in
@scheme[interval-map]. If no mapping is found, @scheme[default] is
applied if it is a procedure, or returned otherwise.
}

@defproc[(interval-map-set! [interval-map interval-map?]
                            [start any/c]
                            [end any/c]
                            [value any/c])
         void?]{

Updates @scheme[interval-map], associating every position in
[@scheme[start], @scheme[end]) with @scheme[value].

Existing interval mappings contained in [@scheme[start], @scheme[end])
are destroyed, and partly overlapping intervals are truncated. See
@scheme[interval-map-update*!] for an updating procedure that
preserves distinctions within [@scheme[start], @scheme[end]).
}

@defproc[(interval-map-update*! [interval-map interval-map?]
                                [start any/c]
                                [end any/c]
                                [updater (any/c . -> . any/c)]
                                [default any/c (lambda () (error ....))])
         void?]{

Updates @scheme[interval-map], associating every position in
[@scheme[start], @scheme[end]) with the result of applying
@scheme[updater] to the position's previously associated value, or to
the default value produced by @scheme[default] if no mapping exists.

Unlike @scheme[interval-map-set!], @scheme[interval-map-update*!]
preserves existing distinctions within [@scheme[start], @scheme[end]).
}

@defproc[(interval-map-remove! [interval-map interval-map?]
                               [start any/c]
                               [end any/c])
         void?]{

Removes the value associated with every position in [@scheme[start],
@scheme[end]).
}

@defproc[(interval-map-expand! [interval-map interval-map-with-translate?]
                               [start any/c]
                               [end any/c])
         void?]{

Expands @scheme[interval-map]'s domain by introducing a gap
[@scheme[start], @scheme[end]) and adjusting intervals after
@scheme[start] using @scheme[(_translate start end)].

If @scheme[interval-map] was not constructed with a
@scheme[_translate] argument, an exception is raised. If
@scheme[start] is not less than @scheme[end], an exception is raised.
}

@defproc[(interval-map-contract! [interval-map interval-map-with-translate?]
                                 [start any/c]
                                 [end any/c])
         void?]{

Contracts @scheme[interval-map]'s domain by removing all mappings on
the interval [@scheme[start], @scheme[end]) and adjusting intervals
after @scheme[end] using @scheme[(_translate end start)].

If @scheme[interval-map] was not constructed with a
@scheme[_translate] argument, an exception is raised. If
@scheme[start] is not less than @scheme[end], an exception is raised.
}

@defproc[(interval-map-cons*! [interval-map interval-map?]
                              [start any/c]
                              [end any/c]
                              [v any/c]
                              [default any/c null])
         void?]{

Same as the following:
@schemeblock[
(interval-map-update*! interval-map start end
                       (lambda (old) (cons v old))
                       default)
]
}

@defproc[(interval-map-iter? [v any/c])
         boolean?]{

Returns @scheme[#t] if @scheme[v] represents a position in an
interval-map, @scheme[#f] otherwise.
}
