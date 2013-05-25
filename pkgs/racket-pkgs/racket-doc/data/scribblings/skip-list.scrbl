#lang scribble/manual
@(require scribble/eval
          (for-label data/skip-list
                     data/order
                     racket/contract
                     racket/dict
                     racket/base))

@title[#:tag "skip-list"]{Skip Lists}

@(define the-eval (make-base-eval))
@(the-eval '(require racket/dict data/order data/skip-list))

@defmodule[data/skip-list]

@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

Skip lists are a simple, efficient data structure for mutable
dictionaries with totally ordered keys. They were described in the
paper ``Skip Lists: A Probabilistic Alternative to Balanced Trees'' by
William Pugh in Communications of the ACM, June 1990, 33(6) pp668-676.

A skip-list is an ordered dictionary (@racket[dict?] and
@racket[ordered-dict?]). It also supports extensions of the dictionary
interface for iterator-based search and mutation.

Operations on skip-lists are not thread-safe. If a key in a skip-list
is mutated, the skip-list's internal invariants may be violated,
causings its behavior to become unpredictable.


@defproc[(make-skip-list [ord order? datum-order]
                         [#:key-contract key-contract contract? any/c]
                         [#:value-contract value-contract contract? any/c])
         skip-list?]{

Makes a new empty skip-list. The skip-list uses @racket[ord] to order
keys; in addition, the domain contract of @racket[ord] is combined
with @racket[key-contract] to check keys.

@examples[#:eval the-eval
(define skip-list (make-skip-list real-order))
(skip-list-set! skip-list 3 'apple)
(skip-list-set! skip-list 6 'cherry)
(dict-map skip-list list)
(skip-list-ref skip-list 3)
(skip-list-remove! skip-list 6)
(skip-list-count skip-list)
]
}

@defproc[(make-adjustable-skip-list
           [#:key-contract key-contract contract? any/c]
           [#:value-contract value-contract contract? any/c])
         adjustable-skip-list?]{

Makes a new empty skip-list that permits only exact integers as keys
(in addition to any constraints imposed by @racket[key-contract]). The
resulting skip-list answers true to @racket[adjustable-skip-list?]
and supports key adjustment.
}

@defproc[(skip-list? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a skip-list, @racket[#f]
otherwise.
}

@defproc[(adjustable-skip-list? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a skip-list that supports key
adjustment; see @racket[skip-list-contract!] and
@racket[skip-list-expand!].
}

@deftogether[[
@defproc[(skip-list-ref [skip-list skip-list?]
                        [key any/c]
                        [default any/c (lambda () (error ....))])
         any/c]
@defproc[(skip-list-set! [skip-list skip-list?]
                         [key any/c]
                         [value any/c])
         void?]
@defproc[(skip-list-remove! [skip-list skip-list?]
                            [key any/c])
         void?]
@defproc[(skip-list-count [skip-list skip-list?])
         exact-nonnegative-integer?]
@defproc[(skip-list-iterate-first [skip-list skip-list?])
         (or/c skip-list-iter? #f)]
@defproc[(skip-list-iterate-next [skip-list skip-list?]
                                 [iter skip-list-iter?])
         (or/c skip-list-iter? #f)]
@defproc[(skip-list-iterate-key [skip-list skip-list?]
                                [iter skip-list-iter?])
         any/c]
@defproc[(skip-list-iterate-value [skip-list skip-list?]
                                  [iter skip-list-iter?])
         any/c]]]{

Implementations of @racket[dict-ref], @racket[dict-set!],
@racket[dict-remove!], @racket[dict-count],
@racket[dict-iterate-first], @racket[dict-iterate-next],
@racket[dict-iterate-key], and @racket[dict-iterate-value],
respectively.
}

@defproc[(skip-list-remove-range! [skip-list skip-list?]
                                  [from any/c]
                                  [to any/c])
         void?]{

Removes all keys in [@racket[from], @racket[to]); that is, all keys
greater than or equal to @racket[from] and less than @racket[to].
}

@defproc[(skip-list-contract! [skip-list adjustable-skip-list?]
                              [from exact-integer?]
                              [to exact-integer?])
         void?]{

Like @racket[skip-list-remove-range!], but also decreases the value
of all keys greater than or equal to @racket[to] by @racket[(- to
from)].

This operation takes time proportional to the number of elements with
keys greater than or equal to @racket[to].
}

@defproc[(skip-list-expand! [skip-list adjustable-skip-list?]
                            [from exact-integer?]
                            [to exact-integer?])
         void?]{

Increases the value of all keys greater than or equal to @racket[from]
by @racket[(- to from)].

This operation takes time proportional to the number of elements with
keys greater than or equal to @racket[from].
}

@deftogether[[
@defproc[(skip-list-iterate-least/>? [skip-list skip-list?]
                                        [key any/c])
         (or/c skip-list-iter? #f)]
@defproc[(skip-list-iterate-least/>=? [skip-list skip-list?]
                                        [key any/c])
         (or/c skip-list-iter? #f)]
@defproc[(skip-list-iterate-greatest/<? [skip-list skip-list?]
                                        [key any/c])
         (or/c skip-list-iter? #f)]
@defproc[(skip-list-iterate-greatest/<=? [skip-list skip-list?]
                                        [key any/c])
         (or/c skip-list-iter? #f)]
@defproc[(skip-list-iterate-least [skip-list skip-list?])
         (or/c skip-list-iter? #f)]
@defproc[(skip-list-iterate-greatest [skip-list skip-list?])
         (or/c skip-list-iter? #f)]]]{

Implementations of @racket[dict-iterate-least],
@racket[dict-iterate-greatest], @racket[dict-iterate-least/>?],
@racket[dict-iterate-least/>=?], @racket[dict-iterate-greatest/<?],
@racket[dict-iterate-greatest/<=?], @racket[dict-iterate-least], and
@racket[dict-iterate-greatest], respectively.
}

@defproc[(skip-list-iter? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] represents a position in a
skip-list, @racket[#f] otherwise.
}

@defproc[(skip-list->list [skip-list skip-list?]) (listof pair?)]{

Returns an association list with the keys and values of
@racket[skip-list], in order.
}


@close-eval[the-eval]
