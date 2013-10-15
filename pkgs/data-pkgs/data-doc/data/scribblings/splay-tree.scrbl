#lang scribble/manual
@(require scribble/eval
          (for-label data/splay-tree
                     data/order
                     racket/contract
                     racket/dict
                     racket/base))

@title{Splay Trees}

@(define the-eval (make-base-eval))
@(the-eval '(require racket/dict data/order data/splay-tree))

@defmodule[data/splay-tree]

@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

Splay trees are an efficient data structure for mutable dictionaries
with totally ordered keys. They were described in the paper
``Self-Adjusting Binary Search Trees'' by Daniel Sleator and Robert
Tarjan in Journal of the ACM 32(3) pp652-686.

A splay-tree is a ordered dictionary (@racket[dict?] and
@racket[ordered-dict?]).

Operations on splay-trees are not thread-safe. If a key in a
splay-tree is mutated, the splay-tree's internal invariants may be
violated, causing its behavior to become unpredictable.


@defproc[(make-splay-tree [ord order? datum-order]
                          [#:key-contract key-contract contract? any/c]
                          [#:value-contract value-contract contract? any/c])
         splay-tree?]{

Makes a new empty splay-tree. The splay tree uses @racket[ord] to
order keys; in addition, the domain contract of @racket[ord] is
combined with @racket[key-contract] to check keys.

@examples[#:eval the-eval
(define splay-tree
  (make-splay-tree (order 'string-order string? string=? string<?)))
(splay-tree-set! splay-tree "dot" 10)
(splay-tree-set! splay-tree "cherry" 500)
(dict-map splay-tree list)
(splay-tree-ref splay-tree "dot")
(splay-tree-remove! splay-tree "cherry")
(splay-tree-count splay-tree)
(splay-tree-set! splay-tree 'pear 3)
]
}

@defproc[(make-adjustable-splay-tree
           [#:key-contract key-contract contract? any/c]
           [#:value-contract value-contract contract? any/c])
         splay-tree?]{

Makes a new empty splay-tree that permits only exact integers as keys
(in addition to any constraints imposed by @racket[key-contract]). The
resulting splay tree answers true to @racket[adjustable-splay-tree?]
and supports efficient key adjustment.

@examples[#:eval the-eval
(define splay-tree (make-adjustable-splay-tree))
(splay-tree-set! splay-tree 3 'apple)
(splay-tree-set! splay-tree 6 'cherry)
(dict-map splay-tree list)
(splay-tree-ref splay-tree 3)
(splay-tree-remove! splay-tree 6)
(splay-tree-count splay-tree)
]
}

@defproc[(splay-tree? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a splay-tree, @racket[#f] otherwise.
}

@defproc[(adjustable-splay-tree? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a splay-tree that supports key
adjustment; see @racket[splay-tree-contract!] and
@racket[splay-tree-expand!].
}

@deftogether[[
@defproc[(splay-tree-ref [s splay-tree?]
                         [key any/c]
                         [default any/c (lambda () (error ....))])
         any]
@defproc[(splay-tree-set! [s splay-tree?]
                          [key any/c]
                          [value any/c])
         void?]
@defproc[(splay-tree-remove! [s splay-tree?]
                             [key any/c])
         void?]
@defproc[(splay-tree-count [s splay-tree?]) exact-nonnegative-integer?]
@defproc[(splay-tree-iterate-first [s splay-tree?])
         (or/c #f splay-tree-iter?)]
@defproc[(splay-tree-iterate-next [s splay-tree?] [iter splay-tree-iter?])
         (or/c #f splay-tree-iter?)]
@defproc[(splay-tree-iterate-key [s splay-tree?] [iter splay-tree-iter?])
         any/c]
@defproc[(splay-tree-iterate-value [s splay-tree?] [iter splay-tree-iter?])
         any/c]]]{

Implementations of @racket[dict-ref], @racket[dict-set!],
@racket[dict-remove!], @racket[dict-count],
@racket[dict-iterate-first], @racket[dict-iterate-next],
@racket[dict-iterate-key], and @racket[dict-iterate-value],
respectively.
}

@defproc[(splay-tree-remove-range! [s splay-tree?]
                                   [from any/c]
                                   [to any/c])
         void?]{

Removes all keys in [@racket[from], @racket[to]); that is, all keys
greater than or equal to @racket[from] and less than @racket[to].

This operation takes @italic{O(N)} time, or @italic{O(log N)} time if
@racket[(adjustable-splay-tree? s)].
}

@defproc[(splay-tree-contract! [s adjustable-splay-tree?]
                               [from exact-integer?] [to exact-integer?])
         void?]{

Like @racket[splay-tree-remove-range!], but also decreases the value
of all keys greater than or equal to @racket[to] by @racket[(- to
from)].

This operation is only allowed on adjustable splay trees, and it takes
@italic{O(log N)} time.
}

@defproc[(splay-tree-expand! [s adjustable-splay-tree?]
                             [from exact-integer?] [to exact-integer?])
         void?]{

Increases the value of all keys greater than or equal to @racket[from]
by @racket[(- to from)].

This operation is only allowed on adjustable splay trees, and it takes
@italic{O(log N)} time.
}

@deftogether[[
@defproc[(splay-tree-iterate-least [s splay-tree])
         (or/c #f splay-tree-iter?)]
@defproc[(splay-tree-iterate-greatest [s splay-tree])
         (or/c #f splay-tree-iter?)]
@defproc[(splay-tree-iterate-least/>? [s splay-tree?] [key any/c])
         (or/c #f splay-tree-iter?)]
@defproc[(splay-tree-iterate-least/>=? [s splay-tree?] [key any/c])
         (or/c #f splay-tree-iter?)]
@defproc[(splay-tree-iterate-greatest/<? [s splay-tree?] [key any/c])
         (or/c #f splay-tree-iter?)]
@defproc[(splay-tree-iterate-greatest/<=? [s splay-tree?] [key any/c])
         (or/c #f splay-tree-iter?)]
]]{

Implementations of @racket[dict-iterate-least],
@racket[dict-iterate-greatest], @racket[dict-iterate-least/>?],
@racket[dict-iterate-least/>=?], @racket[dict-iterate-greatest/<?],
and @racket[dict-iterate-greatest/<=?], respectively.
}

@defproc[(splay-tree-iter? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] represents a position in a
splay-tree, @racket[#f] otherwise.
}

@defproc[(splay-tree->list [s splay-tree?]) (listof pair?)]{

Returns an association list with the keys and values of @racket[s], in
order.
}


@close-eval[the-eval]
