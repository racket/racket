#lang scribble/manual
@(require scribble/eval
          (for-label data/splay-tree
                     racket/contract
                     racket/dict
                     racket/base))

@title{Splay Trees}

@(define the-eval (make-base-eval))
@(the-eval '(require data/splay-tree))
@(the-eval '(require racket/dict))

@defmodule[data/splay-tree]

@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

Splay trees are an efficient data structure for mutable dictionaries
with totally ordered keys. They were described in the paper
``Self-Adjusting Binary Search Trees'' by Daniel Sleator and Robert
Tarjan in Journal of the ACM 32(3) pp652-686.

A splay-tree is a dictionary (@racket[dict?] from
@racketmodname[racket/dict]). It also supports extensions of the
dictionary interface for iterator-based search.

@defproc[(make-splay-tree [=? (-> any/c any/c any/c)]
                          [<? (-> any/c any/c any/c)]
                          [#:key-contract key-contract contract? any/c]
                          [#:value-contract value-contract contract? any/c])
         splay-tree?]{

Makes a new empty splay-tree. The splay tree uses @racket[=?] and
@racket[<?] to compare keys.

@examples[#:eval the-eval
(define splay-tree (make-splay-tree string=? string<?))
(splay-tree-set! splay-tree "dot" 10)
(splay-tree-set! splay-tree "cherry" 500)
(dict-map splay-tree list)
(splay-tree-ref splay-tree "dot")
(splay-tree-remove! splay-tree "cherry")
(splay-tree-count splay-tree)
]
}

@defproc[(make-integer-splay-tree [#:adjust? adjust boolean? #f]
                                  [#:key-contract key-contract contract? any/c]
                                  [#:value-contract value-contract contract? any/c])
         splay-tree?]{

Makes a new empty splay-tree that permits only exact integers as keys
(in addition to any constraints imposed by @racket[key-contract]). If
@racket[adjust?] is true, then the resulting splay tree answers true
to @racket[splay-tree-with-adjust?] and supports efficient key
adjustment.

@examples[#:eval the-eval
(define splay-tree (make-integer-splay-tree))
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

@defproc[(splay-tree-with-adjust? [s splay-tree?]) boolean?]{

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

@defproc[(splay-tree-remove-range! [s splay-tree?] [from any/c] [to any/c])
         void?]{

Removes all keys in [@racket[from], @racket[to]); that is, all keys
greater than or equal to @racket[from] and less than @racket[to].
}

@defproc[(splay-tree-contract! [s (and/c splay-tree? splay-tree-with-adjust?)]
                               [from any/c] [to any/c])
         void?]{

Like @racket[splay-tree-remove-range!], but also decreases the value
of all keys greater than or equal to @racket[to] by @racket[(- to
from)].
}

@defproc[(splay-tree-expand! [s (and/c splay-tree? splay-tree-with-adjust?)]
                             [from any/c] [to any/c])
         void?]{

Increases the value of all keys greater than or equal to @racket[from]
by @racket[(- to from)].
}

@deftogether[[
@defproc[(splay-tree-iterate-greatest/<? [s splay-tree?] [key any/c])
         (or/c #f splay-tree-iter?)]
@defproc[(splay-tree-iterate-greatest/<=? [s splay-tree?] [key any/c])
         (or/c #f splay-tree-iter?)]
@defproc[(splay-tree-iterate-least/>? [s splay-tree?] [key any/c])
         (or/c #f splay-tree-iter?)]
@defproc[(splay-tree-iterate-least/>=? [s splay-tree?] [key any/c])
         (or/c #f splay-tree-iter?)]]]{

Return the position of, respectively, the greatest key less than
@racket[key], the greatest key less than or equal to @racket[key], the
least key greater than @racket[key], and the least key greater than or
equal to @racket[key].
}

@defproc[(splay-tree-iter? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] represents a position in a
splay-tree, @racket[#f] otherwise.
}

@defproc[(splay-tree->list [s splay-tree?]) (listof pair?)]{

Returns an association list with the keys and values of @racket[s], in
order.
}
