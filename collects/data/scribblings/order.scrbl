#lang scribble/manual
@(require scribble/eval
          (for-label data/order
                     racket/contract
                     unstable/prop-contract
                     racket/dict
                     racket/base))

@title{Orders and Ordered Dictionaries}

@(define the-eval (make-base-eval))
@(the-eval '(require racket/dict data/order))

@defmodule[data/order]

@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

This library defines @deftech{orders} and the @deftech{ordered
dictionary} generic interface.

@defthing[ordering/c flat-contract?]{

Contract for orderings, represented by the symbols @racket['=],
@racket['<], and @racket['>].
}

@defthing[prop:ordered-dict
          (struct-type-property/c
           (vector-immutableof _e/c _e/c _s/c _s/c _s/c _s/c))]{

Struct-type property for defining new ordered dictionary types. The
value associated with @racket[prop:ordered-dict] should be an
immutable vector of six procedures, two ``extrema'' procedures and
four ``search'' procedures. The extrema procedures must satisfy
@racket[_e/c] and the search procedures must satisfy @racket[_s/c]:

@racketblock[
_e/c = (->i ([d ordered-dict?])
            [_ (d) (or/c #f (dict-iter-contract d))])
_s/c = (->i ([d ordered-dict?]
             [k (d) (dict-key-contract d)])
            [_ (d) (or/c #f (dict-iter-contract d))])
]

The procedures are implementations of the following generic functions:

@itemize[
@item{@racket[dict-iterate-least]}
@item{@racket[dict-iterate-greatest]}
@item{@racket[dict-iterate-least/>?]}
@item{@racket[dict-iterate-least/>=?]}
@item{@racket[dict-iterate-greatest/<?]}
@item{@racket[dict-iterate-greatest/<=?]}
]

A struct type that implements @racket[prop:ordered-dict] must also
implement @racket[prop:dict].
}

@defproc[(ordered-dict? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is an instance of a struct
implementing the @tech{ordered dictionary} interface (via
@racket[prop:ordered-dict]).
}

@deftogether[[
@defproc[(dict-iterate-least [dict ordered-dict?]) any/c]
@defproc[(dict-iterate-greatest [dict ordered-dict?]) any/c]]]{

Returns the position of the least (greatest) key in the ordered
dictionary @racket[dict]. If @racket[dict] is empty, @racket[#f] is
returned.
}

@deftogether[[
@defproc[(dict-iterate-least/>? [dict ordered-dict?] [key any/c]) any/c]
@defproc[(dict-iterate-least/>=? [dict ordered-dict?] [key any/c]) any/c]
@defproc[(dict-iterate-greatest/<? [dict ordered-dict?] [key any/c]) any/c]
@defproc[(dict-iterate-greatest/<=? [dict ordered-dict?] [key any/c]) any/c]
]]{

Returns the position of the least key greater than @racket[key], the
least key greater than or equal to @racket[key], the greatest key less
than @racket[key], and the greatest key less than or equal to
@racket[key], respectively. If no key satisfies the criterion,
@racket[#f] is returned.
}

@defproc*[([(order [name symbol?] 
                   [domain-contract contract?]
                   [comparator (-> any/c any/c ordering/c)])
            (and/c order? procedure?)]
           [(order [name symbol?]
                   [domain-contract contract?]
                   [=? (-> any/c any/c boolean?)]
                   [<? (-> any/c any/c boolean?)]
                   [>? (-> any/c any/c boolean?)
                       (lambda (x y) (<? y x))])
            (and/c order? procedure?)])]{

Produces a named order object encapsulating a domain contract and a
comparator function. If a single procedure is given, it is used
directly as the comparator. If two or three procedures are given, they
are used to construct the comparator.

The @racket[domain-contract] is not applied to the comparison
function; rather, clients of the order are advised to incorporate the
domain contracts into their own contracts. For example, when a
splay-tree (see @racketmodname[data/splay-tree]) is constructed with
an order, it applies the domain-contract to its keys. Thus the
contract is checked once per dictionary procedure call, rather than on
every comparison.

An order object is applicable as a procedure; it behaves as its
comparator.

@examples[#:eval the-eval
(define string-order (order 'string-order string? string=? string<?))
(string-order "abc" "acdc")
(string-order "x" 12)
]
}

@defproc[(order? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is an order object, @racket[#f]
otherwise.
}

@defproc[(order-comparator [ord order?])
         (-> any/c any/c ordering/c)]{

Extracts the comparator function from an order object.
}

@defproc[(order-domain-contract [ord order?]) contract?]{

Extracts the domain contract from an order object.
}

@deftogether[[
@defproc[(order-=? [ord order?]) (-> any/c any/c boolean?)]
@defproc[(order-<? [ord order?]) (-> any/c any/c boolean?)]
]]{

Returns a procedure representing the order's equality relation or
less-than relation, respectively.
}

@defthing[real-order order?]{

The order of the real numbers. The domain of @racket[real-order]
excludes @racket[+nan.0] but includes @racket[+inf.0] and
@racket[-inf.0]. The standard numeric comparisons (@racket[=],
@racket[<]) are used; exact @racket[1] is equal to inexact
@racket[1.0].

@examples[#:eval the-eval
(real-order 1.0 1)
(real-order 5 7)
(real-order 9.0 3.4)
(real-order 1 +inf.0)
(real-order 5 -inf.0)
]
}

@defthing[datum-order order?]{

An ad hoc order that encompasses many built-in Racket data types. The
@racket[datum-order] comparator orders values of the same data type
according to the data type's natural order: @racket[string=?],
@racket[string<?] for strings, for example (but see the warning about
numbers below). Different data types are ordered arbitrarily but
contiguously; for example, all strings sort before all vectors, or
vice versa. Programs should not rely on the ordering of different data
types.

The order is designed so that lists, vectors, and prefab structs are
ordered lexicographically.

@bold{Warning!} The @racket[datum-order] is not compatible with the
standard numeric order; all exact numbers are ordered before all
inexact numbers. This allows @racket[1] to be considered distinct from
@racket[1.0], for example.

The following built-in data types are currently supported: numbers,
strings, bytes, keywords, symbols, booleans, characters, null, pairs,
vectors, boxes, and prefab structs.

@examples[#:eval the-eval
(datum-order 1 2)
(datum-order 8 5.0)
(datum-order 3+5i 3+2i)
(datum-order '(a #:b c) '(a #:c d c))
(datum-order "apricot" "apple")
(datum-order '#(1 2 3) '#(1 2))
(datum-order '#(1 2 3) '#(1 3))
(datum-order 'apple (box "candy"))
]
}
