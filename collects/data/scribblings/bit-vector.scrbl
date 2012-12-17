#lang scribble/manual
@(require scribble/eval
          (for-label data/bit-vector
                     racket/contract
                     racket/dict
                     racket/base)
          data/bit-vector)

@title[#:tag "bit-vector"]{Bit Vectors}

@(define the-eval (make-base-eval))
@(the-eval '(require data/bit-vector))
@(the-eval '(require racket/dict))

@defmodule[data/bit-vector]

@author[@author+email["Jens Axel Søgaard" "soegaard@racket-lang.org"]]

A @deftech{bit vector} is a mutable sequence whose elements 
are booleans. A bit vector also acts as a dictionary (@racket[dict?]
from @racketmodname[racket/dict]), where the keys are zero-based
indexes and the values are the elements of the bit-vector. A bit-vector 
has a fixed size.

Two bit-vectors are @racket[equal?] if they contain the same number of
elements and if they contain equal elements at each index.

@defproc[(make-bit-vector [size exact-integer?] [fill boolean? #f])
         bit-vector?]{

Creates a new bit-vector of size @racket[size]. All elements
are initialized to @racket[fill].

@examples[#:eval the-eval
(bit-vector-ref (make-bit-vector 3) 2)
(bit-vector-ref (make-bit-vector 3 #t) 2)
]
}

@defproc[(bit-vector [elem boolean?] ...)
         bit-vector?]{

Creates a new bit-vector containing each @racket[elem] in order.

@examples[#:eval the-eval
(bit-vector-ref (bit-vector #f #t #f) 1)
]
}

@defproc[(bit-vector? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a bit-vector, @racket[#f] otherwise.
}

@defproc[(bit-vector-ref [bv bit-vector?]
                         [index exact-nonnegative-integer?]
                         [default any/c (error ....)])
         any/c]{

Returns the element at index @racket[index], if @racket[index] is less
than @racket[(bit-vector-length bv)]. Otherwise, @racket[default] is
invoked if it is a procedure, returned otherwise.

@examples[#:eval the-eval
(bit-vector-ref (bit-vector #f #t) 1)
(bit-vector-ref (bit-vector #f #t) 5 'not-there)
]
}

@defproc[(bit-vector-set! 
          [bv bit-vector?]
          [index (and/c exact-nonnegative-integer? 
                        (</c (+ 1 (bit-vector-length vv))))]
          [value boolean?])
         void?]{

Sets the value at index @racket[index] to be @racket[value]. 

@examples[#:eval the-eval
(define bv (bit-vector #f #t))
(bit-vector-ref bv 0)
(bit-vector-set! bv 0 #t)
(bit-vector-ref bv 0)
]
}

@defproc[(bit-vector-length [bv bit-vector?])
         exact-nonnegative-integer?]{

Returns the number of items in the bit-vector @racket[bv].
}

@defproc[(bit-vector-popcount [bv bit-vector?])
         exact-nonnegative-integer?]{

Returns the number of set bits in the bit-vector @racket[bv].

@examples[#:eval the-eval
(bit-vector-popcount (bit-vector #f #t #t))]
}

@defproc[(bit-vector-copy [bv bit-vector?]
                          [start exact-nonnegative-integer? 0]
                          [end exact-nonnegative-integer? (vector-length v)])
         bit-vector?]{

Creates a fresh bit-vector with the
same elements as @racket[bv] from @racket[start] (inclusive)
to @racket[end] (exclusive).
}

@defproc[(in-bit-vector [bv bit-vector?])
         sequence?]{

Returns a sequence whose elements are the elements of the bit-vector
@racket[bv]. Mutation of @racket[bv] while the sequence is running
changes the elements produced by the sequence. To obtain a sequence
from a snapshot of @racket[bv], use @racket[(in-bit-vector
(bit-vector-copy bv))] instead.

@examples[#:eval the-eval
(define bv (bit-vector #f #t #f))
(for/list ([x (in-bit-vector bv)]) x)]
}

@defform/subs[(for/bit-vector maybe-length (for-clause ...) 
                body-or-break ... body)
              ([maybe-length (code:line)
                             (code:line #:length length-expr)
                             (code:line #:length length-expr #:fill fill-expr)])
              #:contracts ([length-expr exact-nonnegative-integer?])]{

Iterates like @racket[for/vector], but results are accumulated into
a bit-vector instead of a vector. 

If the optional @racket[#:length] clause is specified, the result of
@racket[length-expr] determines the length of the result bit-vector. 
In that case, the iteration can be performed more efficiently, and it
terminates when the bit-vector is full or the requested number of
iterations have been performed, whichever comes first. If
@racket[length-expr] specifies a length longer than the number of
iterations, then the remaining slots of the vector are initialized to
the value of @racket[fill-expr], which defaults to @racket[#f] (i.e.,
the default argument of @racket[make-bit-vector]).

@examples[#:eval the-eval
(bit-vector->list
 (for/bit-vector ([i '(1 2 3)]) (odd? i)))
(bit-vector->list
 (for/bit-vector #:length 2 ([i '(1 2 3)]) (odd? i)))
(bit-vector->list
 (for/bit-vector #:length 4 ([i '(1 2 3)]) (odd? i)))
(bit-vector->list
 (for/bit-vector #:length 4 #:fill #t ([i '(1 2 3)]) (odd? i)))
]

The @racket[for/bit-vector] form may allocate a bit-vector and mutate it 
after each iteration of @racket[body], which means that capturing a
continuation during @racket[body] and applying it multiple times may
mutate a shared bit-vector.}

@defform[(for*/bit-vector maybe-length (for-clause ...)
           body-or-break ... body)]{

Like @racket[for/bit-vector] but with the implicit nesting of @racket[for*].
}

@deftogether[[
@defproc[(bit-vector->list [bv bit-vector?]) (listof boolean?)]
@defproc[(list->bit-vector [bits (listof boolean?)]) bit-vector?]
@defproc[(bit-vector->string [bv bit-vector?]) (and/c string? #rx"^[01]*$")]
@defproc[(string->bit-vector [s (and/c string? #rx"^[01]*$")]) bit-vector?]
]]{

Converts between bit-vectors and their representations as lists and
strings.

@examples[#:eval the-eval
(bit-vector->list (string->bit-vector "100111"))
(bit-vector->string (list->bit-vector '(#t #f #t #t)))
]
}

@close-eval[the-eval]
