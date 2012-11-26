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

A bit vector (bit-vector) is a mutable sequence whose elements 
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
(define bv (make-bit-vector 3))
(bit-vector-ref bv 1)]
}

@defproc[(bit-vector [elem boolean?] ...)
         bit-vector?]{

Creates a new bit-vector containing each @racket[elem] in order.
@examples[#:eval the-eval
(define bv (bit-vector #f #t #f))
(bit-vector-ref bv 1)]
}

@defproc[(bit-vector? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a bit-vector, @racket[#f] otherwise.
}

@defproc[(bit-vector-ref [bv bit-vector?]
                         [index exact-nonnegative-integer?]
                         [default any/c (error ....)])
         any/c]{

Returns the element at index @racket[index], if @racket[index] is less
than @racket[(bit-vector-count gv)]. Otherwise, @racket[default] is
invoked if it is a procedure, returned otherwise.
}

@defproc[(bit-vector-set! 
          [bv bit-vector?]
          [index (and/c exact-nonnegative-integer? 
                        (</c (+ 1 (bit-vector-count vv))))]
          [value boolean?])
         void?]{

Sets the value at index @racket[index] to be @racket[value]. 
@examples[#:eval the-eval
(define bv (bit-vector #f #t #f))
(bit-vector-ref bv 1)]
}

@defproc[(bit-vector-count [bv bit-vector?])
         exact-nonnegative-integer?]{

Returns the number of items in the bit-vector @racket[bv].
}

@defproc[(bit-vector-copy [bv bit-vector?])
         bit-vector?]{

Creates a fresh bit-vector of the same size and with the
same elements as the bit-vector @racket[bv].
}

@defproc[(in-bit-vector [bv bit-vector?])
         sequence?]{

Returns a sequence whose elements are the elements of the bit-vector
@racket[bv]. Mutation of @racket[bv] while the sequence is running
changes the elements produced by the sequence. To obtain a sequence
from a snapshot of @racket[gv], use @racket[(in-vector
(bit-vector-copy bv))] instead.

                      
@examples[#:eval the-eval
(define bv (bit-vector #f #t #f))
(for/list ([x (in-bit-vector bv)]) x)]
}

@defform/subs[(for/bit-vector maybe-length (for-clause ...) body-or-break ... body)
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
(define (to-list bv) (for/list ([x bv]) x))                 
(to-list (for/bit-vector ([i '(1 2 3)]) (odd? i)))
(to-list (for/bit-vector #:length 2 ([i '(1 2 3)]) (odd? i)))
(to-list (for/bit-vector #:length 4 ([i '(1 2 3)]) (odd? i)))
(to-list (for/bit-vector #:length 4 #:fill #t ([i '(1 2 3)]) (odd? i)))
]

The @racket[for/bit-vector] form may allocate a bit-vector and mutate it 
after each iteration of @racket[body], which means that capturing a
continuation during @racket[body] and applying it multiple times may
mutate a shared bit-vector.}

@defform/subs[(for*/bit-vector maybe-length (for-clause ...) body-or-break ... body)
              ([maybe-length (code:line)
                             (code:line #:length length-expr)
                             (code:line #:length length-expr #:fill fill-expr)])
              #:contracts ([length-expr exact-nonnegative-integer?])]{

Like @racket[for/bit-vector] but with the implicit nesting of @racket[for*].
}

@close-eval[the-eval]
