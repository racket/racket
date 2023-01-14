#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/fixnum
                     racket/fasl
                     racket/serialize)
          (for-syntax racket/base))

@(define-syntax (sv-example stx)
   (syntax-case stx ()
     [(_ . strs)
      (with-syntax ([(form ...) (for/list ([str (in-list (syntax->datum #'strs))]
                                           #:unless (equal? "\n" str))
                                  (with-syntax ([str str]
                                                [e (read (open-input-string str))])
                                    #'(eval:alts #,(code str) e)))])
         #'@mz-examples[form ...])]))

@title[#:tag "stencil vectors"]{Stencil Vectors}

A @deftech{stencil vector} is like a @tech{vector}, but it has an
associated mask @tech{fixnum} where the number of bits set in the mask
determines the length of the vector. A stencil vector is useful for
implementing some data structures @cite["Torosyan21"], such as a hash
array mapped trie (HAMT).

Conceptually, a stencil vector's mask indicates which virtual elements
of a full-sized stencil vector are present, but mask bits have no
effect on access or mutation via @racket[stencil-vector-ref] and
@racket[stencil-vector-set!]. For example, such a stencil vector has a
mask @racket[25], which could also be written @racketvalfont{#b11001};
reading from low bit to high, that mask represents values present at
the virtual slots 0, 3, and 4. If that stencil vector's
elements are @racket['a], @racket['b], and @racket['c], then
@racket['a] is at virtual slot 0 and accessed with index @racket[0],
@racket['b] is at virtual slot 3 and accessed with index @racket[1],
and @racket['c] is at virtual slot 4 and accessed with index
@racket[2].

The relative order of bits in a mask @emph{is} relevant for a
functional-update operation with @racket[stencil-vector-update].
Elements to remove are specified with a removal mask, and elements to
add are ordered relative to remaining elements through an addition
mask. For example, starting with the stencil vector whose mask is
@racketvalfont{#b11001} with elements @racket['a], @racket['b], and
@racket['c], adding new elements @racket['d] and @racket['e] using the
addition mask @racketvalfont{#b100100} produces a stencil vector whose
mask is @racketvalfont{#b111101} and whose elements in order are
@racket['a], @racket['d], @racket['b], @racket['c], and @racket['e].

The maximum size of a stencil vector is 58 elements on a 64-bit
platform and 26 elements on a 32-bit platform. This limited size
enables a compact internal representation and ensures that update
operations are relatively simple. Stencil vectors are mutable,
although they are intended primarily for use without mutation to
implement a persistent data structure.

Two stencil vectors are @racket[equal?] if they have the same mask,
and if the values in corresponding slots of the stencil vectors are
@racket[equal?].

A printed vector starts with @litchar{#<stencil ...>}, and this
printed form cannot be parsed by @racket[read]. The
@racket[s-exp->fasl] and @racket[serialize] functions do not support
stencil vectors, in part because a stencil vector on a 64-bit platform
might not be representable on a 32-bit platform. The intent is that
stencil vectors are used as an in-memory representation for a datatype
implementation.

@history[#:added "8.5.0.7"]


@defproc[(stencil-vector? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{stencil vector}, @racket[#f] otherwise.

@sv-example{
(stencil-vector #b10010 'a 'b)
(stencil-vector #b111 'a 'b 'c)
}

@history[#:added "8.5.0.7"]}

@defproc[(stencil-vector-mask-width) exact-nonnegative-integer?]{

Returns the maximum number of elements allowed in a stencil vector on
the current platform. The result is @racket[58] on a 64-bit platform
or @racket[26] on a 32-bit platform.}


@defproc[(stencil-vector [mask (integer-in 0 (sub1 (expt 2 (stencil-vector-mask-width))))]
                         [v any/c]
                         ...)
         stencil-vector?]{

Returns a stencil vector combining @racket[mask] with elements
@racket[v]. The number of supplied @racket[v]s must match the number
of bits set in @racket[mask]'s two's complement representation.

@history[#:added "8.5.0.7"]}


@defproc[(stencil-vector-mask [vec stencil-vector?])
         (integer-in 0 (sub1 (expt 2 (stencil-vector-mask-width))))]{

Returns the mask of @racket[vec]. Note that the mask of a stencil
vector is determined at creation time and cannot be changed later.

@sv-example{
(stencil-vector-mask (stencil-vector #b10010 'a 'b))
}

@history[#:added "8.5.0.7"]}


@defproc[(stencil-vector-length [vec stencil-vector?])
         (integer-in 0 (sub1 (stencil-vector-mask-width)))]{

Returns the length of @racket[vec] (i.e., the number of slots in the
vector). The result is the same as @racket[(fxpopcount (stencil-vector-mask vec))].

@sv-example{
(stencil-vector-length (stencil-vector #b10010 'a 'b))
}

@history[#:added "8.5.0.7"]}


@defproc[(stencil-vector-ref [vec stencil-vector?]
                             [pos exact-nonnegative-integer?])
         any/c]{

Returns the element in slot @racket[pos] of @racket[vec]. The first
slot is position @racket[0], and the last slot is one less than
@racket[(stencil-vector-length vec)].

@sv-example{
(stencil-vector-ref (stencil-vector #b10010 'a 'b) 1)
(stencil-vector-ref (stencil-vector #b111 'a 'b 'c) 1)
}

@history[#:added "8.5.0.7"]}


@defproc[(stencil-vector-set! [vec stencil-vector?]
                              [pos exact-nonnegative-integer?]
                              [v any/c])
         void?]{

Updates the slot @racket[pos] of @racket[vec] to contain @racket[v].

@sv-example{
(define st-vec (stencil-vector #b101 'a 'b))
st-vec
(stencil-vector-set! st-vec 1 'c)
st-vec}

@history[#:added "8.5.0.7"]}


@defproc[(stencil-vector-update [vec stencil-vector?]
                                [remove-mask (integer-in 0 (sub1 (expt 2 (stencil-vector-mask-width))))]
                                [add-mask (integer-in 0 (sub1 (expt 2 (stencil-vector-mask-width))))]
                                [v any/c]
                                ...)
         stencil-vector?]{

Returns a stencil vector that is like @racket[vec], but with elements
corresponding to @racket[remove-mask] removed, and with the given
@racket[v]s added at positions relative to existing (unremoved)
elements determined by @racket[add-mask].

@sv-example{
(define st-vec (stencil-vector #b101 'a 'b))
(stencil-vector-update st-vec #b0 #b10 'c)
(stencil-vector-update st-vec #b0 #b1000 'c)
st-vec ; unchanged by updates
(stencil-vector-update st-vec #b1 #b1 'c)
(stencil-vector-update st-vec #b100 #b100 'c)
(stencil-vector-update st-vec #b100 #b0)
}

@history[#:added "8.5.0.7"]}
