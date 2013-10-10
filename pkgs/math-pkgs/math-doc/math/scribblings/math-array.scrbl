#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label racket/base racket/vector racket/match racket/unsafe/ops racket/string
                     (except-in racket/list permutations) ; FIXME
                     math plot
                     (only-in typed/racket/base
                              ann inst : λ: define: make-predicate ->
                              Flonum Real Boolean Any Integer Index Natural Exact-Positive-Integer
                              Nonnegative-Real Sequenceof Fixnum Values Number Float-Complex
                              All U List Vector Listof Vectorof Struct FlVector
                              Symbol Output-Port))
          "utils.rkt")

@(define typed-eval (make-math-eval))
@interaction-eval[#:eval typed-eval
                         (require racket/match
                                  racket/vector
                                  racket/string
                                  racket/sequence
                                  racket/list)]

@title[#:tag "array" #:style 'toc]{Arrays}
@(author-neil)

@bold{Performance Warning:} Indexing the elements of arrays created in untyped Racket
is currently 25-50 times slower than doing the same in Typed Racket, due to the overhead
of checking higher-order contracts. We are working on it.

For now, if you need speed, use the @racketmodname[typed/racket] language.

@defmodule[math/array]

One of the most common ways to structure data is with an array: a rectangular grid of
homogeneous, independent elements. But an array data type
is usually absent from functional languages' libraries. This is probably because arrays
are perceived as requiring users to operate on them using destructive updates, write
loops that micromanage array elements, and in general, stray far from the declarative
ideal.

Normally, they do. However, experience in Python, and more recently Data-Parallel Haskell,
has shown that providing the right data types and a rich collection of whole-array operations
allows working effectively with arrays in a functional, declarative style. As a bonus,
doing so opens the possibility of parallelizing nearly every operation.

@local-table-of-contents[]


@;{==================================================================================================}


@section[#:tag "array:quick"]{Quick Start}

Arrays can be created from expressions denoting each element's value using the @racket[array]
macro:
@interaction[#:eval typed-eval
                    (array #[0 1 2 3 4])
                    (array #[#['first 'row 'data] #['second 'row 'data]])
                    (array "This array has zero axes and one element")]
They can also be created using @racket[build-array] to specify a @tech{shape} and @tech{procedure}:
@interaction[#:eval typed-eval
                    (define arr
                      (build-array #(4 5) (λ: ([js : Indexes])
                                            (match-define (vector j0 j1) js)
                                            (+ j0 j1))))
                    arr]
Other ways to create arrays are to convert them from lists and vectors using
@racket[list->array], @racket[list*->array], @racket[vector->array] and @racket[vector*->array],
and to generate them in a loop using @racket[for/array:] and @racket[for*/array:].

Arrays can be indexed using @racket[array-ref], and settable arrays can be mutated using
@racket[array-set!]:
@interaction[#:eval typed-eval
                    (array-ref arr #(2 3))
                    (define brr (array->mutable-array arr))
                    (array-set! brr #(2 3) -1000)
                    brr]
However, both of these activities are discouraged in favor of functional, whole-array operations.

Arrays can be mapped over and otherwise operated on @tech{pointwise}:
@interaction[#:eval typed-eval
                    (array-map (λ: ([n : Natural]) (* 2 n)) arr)
                    (array+ arr arr)]
When arrays have different shapes, they can often be @tech{broadcast}, or stretched, to be the same
shape before applying the pointwise operation:
@interaction[#:eval typed-eval
                    (array* arr (array 2))
                    (array* arr (array #[0 2 0 2 0]))]
By default, zero-dimensional arrays like @racket[(array 2)] can be broadcast to any shape.
See @secref{array:broadcasting} for details.

Arrays can be @tech[#:key "slicing"]{sliced} to yield sub-arrays, using a list of slice specifications
that correspond to array axes.
For example, keeping every row of @racket[arr] and every even-numbered column:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list (::) (:: 0 5 2)))]
Here, @racket[::] has semantics almost, but not quite, entirely unlike @racket[in-range].
See @secref{array:slicing} for details.

Functional code that uses whole-array operations often creates many short-lived, intermediate arrays
whose elements are referred to only once.
The overhead of allocating and filling storage for these arrays can be removed entirely by using
@tech{nonstrict} arrays, sometimes at the cost of making the code's performance more difficult to
reason about.
Another bonus is that computations with nonstrict arrays have fewer synchronization points, meaning
that they will be easier to parallelize as Racket's support for parallel computation improves.
See @secref{array:nonstrict} for details.


@;{==================================================================================================}


@section[#:tag "array:defs"]{Definitions}

An array's domain is determined by its @deftech{shape}, a vector of nonnegative integers such as
@racket[#(4 5)], @racket[#(10 1 5 8)] or @racket[#()].
The shape's length is the number of array dimensions, or @deftech{axes}.
The shape's contents are the length of each axis.

The product of the axis lengths is the array's size. In particular, an array with shape
@racket[#()] has one element.

@deftech{Indexes} are a vector of nonnegative integers that identify a particular element.
Indexes are in-bounds when there are the same number of them as axes, and each is
@italic{less than} its corresponding axis length.

An array's contents are determined by its @deftech{procedure}, which returns an element when applied
to in-bounds @tech{indexes}.
By default, most arrays' procedures look up elements in memory.
Others, such as those returned by @racket[make-array], return computed values.

A @deftech{pointwise} operation is one that operates on each array element independently, on each
corresponding pair of elements from two arrays independently, or on a corresponding collection
of elements from many arrays independently. This is usually done using @racket[array-map].

When a pointwise operation is performed on arrays with different shapes, the arrays are
@deftech{broadcast} so that their shapes match. See @secref{array:broadcasting} for details.


@;{==================================================================================================}


@section[#:tag "array:broadcasting"]{Broadcasting}

It is often useful to apply a @tech{pointwise} operation to two or more arrays in a many-to-one
manner. Library support for this, which @racketmodname[math/array] provides, is called
@tech[#:key "broadcast"]{broadcasting}.

Creating a 6×6 identity matrix:
@interaction[#:eval typed-eval
                    (define diag (diagonal-array 2 6 1 0))
                    (array-shape diag)
                    diag]
Multiplying each element by @racket[10]:
@interaction[#:eval typed-eval
                    (array-shape (array 10))
                    (array* diag (array 10))]
Adding @racket[(array #[0 1 2 3 4 5])] pointwise to every row:
@interaction[#:eval typed-eval
                    (array+ (array* diag (array 10))
                            (array #[0 1 2 3 4 5]))]

@subsection[#:tag "array:broadcasting:rules"]{Broadcasting Rules}

Suppose we have two array shapes @racket[ds = (vector d0 d1 ...)] and
@racket[es = (vector e0 e1 ...)]. Broadcasting proceeds as follows:
@itemlist[#:style 'ordered
  @item{The shorter shape is padded on the left with @racket[1] until it is the same length
        as the longer shape.}
  @item{For each axis @racket[k], @racket[dk] and @racket[ek] are compared. If @racket[dk = ek],
        the result axis is @racket[dk]; if one axis is length @racket[1], the result axis
        is the length of the other; otherwise fail.}
  @item{Both arrays' axes are stretched by (conceptually) copying the rows of axes with length
        @racket[1].}
  ]

Example: Suppose we have an array @racket[drr] with shape @racket[ds = #(4 1 3)] and another array
@racket[err] with shape @racket[es = #(3 3)]. Following the rules:
@itemlist[#:style 'ordered
  @item{@racket[es] is padded to get @racket[#(1 3 3)].}
  @item{The result axis is derived from @racket[#(4 1 3)] and @racket[#(1 3 3)] to get
        @racket[#(4 3 3)].}
  @item{@racket[drr]'s second axis is stretched to length @racket[3], and @racket[err]'s new first
        axis (which is length @racket[1] by rule 1) is stretched to length @racket[4].}
  ]

The same example, but more concrete:
@interaction[#:eval typed-eval
                    (define drr
                      (array #[#[#["00" "01" "02"]]
                               #[#["10" "11" "12"]]
                               #[#["20" "21" "22"]]
                               #[#["30" "31" "32"]]]))
                    (array-shape drr)
                    (define err
                      (array #[#["aa" "ab" "ac"]
                               #["ba" "bb" "bc"]
                               #["ca" "cb" "cc"]]))
                    (array-shape err)
                    (define drr+err (array-map string-append drr err))
                    (array-shape drr+err)
                    drr+err]
Notice how the row @racket[#["00" "01" "02"]] in @racket[drr] is repeated in the result
because @racket[drr]'s second axis was stretched during broadcasting. Also, the column
@racket[#[#["aa"] #["ba"] #["ca"]]] in @racket[err] is repeated because @racket[err]'s first
axis was stretched.

For the above example, @racket[array-map] does this before operating on @racket[drr] and @racket[err]:
@interaction[#:eval typed-eval
                    (define ds (array-shape-broadcast (list (array-shape drr)
                                                            (array-shape err))))
                    ds
                    (array-broadcast drr ds)
                    (array-broadcast err ds)]

@subsection[#:tag "array:broadcasting:control"]{Broadcasting Control}

The parameter @racket[array-broadcasting] controls how pointwise operations @tech{broadcast}
arrays. Its default value is @racket[#t], which means that broadcasting proceeds as described
in @secref{array:broadcasting:rules}. Another possible value is @racket[#f], which allows pointwise
operations to succeed only if array shapes match exactly:
@interaction[#:eval typed-eval
                    (parameterize ([array-broadcasting #f])
                      (array* (index-array #(3 3)) (array 10)))]

Another option is @hyperlink["http://www.r-project.org"]{R}-style permissive broadcasting,
which allows pointwise operations to @italic{always} succeed, by repeating shorter axes' rows
instead of repeating just singleton axes' rows:
@interaction[#:eval typed-eval
                    (define arr10 (array-map number->string (index-array #(10))))
                    (define arr3 (array-map number->string (index-array #(3))))
                    arr10
                    arr3
                    (array-map string-append arr10 (array #["+" "-"]) arr3)
                    (parameterize ([array-broadcasting 'permissive])
                      (array-map string-append arr10 (array #["+" "-"]) arr3))]
Notice that @racket[(array #["+" "-"])] was repeated five times, and that
@racket[arr3] was repeated three full times and once partially.


@;{==================================================================================================}


@section[#:tag "array:slicing"]{Slicing}

One common array transformation is @deftech{slicing}: extracting sub-arrays by picking rows from
each axis independently.

Slicing is done by applying @racket[array-slice-ref] or @racket[array-slice-set!] to an array and a
list of @deftech{slice specifications} corresponding to array @tech{axes}.
There are five types of slice specification:
@itemlist[@item{@racket[(Sequenceof Integer)]: pick rows from an axis by index.}
          @item{@racket[Slice]: pick rows from an axis as with an @racket[in-range] sequence.}
          @item{@racket[Slice-Dots]: preserve remaining adjacent axes}
          @item{@racket[Integer]: remove an axis by replacing it with one of its rows.}
          @item{@racket[Slice-New-Axis]: insert an axis of a given length.}]

Create @racket[Slice] objects using @racket[::] and @racket[Slice-New-Axis] objects using
@racket[::new]. There is only one @racket[Slice-Dots] object, namely @racket[::...].

When slicing an array with @racket[n] axes, unless a list of slice specifications contains
@racket[::...], it must contain exactly @racket[n] slice specifications.

The remainder of this section uses the following example array:
@interaction[#:eval typed-eval
                    (define arr
                      (build-array
                       #(2 3 4)
                       (λ: ([js : Indexes])
                         (string-append* (map number->string (vector->list js))))))
                    arr]

@subsection{@racket[(Sequenceof Integer)]: pick rows}

Using a sequence of integers as a slice specification picks rows from the corresponding axis. For
example, we might use lists of integers to pick @italic{every} row from every axis:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list '(0 1) '(0 1 2) '(0 1 2 3)))]
This simply copies the array.

More usefully, we can use sequences to swap rows on the same axis:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list '(1 0) '(0 1 2) '(0 1 2 3)))]
We can also remove rows:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list '(0 1) '(0 2) '(0 2)))
                    (array-slice-ref arr (list '(0 1) '(0 1 2) '()))]
Or duplicate rows:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list '(0 1) '(0 1 2) '(0 0 1 2 2 3)))]
However, a sequence slice specification cannot alter the number of axes.

Using sequence constructors like @racket[in-range], we can pick every even-indexed row in an axis:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list '(1 0) '(0 1 2) (in-range 0 4 2)))]
We could also use @racket[in-range] to pick every row instead of enumerating their indexes in a
list, but that would require another kind of tedium:
@interaction[#:eval typed-eval
                    (define ds (array-shape arr))
                    (array-slice-ref arr (list (in-range (vector-ref ds 0))
                                               (in-range (vector-ref ds 1))
                                               (in-range (vector-ref ds 2))))]
The situation calls for an @racket[in-range]-like slice specification that is aware of the lengths
of the axes it is applied to.

@subsection{@racket[Slice]: pick rows in a length-aware way}

As a slice specification, a @racket[Slice] object acts like the sequence object returned
by @racket[in-range], but either @racket[start] or @racket[end] may be @racket[#f].

If @racket[start] is @racket[#f], it is interpreted as the first valid axis index in the direction
of @racket[step]. If @racket[end] is @racket[#f], it is interpreted as the last valid axis index
in the direction of @racket[step].

Possibly the most common slice is @racket[(::)], equivalent to @racket[(:: #f #f 1)]. With a
positive @racket[step = 1], @racket[start] is interpreted as @racket[0] and @racket[end] as
the length of the axis. Thus, @racket[(::)] picks all rows from any axis:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list (::) (::) (::)))]
The slice @racket[(:: #f #f -1)] reverses an axis:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list (::) (::) (:: #f #f -1)))]
The slice @racket[(:: 2 #f 1)] picks every row starting from index @racket[2]:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list (::) (::) (:: 2 #f 1)))]
The slice @racket[(:: 1 #f 2)] picks every odd-indexed row:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list (::) (::) (:: 1 #f 2)))]
Notice that every example starts with two @racket[(::)]. In fact, slicing only one axis is so
common that there is a slice specification object that represents any number of @racket[(::)].

@subsection{@racket[Slice-Dots]: preserve remaining axes}

As a slice specification, a @racket[Slice-Dots] object represents any number of leftover, adjacent
axes, and preserves them all.

For example, picking every odd-indexed row of the last axis can be done by
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list ::... (:: 1 #f 2)))]
For @racket[arr] specifically, @racket[::...] represents two @racket[(::)].

Slicing only the first axis while preserving the rest can be done by
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list '(0) ::...))]
If more than one @racket[::...] appears in the list, only the first is expanded:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list ::... '(1) ::...))
                    (array-slice-ref arr (list ::... '(1)))]
If there are no leftover axes, @racket[::...] does nothing when placed in any position:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list ::... '(1) '(1) '(1)))
                    (array-slice-ref arr (list '(1) ::... '(1) '(1)))
                    (array-slice-ref arr (list '(1) '(1) ::... '(1)))
                    (array-slice-ref arr (list '(1) '(1) '(1) ::...))]

@subsection{@racket[Integer]: remove an axis}

All of the slice specifications so far preserve the dimensions of the array. Removing an axis
can be done by using an integer as a slice specification.

This example removes the first axis by collapsing it to its first row:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list 0 ::...))]
Removing the second axis by collapsing it to the row with index @racket[1]:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list (::) 1 ::...))]
Removing the second-to-last axis (which for @racket[arr] is the same as the second):
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list ::... 1 (::)))]
All of these examples can be done using @racket[array-axis-ref]. However, removing an axis
relative to the dimension of the array (e.g. the second-to-last axis) is easier to do
using @racket[array-slice-ref], and it is sometimes convenient to combine axis removal with
other slice operations.

@subsection{@racket[Slice-New-Axis]: add an axis}

As a slice specification, @racket[(::new dk)] inserts @racket[dk] into the resulting array's
shape, in the corresponding axis position. The new axis has length @racket[dk], which must be
nonnegative.

For example, we might conceptually wrap another @racket[#[]] around an array's data:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list (::new) ::...))]
Or duplicate the array twice, within two new outer rows:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list (::new 2) ::...))]
Of course, @racket[dk = 0] is a valid new axis length, but is usually not very useful:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list (::) (::new 0) ::...))]
Inserting axes can also be done using @racket[array-axis-insert].


@;{==================================================================================================}


@section[#:tag "array:nonstrict"]{Nonstrict Arrays}

With few exceptions, by default, the functions exported by @racketmodname[math/array] return
@deftech{strict} arrays, which are arrays whose @tech{procedures} compute elements by looking them
up in a vector.

This conservative default often wastes time and space. In functional code that operates on arrays,
the elements in most intermediate arrays are referred to exactly once, so allocating and filling
storage for them should be unnecessary. For example, consider the following array:
@interaction[#:eval typed-eval
                    (define (make-hellos)
                      (array-map string-append
                                 (array-map string-append
                                            (array #["Hello " "Hallo " "Jó napot "])
                                            (array #["Ada" "Edsger" "John"]))
                                 (make-array #(3) "!")))
                    (define arr (make-hellos))
                    (array-strict? arr)
                    arr]
By default, the result of the inner @racket[array-map] has storage allocated for it and filled with
strings such as @racket["Hello Ada"], even though its storage will be thrown away at the next garbage
collection cycle.

An additional concern becomes even more important as Racket's support for parallel computation
improves. Allocating storage for intermediate arrays is a synchronization point in long computations,
which divides them into many short computations, making them difficult to parallelize.

@(define array-pdf
   "http://research.microsoft.com/en-us/um/people/simonpj/papers/ndp/RArrays.pdf")

@margin-note*{* @bold{Regular, shape-polymorphic, parallel arrays in Haskell},
               Gabriele Keller, Manuel Chakravarty, Roman Leshchinskiy, Simon Peyton Jones,
               and Ben Lippmeier. ICFP 2010. @hyperlink[array-pdf]{(PDF)}}

A solution is to construct @deftech{nonstrict} arrays*, which are arrays whose procedures can do more
than simply look up elements. Setting the parameter @racket[array-strictness] to @racket[#f] causes
almost all @racketmodname[math/array] functions to return nonstrict arrays:
@interaction[#:eval typed-eval
                    (define arr (parameterize ([array-strictness #f])
                                  (make-hellos)))
                    (array-strict? arr)
                    arr]
In @racket[arr], the first element is the @italic{computation}
@racket[(string-append (string-append "Hello " "Ada") "!")], not the value @racket["Hello Ada!"].
The value @racket["Hello Ada!"] is recomputed every time the first element is referred to.

To use nonstrict arrays effectively, think of every array as if it were the array's
@tech{procedure} itself. In other words,

@nested[#:style 'inset]{@bold{An @italic{array} is just a function with a finite,
                                 rectangular domain.}}

Some arrays are mutable, some are lazy, some are strict, some are sparse, and most
do not even allocate contiguous space to store their elements. All are functions that can be
applied to indexes to retrieve elements.

The two most common kinds of operations, mapping over and transforming arrays, are compositions.
Mapping @racket[f] over array @racket[arr] is nothing more than composing @racket[f] with
@racket[arr]'s procedure.
Transforming @racket[arr] using @racket[g], a function from new indexes to old indexes, is nothing
more than composing @racket[arr]'s procedure with @racket[g].

@subsection{Caching Nonstrict Elements}

@bold{Nonstrict arrays are not lazy.} Very few nonstrict arrays cache computed elements,
but like functions, recompute them every time they are referred to. Unlike functions, they can have
every element computed and cached at once, by making them @tech{strict}.

To compute and store an array's elements, use @racket[array-strict!] or @racket[array-strict]:
@interaction[#:eval typed-eval
                    (array-strict? arr)
                    (array-strict! arr)
                    (array-strict? arr)
                    (array-strict arr)]
If the array is already strict, as in the last example above, @racket[array-strict!] and
@racket[array-strict] do nothing.

To make a strict @italic{copy} of an array without making the original array strict, use
@racket[array->mutable-array].

@subsection{Performance Considerations}

One downside to nonstrict arrays is that it is more difficult to reason about the performance of
operations on them. Another is that the user must decide which arrays to make strict. Fortunately,
there is a simple rule of thumb:

@nested[#:style 'inset]{@bold{Make arrays strict when you must refer to most of their elements
                              more than once or twice.}}

Having to name an array is a good indicator that it should be strict. In the
following example, which computes @racket[(+ (expt x x) (expt x x))] for @racket[x] from @racket[0]
to @racket[2499], each element in @racket[xrr] is computed twice whenever its corresponding element
in @racket[res] is referred to:
@racketblock[(define xrr (array-map expt
                                    (index-array #(50 50))
                                    (index-array #(50 50))))
             (define res (array+ xrr xrr))]
Having to name @racket[xrr] means we should make it strict:
@racketblock[(define xrr (array-strict
                          (array-map expt
                                     (index-array #(50 50))
                                     (index-array #(50 50)))))
             (define res (array+ xrr xrr))]
Doing so halves the time it takes to compute @racket[res]'s elements.

When returning an array from a function, return nonstrict arrays as they are, to allow the caller to
decide whether the result should be strict.

When writing library functions that may be called with either @racket[(array-strictness #t)] or
@racket[(array-strictness #f)], operate on nonstrict arrays and wrap the result with
@racket[array-default-strict] to return what the user is expecting.
For example, if @racket[make-hellos] is a library function, it should be written as
@racketblock[(define (make-hellos)
               (array-default-strict
                (parameterize ([array-strictness #f])
                  (array-map string-append
                             (array-map string-append
                                        (array #["Hello " "Hallo " "Jó napot "])
                                        (array #["Ada" "Edsger" "John"]))
                             (make-array #(3) "!")))))]

If you cannot determine whether to make arrays strict, or are using arrays for so-called
``dynamic programming,'' you can make them lazy using @racket[array-lazy].


@;{==================================================================================================}


@section[#:tag "array:types"]{Types, Predicates and Accessors}

@defform[(Array A)]{
The parent array type. Its type parameter is the type of the array's elements.

The polymorphic @racket[Array] type is @italic{covariant}, meaning that @racket[(Array A)] is a
subtype of @racket[(Array B)] if @racket[A] is a subtype of @racket[B]:
@interaction[#:eval typed-eval
                    (define arr (array #[1 2 3 4 5]))
                    arr
                    (ann arr (Array Real))
                    (ann arr (Array Any))]
Because subtyping is transitive, the @racket[(Array A)] in the preceeding subtyping rule can be
replaced with any of @racket[(Array A)]'s subtypes, including descendant types of @racket[Array].
For example, @racket[(Mutable-Array A)] is a subtype of @racket[(Array B)] if @racket[A] is a
subtype of @racket[B]:
@interaction[#:eval typed-eval
                    (define arr (mutable-array #[1 2 3 4 5]))
                    arr
                    (ann arr (Array Real))
                    (ann arr (Array Any))]
}

@defform[(Settable-Array A)]{
The parent type of arrays whose elements can be mutated. Functions like @racket[array-set!] and
@racket[array-slice-set!] accept arguments of this type. Examples of subtypes are
@racket[Mutable-Array], @racket[FlArray] and @racket[FCArray].

This type is @italic{invariant}, meaning that @racket[(Settable-Array A)] is @bold{not} a subtype
of @racket[(Settable-Array B)] if @racket[A] and @racket[B] are different types, even if @racket[A]
is a subtype of @racket[B]:
@interaction[#:eval typed-eval
                    (define arr (mutable-array #[1 2 3 4 5]))
                    arr
                    (ann arr (Settable-Array Integer))
                    (ann arr (Settable-Array Real))]
}

@defform[(Mutable-Array A)]{
The type of mutable arrays. Its type parameter is the type of the array's elements.

Arrays of this type store their elements in a @racket[(Vectorof A)]:
@interaction[#:eval typed-eval
                    (define arr (mutable-array #[#[1 2] #[3 4]]))
                    (vector-set! (mutable-array-data arr) 0 -10)
                    arr]
Mutable arrays are always @tech{strict}.
}

@defidform[Indexes]{
The type of array shapes and array indexes @italic{produced} by @racketmodname[math/array] functions.
Defined as @racket[(Vectorof Index)].
@examples[#:eval typed-eval
                 (array-shape (array #[#[#[0]]]))]
}

@defidform[In-Indexes]{
The type of array shapes and array indexes @italic{accepted} by @racketmodname[math/array] functions.
Defined as @racket[(U Indexes (Vectorof Integer))].

@examples[#:eval typed-eval
                 (define ds #(3 2))
                 ds
                 (make-array ds (void))]

This makes indexes-accepting functions easier to use, because it is easier to convince Typed
Racket that a vector contains @racket[Integer] elements than that a vector contains @racket[Index]
elements.

@racket[In-Indexes] is not defined as @racket[(Vectorof Integer)] because mutable container types
like @racket[Vector] and @racket[Vectorof] are invariant.
In particular, @racket[(Vectorof Index)] is not a subtype of @racket[(Vectorof Integer)]:
@interaction[#:eval typed-eval
                    (define js ((inst vector Index) 3 4 5))
                    js
                    (ann js (Vectorof Integer))
                    (ann js In-Indexes)]
}

@deftogether[(@defproc[(array? [v Any]) Boolean]
              @defproc[(settable-array? [v Any]) Boolean]
              @defproc[(mutable-array? [v Any]) Boolean])]{
Predicates for the types @racket[Array], @racket[Settable-Array], and @racket[Mutable-Array].

Because @racket[Settable-Array] and its descendants are invariant, @racket[settable-array?] and
its descendants' predicates are generally not useful in occurrence typing. For example, if
we know we have an @racket[Array] but would like to treat it differently if it happens to be 
a @racket[Mutable-Array], we are basically out of luck:
@interaction[#:eval typed-eval
                    (: maybe-array-data (All (A) ((Array A) -> (U #f (Vectorof A)))))
                    (define (maybe-array-data arr)
                      (cond [(mutable-array? arr)  (mutable-array-data arr)]
                            [else  #f]))]
In general, predicates with a @racket[Struct] filter do not give conditional branches access
to a struct's accessors. Because @racket[Settable-Array] and its descendants are invariant,
their predicates have @racket[Struct] filters:
@interaction[#:eval typed-eval
                    array?
                    settable-array?
                    mutable-array?]
}

@defproc[(array-shape [arr (Array A)]) Indexes]{
Returns @racket[arr]'s @tech{shape}, a vector of indexes that contains the lengths
of @racket[arr]'s axes.
@examples[#:eval typed-eval
                 (array-shape (array 0))
                 (array-shape (array #[0 1]))
                 (array-shape (array #[#[0 1]]))
                 (array-shape (array #[]))]
}

@defproc[(array-size [arr (Array A)]) Index]{
Returns the number of elements in @racket[arr], which is the product of its axis lengths.
@examples[#:eval typed-eval
                 (array-size (array 0))
                 (array-size (array #[0 1]))
                 (array-size (array #[#[0 1]]))
                 (array-size (array #[]))]
}

@defproc[(array-dims [arr (Array A)]) Index]{
Returns the number of @racket[arr]'s dimensions. Equivalent to
@racket[(vector-length (array-shape arr))].
}

@defproc[(mutable-array-data [arr (Mutable-Array A)]) (Vectorof A)]{
Returns the vector of data that @racket[arr] contains.
}


@;{==================================================================================================}


@section[#:tag "array:construct"]{Construction}

@defform/subs[(array #[#[...] ...] maybe-type-ann)
              [(maybe-type-ann (code:line) (code:line : type))]]{
Creates an @racket[Array] from nested rows of expressions.

The vector syntax @racket[#[...]] delimits rows. These may be nested to any depth, and must have a
rectangular shape. Using square parentheses is not required, but is encouraged to help visually
distinguish array contents from array indexes and other vectors.
(See the examples for @racket[indexes-array] for an illustration.)

@examples[#:eval typed-eval
                 (array 0)
                 (array #[0 1 2 3])
                 (array #[#[1 2 3] #[4 5 6]])
                 (array #[#[1 2 3] #[4 5]])]
As with the @racket[list] constructor, the type chosen for the array is the narrowest type
all the elements can have. Unlike @racket[list], because @racket[array] is syntax, instantiating
@racket[array] with the desired element type is a syntax error:
@interaction[#:eval typed-eval
                    (list 1 2 3)
                    (array #[1 2 3])
                    ((inst list Real) 1 2 3)
                    ((inst array Real) #[1 2 3])]
There are two easy ways to annotate the element type:
@interaction[#:eval typed-eval
                    (array #[1 2 3] : Real)
                    (ann (array #[1 2 3]) (Array Real))]
Annotating should rarely be necessary because the @racket[Array] type is covariant.

Normally, the datums within literal vectors are implicitly quoted. However, when used within the
@racket[array] form, the datums must be explicitly quoted.
@interaction[#:eval typed-eval
                    #(this is okay)
                    (array #[not okay])
                    (array #['this 'is 'okay])
                    (array #['#(an) '#(array) '#(of) '#(vectors)])]

Arrays returned by @racket[array] are @tech{strict}.
Another way to create immutable, strict arrays from literal data is to use @racket[list->array].
}

@defform/subs[(mutable-array #[#[...] ...] maybe-type-ann)
              [(maybe-type-ann (code:line) (code:line : type))]]{
Creates a @racket[Mutable-Array] from nested rows of expressions.

The semantics are almost identical to @racket[array]'s, except the result is mutable:
@interaction[#:eval typed-eval
                    (define arr (mutable-array #[0 1 2 3]))
                    arr
                    (array-set! arr #(0) 10)
                    arr]
Because mutable arrays are invariant, this form additionally accepts a type annotation for
the array's elements:
@interaction[#:eval typed-eval
                    (define arr (mutable-array #[0 1 2 3] : Real))
                    arr
                    (array-set! arr #(0) 10.0)
                    arr]

Another way to create mutable arrays from literal data is to use @racket[vector->array].
}

@defproc[(make-array [ds In-Indexes] [value A]) (Array A)]{
Returns an array with @tech{shape} @racket[ds], with every element's value as @racket[value].
Analogous to @racket[make-vector].
@examples[#:eval typed-eval
                 (make-array #() 5)
                 (make-array #(1 2) 'sym)
                 (make-array #(4 0 2) "Invisible")]
The arrays returned by @racket[make-array] do not allocate storage for their elements and
are @tech{strict}.
}

@defproc[(build-array [ds In-Indexes] [proc (Indexes -> A)]) (Array A)]{
Returns an array with @tech{shape} @racket[ds] and @tech{procedure} @racket[proc].
Analogous to @racket[build-vector].
}

@defproc[(array->mutable-array [arr (Array A)]) (Mutable-Array A)]{
Returns a mutable array with the same elements as @racket[arr]. The result is a copy of
@racket[arr], even when @racket[arr] is mutable.
}

@defproc[(mutable-array-copy [arr (Mutable-Array A)]) (Mutable-Array A)]{
Like @racket[(array->mutable-array arr)], but restricted to mutable arrays. It is also faster.
}

@defproc[(indexes-array [ds In-Indexes]) (Array Indexes)]{
Returns an array with @tech{shape} @racket[ds], with each element set to its position in the
array.
@examples[#:eval typed-eval
                 (indexes-array #())
                 (indexes-array #(4))
                 (indexes-array #(2 3))
                 (indexes-array #(4 0 2))]
The resulting array does not allocate storage for its return value's elements, and is @tech{strict}.
(It is essentially the identity function for the domain @racket[ds].)
}

@defproc[(index-array [ds In-Indexes]) (Array Index)]{
Returns an array with @tech{shape} @racket[ds], with each element set to its row-major index in
the array.
@examples[#:eval typed-eval
                 (index-array #(2 3))
                 (array-flatten (index-array #(2 3)))]
As with @racket[indexes-array], the result does not allocate storage for its elements, and is
@tech{strict}.
}

@defproc[(axis-index-array [ds In-Indexes] [axis Integer]) (Array Index)]{
Returns an array with @tech{shape} @racket[ds], with each element set to its position in axis
@racket[axis]. The axis number @racket[axis] must be nonnegative and less than the number of axes
(the length of @racket[ds]).
@examples[#:eval typed-eval
                 (axis-index-array #(3 3) 0)
                 (axis-index-array #(3 3) 1)
                 (axis-index-array #() 0)]
As with @racket[indexes-array], the result does not allocate storage for its elements, and is
@tech{strict}.
}

@defproc[(diagonal-array [dims Integer] [axes-length Integer] [on-value A] [off-value A])
         (Array A)]{
Returns an array with @racket[dims] axes, each with length @racket[axes-length].
(For example, the returned array for @racket[dims = 2] is square.)
The elements on the diagonal (i.e. at indexes of the form @racket[(vector j j ...)] for
@racket[j < axes-length]) have the value @racket[on-value]; the rest have @racket[off-value].
@examples[#:eval typed-eval
                 (diagonal-array 2 7 1 0)]
As with @racket[indexes-array], the result does not allocate storage for its elements, and is
@tech{strict}.
}


@;{==================================================================================================}


@section[#:tag "array:convert"]{Conversion}

@defform[(Listof* A)]{
Equivalent to @racket[(U A (Listof A) (Listof (Listof A)) ...)] if infinite unions were allowed.
This is used as an argument type to @racket[list*->array] and as the return type of
@racket[array->list*].
}

@defform[(Vectorof* A)]{
Like @racket[(Listof* A)], but for vectors. See @racket[vector*->array] and @racket[array->vector*].
}

@deftogether[(@defproc*[([(list->array [lst (Listof A)]) (Array A)]
                         [(list->array [ds In-Indexes] [lst (Listof A)]) (Array A)])]
              @defproc[(array->list [arr (Array A)]) (Listof A)])]{
Convert lists to immutable arrays and back.

The two-argument variant of @racket[list->array] assumes the elements in @racket[lst] are in
row-major order.

For @racket[array->list], if @racket[arr] has no axes or more than one axis, it is (conceptually)
flattened before being converted to a list.

@examples[#:eval typed-eval
                 (list->array '(1 2 3))
                 (list->array '((1 2 3) (4 5)))
                 (list->array #(2 2) '(1 2 3 4))
                 (array->list (array #[1 2 3]))
                 (array->list (array 10))
                 (array->list (array #[#[1 2 3] #[4 5 6]]))]
For conversion between nested lists and multidimensional arrays, see @racket[list*->array] and
@racket[array->list*].
For conversion from flat values to mutable arrays, see @racket[vector->array].

The arrays returned by @racket[list->array] are always @tech{strict}.
}

@deftogether[(@defproc*[([(vector->array [vec (Vectorof A)]) (Mutable-Array A)]
                         [(vector->array [ds In-Indexes] [vec (Vectorof A)]) (Mutable-Array A)])]
              @defproc[(array->vector [arr (Array A)]) (Vectorof A)])]{
Like @racket[list->array] and @racket[array->list], but for vectors.
@examples[#:eval typed-eval
                 (vector->array #(1 2 3))
                 (vector->array #((1 2 3) (4 5)))
                 (vector->array #(2 2) #(1 2 3 4))
                 (array->vector (array #[1 2 3]))
                 (array->vector (array 10))
                 (array->vector (array #[#[1 2 3] #[4 5 6]]))]
For conversion between nested vectors and multidimensional arrays, see @racket[vector*->array] and
@racket[array->vector*].
}

@defproc[(list*->array [lsts (Listof* A)] [pred? ((Listof* A) -> Any : A)]) (Array A)]{
Converts a nested list of elements of type @racket[A] to an array.
The predicate @racket[pred?] identifies elements of type @racket[A].
The shape of @racket[lsts] must be rectangular.

@examples[#:eval typed-eval
                 (list*->array 'singleton symbol?)
                 (list*->array '(0 1 2 3) byte?)
                 (list*->array (list (list (list 5) (list 2 3))
                                     (list (list 4.0) (list 1.4 0.2 9.3)))
                               (make-predicate (Listof Nonnegative-Real)))]

There is no well-typed Typed Racket function that behaves like @racket[list*->array] but does not
require @racket[pred?].
Without an element predicate, there is no way to prove to the type checker that
@racket[list*->array]'s implementation correctly distinguishes elements from rows.

The arrays returned by @racket[list*->array] are always @tech{strict}.
}

@defproc[(array->list* [arr (Array A)]) (Listof* A)]{
The inverse of @racket[list*->array].
}

@defproc[(vector*->array [vecs (Vectorof* A)] [pred? ((Vectorof* A) -> Any : A)]) (Mutable-Array A)]{
Like @racket[list*->array], but accepts nested vectors of elements.
@examples[#:eval typed-eval
                 (vector*->array 'singleton symbol?)
                 ((inst vector*->array Byte) #(0 1 2 3) byte?)]
As in the last example, Typed Racket often needs help inferring @racket[vector*->array]'s
type parameters.
}

@defproc[(array->vector* [arr (Array A)]) (Vectorof* A)]{
Like @racket[array->list*], but produces nested vectors of elements.
}

@defproc[(array-list->array [arrs (Listof (Array A))] [axis Integer 0]) (Array A)]{
Concatenates @racket[arrs] along axis @racket[axis] to form a new array.
If the arrays have different shapes, they are broadcast first.
The axis number @racket[axis] must be nonnegative and @italic{no greater than} the number of axes in
the highest dimensional array in @racket[arrs].
@examples[#:eval typed-eval
                 (array-list->array (list (array 0) (array 1) (array 2) (array 3)))
                 (array-list->array (list (array 0) (array 1) (array 2) (array 3)) 1)
                 (array-list->array (list (array #[0 1 2 3]) (array #['a 'b 'c 'd])))
                 (array-list->array (list (array #[0 1 2 3]) (array '!)))
                 (array-list->array (list (array #[0 1 2 3]) (array '!)) 1)
                 ]
This function is a left inverse of @racket[array->array-list]. (It cannot be a right inverse
because broadcasting cannot be undone.)

For a similar function that does not increase the dimension of the broadcast arrays, see
@racket[array-append*].
}

@defproc[(array->array-list [arr (Array A)] [axis Integer 0]) (Listof (Array A))]{
Turns one axis of @racket[arr] into a list of arrays. Each array in the result has the same shape.
The axis number @racket[axis] must be nonnegative and less than the number of @racket[arr]'s axes.
@examples[#:eval typed-eval
                 (array->array-list (array #[0 1 2 3]))
                 (array->array-list (array #[#[1 2] #[10 20]]))
                 (array->array-list (array #[#[1 2] #[10 20]]) 1)
                 (array->array-list (array 10))]
}

@subsection[#:tag "array:print"]{Printing}

@defparam[array-custom-printer
          print-array
          (All (A) ((Array A)
                    Symbol
                    Output-Port
                    (U Boolean 0 1) -> Any))]{
A parameter whose value is used to print subtypes of @racket[Array].
}

@defproc[(print-array [arr (Array A)]
                      [name Symbol]
                      [port Output-Port]
                      [mode (U Boolean 0 1)])
         Any]{
Prints an array using @racket[array] syntax, using @racket[name] instead of @racket['array]
as the head form. This function is set as the value of @racket[array-custom-printer] when
@racketmodname[math/array] is first required.

Well-behaved @racket[Array] subtypes do not call this function directly to print themselves.
They call the current @racket[array-custom-printer]:
@interaction[#:eval typed-eval
                    (eval:alts
                     ((array-custom-printer)
                      (array #[0 1 2 3])
                      'my-cool-array
                      (current-output-port)
                      #t)
                     (eval:result "" "(my-cool-array #[0 1 2 3])"))]
See @racket[prop:custom-write] for the meaning of the @racket[port] and @racket[mode] arguments.
}


@;{==================================================================================================}


@section[#:tag "array:sequences"]{Comprehensions and Sequences}

Sometimes sequential processing is unavoidable, so @racket[math/array] provides loops and sequences.

@deftogether[(@defform[(for/array: maybe-shape maybe-fill (for:-clause ...) maybe-type-ann
                         body ...+)]
              @defform/subs[(for*/array: maybe-shape maybe-fill (for:-clause ...) maybe-type-ann
                              body ...+)
                            ([maybe-shape (code:line) (code:line #:shape ds)]
                             [maybe-fill (code:line) (code:line #:fill fill)]
                             [maybe-type-ann (code:line) (code:line : body-type)])
                            #:contracts ([ds In-Indexes]
                                         [fill body-type])])]{
Creates arrays by generating elements in a @racket[for]-loop or @racket[for*]-loop.
Unlike other Typed Racket loop macros, these accept a @italic{body annotation}, which declares
the type of elements. They do not accept an annotation for the entire type of the result.
@examples[#:eval typed-eval
                 (for/array: ([x  (in-range 3)] [y  (in-range 3)]) : Integer
                   (+ x y))
                 (for*/array: ([x  (in-range 3)] [y  (in-range 3)]) : Integer
                   (+ x y))]
The shape of the result is independent of the loop clauses: note that the last example
does not have shape @racket[#(3 3)], but shape @racket[#(9)]. To control the shape, use the
@racket[#:shape] keyword:
@interaction[#:eval typed-eval
                    (for*/array: #:shape #(3 3) ([x  (in-range 3)]
                                                 [y  (in-range 3)]) : Integer
                      (+ x y))]

If the loop does not generate enough elements, the rest are filled with the @italic{first}
generated value:
@interaction[#:eval typed-eval
                    (for*/array: #:shape #(4) ([x  (in-range 1 3)]) x)]
To change this behavior, use the @racket[#:fill] keyword:
@interaction[#:eval typed-eval
                    (for*/array: #:shape #(4) #:fill -1 ([x  (in-range 1 3)]) x)]
In the last two examples, the array's type is @racket[(Mutable-Array Any)] because
a body annotation was not given.
}

@deftogether[(@defform[(for/array maybe-shape maybe-fill (for-clause ...)
                         body ...+)]
              @defform[(for*/array maybe-shape maybe-fill (for-clause ...)
                         body ...+)])]{
Untyped versions of the loop macros.
}

@defproc[(in-array [arr (Array A)]) (Sequenceof A)]{
Returns a sequence of @racket[arr]'s elements in row-major order.
@examples[#:eval typed-eval
                 (define arr (array #[#[1 2] #[10 20]]))
                 (for/list: : (Listof Integer) ([x  (in-array arr)]) x)]
}

@defproc[(in-array-axis [arr (Array A)] [axis Integer 0]) (Sequenceof (Array A))]{
Like @racket[array->array-list], but returns a sequence.
@examples[#:eval typed-eval
                 (define arr (array #[#[1 2] #[10 20]]))
                 (sequence->list (in-array-axis arr))
                 (sequence->list (in-array-axis arr 1))]
}

@defproc[(in-array-indexes [ds In-Indexes]) (Sequenceof Indexes)]{
Returns a sequence of indexes for shape @racket[ds], in row-major order.
@examples[#:eval typed-eval
                 (for/array: #:shape #(3 3) ([js  (in-array-indexes #(3 3))]) : Indexes
                   js)
                 (for*/array: #:shape #(3 3) ([j0  (in-range 3)]
                                              [j1  (in-range 3)]) : In-Indexes
                   (vector j0 j1))
                 (indexes-array #(3 3))]
}


@;{==================================================================================================}


@section[#:tag "array:pointwise"]{Pointwise Operations}

Most of the operations documented in this section are simple macros that apply @racket[array-map]
to a function and their array arguments.

@defproc*[([(array-map [f (-> R)]) (Array R)]
           [(array-map [f (A -> R)] [arr0 (Array A)]) (Array R)]
           [(array-map [f (A B Ts ... -> R)] [arr0 (Array A)] [arr1 (Array B)] [arrs (Array Ts)] ...)
            (Array R)])]{
Composes @racket[f] with the given arrays' @tech{procedures}. When the arrays' shapes do not match,
they are @tech{broadcast} to the same shape first. If broadcasting fails, @racket[array-map] raises
an error.

@examples[#:eval typed-eval
                 (array-map (λ: ([x : String]) (string-append x "!"))
                            (array #[#["Hello" "I"] #["Am" "Shouting"]]))
                 (array-map string-append
                            (array #[#["Hello" "I"] #["Am" "Shouting"]])
                            (array "!"))
                 (array-map + (index-array #(3 3 3)) (array 2))
                 (array-map + (index-array #(2 2)) (index-array #(3 3)))]

Typed Racket can often derive fairly precise element types for the resulting array:
@interaction[#:eval typed-eval
                    (array-map * (array #[-4.3 -1.2 -0.2]) (array -2.81))]
How precise the result type is depends on the type of @racket[f]. Preserving precise result types
for lifted arithmetic operators is the main reason most pointwise operations are macro wrappers for
@racket[array-map].

Unlike @racket[map], @racket[array-map] can map a zero-argument function:
@interaction[#:eval typed-eval
                    (array-map (λ () "Whoa, Nelly!"))]
If the resulting zero-dimensional array is used in a pointwise operation with other arrays,
it will be broadcast to their shape:
@interaction[#:eval typed-eval
                    (array-map + (array #[1 2 3]) (array-map (λ () -10)))]

When explicitly instantiating @racket[array-map]'s types using @racket[inst], instantiate
@racket[R] (the return type's element type) first, then the arguments' element types in order.
}

@defform[(inline-array-map f arrs ...)]{
Like @racket[array-map], but possibly faster. Inlining a map operation can allow Typed Racket's
optimizer to replace @racket[f] with something unchecked and type-specific (for example,
replace @racket[*] with @racket[unsafe-fl*]), at the expense of code size.
}

@deftogether[(@defform[(array+ arrs ...)]
              @defform[(array* arrs ...)]
              @defform[(array- arr0 arrs ...)]
              @defform[(array/ arr0 arrs ...)]
              @defform[(array-min arr0 arrs ...)]
              @defform[(array-max arr0 arrs ...)])]{
Equivalent to mapping arithmetic operators over arrays. Note that because @racket[(array-map f)]
returns sensible answers, so do @racket[(array+)] and @racket[(array*)].
@examples[#:eval typed-eval
                 (array+ (array #[#[0.0 1.0] #[2.0 3.0]]) (array 200))
                 (array+)
                 (array*)
                 (array/ (array #[2 1/2]))]
}

@defform[(array-scale arr x)]{
Equivalent to @racket[(array* arr (array x))], but faster.
}

@deftogether[(@defform[(array-abs arr)]
              @defform[(array-sqr arr)]
              @defform[(array-sqrt arr)]
              @defform[(array-conjugate arr)])]{
Equivalent to @racket[(array-map f arr)], where @racket[f] is respectively
@racket[abs],
@racket[sqr],
@racket[sqrt],
or @racket[conjugate].
}

@deftogether[(@defform[(array-real-part arr)]
              @defform[(array-imag-part arr)]
              @defform[(array-make-rectangular arr0 arr1)]
              @defform[(array-magnitude arr)]
              @defform[(array-angle arr)]
              @defform[(array-make-polar arr0 arr1)])]{
Conversions to and from complex numbers, lifted to operate on arrays.
}

@deftogether[(@defform[(array< arr0 arr1 arrs ...)]
              @defform[(array<= arr0 arr1 arrs ...)]
              @defform[(array> arr0 arr1 arrs ...)]
              @defform[(array>= arr0 arr1 arrs ...)]
              @defform[(array= arr0 arr1 arrs ...)])]{
Equivalent to @racket[(array-map f arr0 arr1 arrs ...)], where @racket[f] is respectively
@racket[<], @racket[<=], @racket[>], @racket[>=], or @racket[=].
}

@deftogether[(@defform[(array-not arr)]
              @defform[(array-and arr ...)]
              @defform[(array-or arr ...)]
              @defform[(array-if cond-arr true-arr false-err)])]{
Boolean operators lifted to operate on arrays.

When given @tech{nonstrict} arrays, the short-cutting behavior of @racket[array-and],
@racket[array-or] and @racket[array-if] can keep their elements from being referred to (and thus
computed).
However, these macros cannot be used to distinguish base and inductive cases in a recursive function,
because the array arguments are eagerly evaluated. For example, this function never returns, even
when @racket[array-strictness] is @racket[#f]:
@racketblock[(: array-factorial ((Array Integer) -> (Array Integer)))
             (define (array-factorial arr)
               (array-if (array<= arr (array 0))
                         (array 1)
                         (array* arr (array-factorial (array- arr (array 1))))))]
}

@subsection{Broadcasting}

@defparam[array-broadcasting broadcasting (U Boolean 'permissive)]{
Determines the rules used when broadcasting arrays for pointwise operations. See
@secref{array:broadcasting:control}.
}

@defproc[(array-shape-broadcast [dss (Listof Indexes)]
                                [broadcasting (U Boolean 'permissive) (array-broadcasting)])
         Indexes]{
Determines the shape of the resulting array if some number of arrays with shapes @racket[dss]
were broadcast for a pointwise operation using the given broadcasting rules. If broadcasting
fails, @racket[array-shape-broadcast] raises an error.
@examples[#:eval typed-eval
                 (array-shape-broadcast '())
                 (array-shape-broadcast (list (vector) ((inst vector Index) 10)))
                 (array-shape-broadcast (list ((inst vector Index) 2)
                                              ((inst vector Index) 10)))
                 (array-shape-broadcast (list ((inst vector Index) 2)
                                              ((inst vector Index) 10))
                                        'permissive)]
}

@defproc[(array-broadcast [arr (Array A)] [ds Indexes]) (Array A)]{
Returns an array with shape @racket[ds] made by inserting new axes and repeating rows.
This is used for both @racket[(array-broadcasting #t)] and @racket[(array-broadcasting 'permissive)].
@examples[#:eval typed-eval
                 (array-broadcast (array 10) ((inst vector Index) 10))
                 (array-broadcast (array #[0 1]) #())
                 (array-broadcast (array #[0 1]) ((inst vector Index) 5))]

When @racket[array-strictness] is @racket[#f], @racket[array-broadcast] always returns a
@tech{nonstrict} array.

When @racket[array-strictness] is @racket[#t], @racket[array-broadcast] returns a strict array
when @racket[arr] is nonstrict and the result has more elements than @racket[arr].
}


@;{==================================================================================================}


@section[#:tag "array:indexing"]{Indexing and Slicing}

@defproc[(array-ref [arr (Array A)] [js In-Indexes]) A]{
Returns the element of @racket[arr] at position @racket[js]. If any index in @racket[js] is
negative or not less than its corresponding axis length, @racket[array-ref] raises an error.
}

@defproc[(array-set! [arr (Settable-Array A)] [js In-Indexes] [value A]) Void]{
Sets the element of @racket[arr] at position @racket[js] to @racket[value].
If any index in @racket[js] is negative or not less than its corresponding axis length,
@racket[array-set!] raises an error.
}

@defproc[(array-indexes-ref [arr (Array A)] [idxs (Array In-Indexes)]) (Array A)]{
High-level explanation: Returns multiple elements from @racket[arr] in a new array.

Lower-level explanation: Returns an array with same shape as @racket[idxs], whose elements are
@racket[array-ref]'d from @racket[arr] using the indexes in @racket[idxs]. 
@examples[#:eval typed-eval
                 (define arr (array #[#[1 2] #[10 20]]))
                 (define idxs (array #['#(0 0) '#(1 1)]))
                 (array-indexes-ref arr idxs)]

Implementation-level explanation: @racket[(array-indexes-ref arr idxs)] is equivalent to
@interaction[#:eval typed-eval
                    (build-array (array-shape idxs)
                                 (λ: ([js : Indexes])
                                   (array-ref arr (array-ref idxs js))))]
but faster.
}

@defproc[(array-indexes-set! [arr (Settable-Array A)] [idxs (Array In-Indexes)] [vals (Array A)])
         Void]{
Indexes @racket[arr] in the same way that @racket[array-indexes-ref] does, but mutates elements.
If @racket[idxs] and @racket[vals] do not have the same shape, they are @tech{broadcast} first.
@examples[#:eval typed-eval
                 (define arr (mutable-array #[#[1 2] #[10 20]]))
                 (define idxs (array #['#(0 0) '#(1 1)]))
                 (array-indexes-set! arr idxs (array -1))
                 arr]
When two indexes in @racket[idxs] are the same, the element at that index is mutated more
than once in some unspecified order.
}

@defproc[(array-slice-ref [arr (Array A)] [specs (Listof Slice-Spec)]) (Array A)]{
Returns a transformation of @racket[arr] according to the list of slice specifications
@racket[specs]. See @secref{array:slicing} for a discussion and examples.
}

@defproc[(array-slice-set! [arr (Settable-Array A)] [specs (Listof Slice-Spec)] [vals (Array A)])
         Void]{
Like @racket[array-indexes-set!], but for slice specifications. Equivalent to
@racketblock[(let ([idxs  (array-slice-ref (indexes-array (array-shape arr)) specs)])
               (array-indexes-set! arr idxs vals))]
@examples[#:eval typed-eval
                 (define arr (array->mutable-array (axis-index-array #(5 5) 1)))
                 (array-slice-set! arr (list (:: 1 #f 2) (::)) (array 1))
                 arr
                 (array-slice-set!
                  arr (list (::) (:: 1 #f 2))
                  (array-scale (array-slice-ref arr (list (::) (:: 1 #f 2))) -1))
                 arr]
When a slice specification refers to an element in @racket[arr] more than once, the element is
mutated more than once in some unspecified order.
}

@defidform[Slice-Spec]{
The type of a slice specification. Currently defined as
@racketblock[(U (Sequenceof Integer) Slice Slice-Dots Integer Slice-New-Axis)]

A @racket[(Sequenceof Integer)] slice specification causes @racket[array-slice-ref] to pick rows
from an axis.
An @racket[Integer] slice specification causes @racket[array-slice-ref] to remove an axis by
replacing it with one of its rows.

See @secref{array:slicing} for an extended example.
}

@deftogether[(@defidform[Slice]
              @defproc*[([(:: [end (U #f Integer) #f]) Slice]
                         [(:: [start (U #f Integer)] [end (U #f Integer)] [step Integer 1])
                          Slice])]
              @defproc[(slice? [v Any]) Boolean]
              @defproc[(slice-start [s Slice]) (U #f Fixnum)]
              @defproc[(slice-end [s Slice]) (U #f Fixnum)]
              @defproc[(slice-step [s Slice]) Fixnum])]{
The type of @racket[in-range]-like slice specifications, its constructor, predicate, and accessors.

@racket[array-slice-ref] interprets a @racket[Slice] like an @racket[in-range] sequence object.
When @racket[start] or @racket[end] is @racket[#f], it is interpreted as an axis-length-dependent
endpoint.
}

@defproc[(slice->range-values [s Slice] [dk Index]) (Values Fixnum Fixnum Fixnum)]{
Given a slice @racket[s] and an axis length @racket[dk], returns the arguments to @racket[in-range]
that would produce an equivalent slice specification.

This is used internally by @racket[array-slice-ref] to interpret a @racket[Slice] object as a
sequence of indexes.
}

@deftogether[(@defidform[Slice-Dots]
              @defthing[::... Slice-Dots]
              @defproc[(slice-dots? [v Any]) Boolean])]{
The type of greedy, multiple-axis-preserving slice specifications, its singleton value, and predicate.
}

@deftogether[(@defidform[Slice-New-Axis]
              @defproc[(::new [dk Integer 1]) Slice-New-Axis]
              @defproc[(slice-new-axis? [v Any]) Boolean]
              @defproc[(slice-new-axis-length [s Slice-New-Axis]) Index])]{
The type of slice specifications that indicate inserting a new axis, its constructor, predicate, and
accessor. The axis length @racket[dk] must be nonnegative.
}


@;{==================================================================================================}


@section[#:tag "array:transform"]{Transformations}

@defproc[(array-transform [arr (Array A)] [ds In-Indexes] [proc (Indexes -> In-Indexes)])
         (Array A)]{
Returns an array with shape @racket[ds] and elements taken from @racket[arr], by composing
@racket[arr]'s procedure with @racket[proc].

Possibly the most useless, but simplest example of @racket[proc] disregards its input
indexes and returns constant indexes:
@interaction[#:eval typed-eval
                    (define arr (array #[#[0 1] #[2 'three]]))
                    (array-transform arr #(3 3) (λ: ([js : Indexes]) #(1 1)))]
Doubling an array in every dimension by duplicating elements:
@interaction[#:eval typed-eval
                    (define arr (index-array #(3 3)))
                    arr
                    (array-transform
                     arr
                     (vector-map (λ: ([d : Index]) (* d 2)) (array-shape arr))
                     (λ: ([js : Indexes])
                       (vector-map (λ: ([j : Index]) (quotient j 2)) js)))]
When @racket[array-strictness] is @racket[#f], the above result takes little more space than the
original array.

Almost all array transformations, including @secref{array:slicing}, are implemented using
@racket[array-transform] or its unsafe counterpart.
}

@defproc[(array-append* [arrs (Listof (Array A))] [k Integer 0]) (Array A)]{
Appends the arrays in @racket[arrs] along axis @racket[k]. If the arrays' shapes are not
the same, they are @tech{broadcast} first.
@examples[#:eval typed-eval
                 (define arr (array #[#[0 1] #[2 3]]))
                 (define brr (array #[#['a 'b] #['c 'd]]))
                 (array-append* (list arr brr))
                 (array-append* (list arr brr) 1)
                 (array-append* (list arr (array 'x)))]
For an append-like operation that increases the dimension of the broadcast arrays, see
@racket[array-list->array].
}

@defproc[(array-axis-insert [arr (Array A)] [k Integer] [dk Integer 1]) (Array A)]{
Inserts an axis of length @racket[dk] before axis number @racket[k], which must be no greater
than the dimension of @racket[arr].
@examples[#:eval typed-eval
                 (define arr (array #[#[0 1] #[2 3]]))
                 (array-axis-insert arr 0)
                 (array-axis-insert arr 1)
                 (array-axis-insert arr 2)
                 (array-axis-insert arr 1 2)]
}

@defproc[(array-axis-ref [arr (Array A)] [k Integer] [jk Integer]) (Array A)]{
Removes an axis from @racket[arr] by keeping only row @racket[jk] in axis @racket[k], which
must be less than the dimension of @racket[arr].
@examples[#:eval typed-eval
                 (define arr (array #[#[0 1] #[2 3]]))
                 (array-axis-ref arr 0 0)
                 (array-axis-ref arr 0 1)
                 (array-axis-ref arr 1 0)]
}

@defproc[(array-axis-swap [arr (Array A)] [k0 Integer] [k1 Integer]) (Array A)]{
Returns an array like @racket[arr], but with axes @racket[k0] and @racket[k1] swapped.
In two dimensions, this is called a transpose.
@examples[#:eval typed-eval
                 (array-axis-swap (array #[#[0 1] #[2 3]]) 0 1)
                 (define arr (indexes-array #(2 2 2)))
                 arr
                 (array-axis-swap arr 0 1)
                 (array-axis-swap arr 1 2)]
}

@defproc[(array-axis-permute [arr (Array A)] [perm (Listof Integer)]) (Array A)]{
Returns an array like @racket[arr], but with axes permuted according to @racket[perm].

The list @racket[perm] represents a mapping from source axis numbers to destination
axis numbers: the source is the list position, the destination is the list element.
For example, the permutation @racket['(0 1 2)] is the identity permutation for three-dimensional
arrays, @racket['(1 0)] swaps axes @racket[0] and @racket[1], and @racket['(3 1 2 0)] swaps
axes @racket[0] and @racket[3].

The permutation must contain each integer from @racket[0] to @racket[(- (array-dims arr) 1)]
exactly once.

@examples[#:eval typed-eval
                 (array-axis-swap (array #[#[0 1] #[2 3]]) 0 1)
                 (array-axis-permute (array #[#[0 1] #[2 3]]) '(1 0))]
}

@defproc[(array-reshape [arr (Array A)] [ds In-Indexes]) (Array A)]{
Returns an array with elements in the same row-major order as @racket[arr], but with
shape @racket[ds]. The product of the indexes in @racket[ds] must be @racket[(array-size arr)].
@examples[#:eval typed-eval
                 (define arr (indexes-array #(2 3)))
                 arr
                 (array-reshape arr #(3 2))
                 (array-reshape (index-array #(3 3)) #(9))]
}

@defproc[(array-flatten [arr (Array A)]) (Array A)]{
Returns an array with shape @racket[(vector (array-size arr))], with the elements of
@racket[arr] in row-major order.
@examples[#:eval typed-eval
                 (array-flatten (array 10))
                 (array-flatten (array #[#[0 1] #[2 3]]))]
}


@;{==================================================================================================}


@section[#:tag "array:fold"]{Folds, Reductions and Expansions}

@subsection{Axis Folds}

@defproc*[([(array-axis-fold [arr (Array A)] [k Integer] [f (A A -> A)]) (Array A)]
           [(array-axis-fold [arr (Array A)] [k Integer] [f (A B -> B)] [init B]) (Array B)])]{
Folds a binary function @racket[f] over axis @racket[k] of @racket[arr]. The result has the
shape of @racket[arr] but with axis @racket[k] removed.

The three-argument variant uses the first value of each row in axis @racket[k] as @racket[init].
It therefore requires axis @racket[k] to have positive length.

@examples[#:eval typed-eval
                 (define arr (index-array #(3 4)))
                 arr
                 (array-axis-fold arr 0 +)
                 (array-axis-fold arr 1 (inst cons Index (Listof Index)) empty)]
Notice that the second example returns an array of reversed lists.
This is therefore a left fold; see @racket[foldl].

If you need control over the order of evaluation in axis @racket[k]'s rows, see
@racket[array-axis-reduce].
}

@deftogether[(@defform*[((array-axis-sum arr k)
                         (array-axis-sum arr k init))]
              @defform*[((array-axis-prod arr k)
                         (array-axis-prod arr k init))
                        #:contracts ([arr   (Array Number)]
                                     [k     Integer]
                                     [init  Number])])]{
}
@deftogether[(@defform*[((array-axis-min arr k)
                         (array-axis-min arr k init))]
              @defform*[((array-axis-max arr k)
                         (array-axis-max arr k init))
                        #:contracts ([arr   (Array Real)]
                                     [k     Integer]
                                     [init  Real])])]{
Some standard per-axis folds, defined in terms of @racket[array-axis-fold]. The two-argument
variants require axis @racket[k] to have positive length.
@examples[#:eval typed-eval
                 (define arr (index-array #(3 4)))
                 arr
                 (array-axis-fold arr 1 +)
                 (array-axis-sum arr 1)
                 (array-axis-sum arr 0 0.0)]
}

@defproc[(array-axis-count [arr (Array A)] [k Integer] [pred? (A -> Any)]) (Array Index)]{
Counts the elements @racket[x] in rows of axis @racket[k] for which @racket[(pred? x)] is true.
@examples[#:eval typed-eval
                 (define arr (index-array #(3 3)))
                 arr
                 (array-axis-count arr 1 odd?)]
}

@deftogether[(@defproc[(array-axis-and [arr (Array A)] [k Integer]) (Array (U A Boolean))]
              @defproc[(array-axis-or [arr (Array A)] [k Integer]) (Array (U A #f))])]{
Apply @racket[and] or @racket[or] to each row in axis @racket[k] of array @racket[arr].
Evaluation is short-cut as with the @racket[and] and @racket[or] macros, which is only observable
if @racket[arr] is @tech{nonstrict}.

In the following example, computing the second array element sets @racket[second?] to @racket[#t]:
@interaction[#:eval typed-eval
                    (define second? (ann #f Boolean))
                    (define arr
                      (parameterize ([array-strictness #f])
                        (build-array #(2) (λ: ([js : Indexes])
                                            (cond [(zero? (vector-ref js 0))  #f]
                                                  [else  (set! second? #t)
                                                         #t])))))]
Printing @racket[arr] causes @racket[(set! second? #t)] to be evaluated, but applying
@racket[array-axis-and] does not:
@interaction[#:eval typed-eval
                    (array-axis-and arr 0)
                    second?]
However, if @racket[arr] were strict, @racket[(set! second? #t)] would be evaluated when @racket[arr]
was created.
}

@subsection{Whole-Array Folds}

@defproc[(array-fold [arr (Array A)] [g ((Array A) Index -> (Array A))]) (Array A)]{
Folds @racket[g] over @italic{each axis} of @racket[arr], in reverse order. The arguments
to @racket[g] are an array (initially @racket[arr]) and the current axis.
It should return an array with one fewer dimension than the array given, but does not have to.
@examples[#:eval typed-eval
                 (define arr (index-array #(3 4)))
                 arr
                 (array-fold arr (λ: ([arr : (Array Integer)] [k : Index])
                                   (array-axis-sum arr k)))
                 (apply + (array->list arr))
                 (array-ref (array-fold arr (inst array->list-array (Listof* Integer)))
                            #())]
}

@defproc*[([(array-all-fold [arr (Array A)] [f (A A -> A)]) A]
           [(array-all-fold [arr (Array A)] [f (A A -> A)] [init A]) A])]{
Folds @racket[f] over every element of @racket[arr] by folding @racket[f] over each axis in
reverse order. The two-argument variant is equivalent to
@racketblock[(array-ref (array-fold arr (λ: ([arr : (Array A)] [k : Index])
                                          (array-axis-fold arr k f)))
                        #())]
and the three-argument variant is similar. The two-argument variant requires every axis to have
positive length.
@examples[#:eval typed-eval
                 (define arr (index-array #(3 4)))
                 arr
                 (array-all-fold arr +)
                 (array-all-fold (array #[]) + 0.0)]
Because @racket[f] is folded over the last axis first, it receives @racket[arr]'s elements (as its
first argument) in row-major order.
}

@deftogether[(@defform*[((array-all-sum arr)
                         (array-all-sum arr init))]
              @defform*[((array-all-prod arr)
                         (array-all-prod arr init))
                        #:contracts ([arr   (Array Number)]
                                     [init  Number])])]{
}
@deftogether[(@defform*[((array-all-min arr)
                         (array-all-min arr init))]
              @defform*[((array-all-max arr)
                         (array-all-max arr init))
                        #:contracts ([arr   (Array Real)]
                                     [init  Real])])]{
Some standard whole-array folds, defined in terms of @racket[array-all-fold]. The one-argument
variants require each axis in @racket[arr] to have positive length.
@examples[#:eval typed-eval
                 (define arr (index-array #(3 4)))
                 arr
                 (array-all-fold arr +)
                 (array-all-sum arr)
                 (array-all-sum arr 0.0)]
}

@deftogether[(@defproc[(array-all-and [arr (Array A)]) (U A Boolean)]
              @defproc[(array-all-or [arr (Array A)]) (U A #f)])]{
Apply @racket[and] or @racket[or] to @racket[arr]'s elements using short-cut evaluation in row-major
order.

@examples[#:eval typed-eval
                 (define arr (index-array #(3 3)))
                 (array-all-and (array= arr arr))
                 (define brr (array+ arr (array 1)))
                 (array-all-and (array= arr brr))
                 (array-all-or (array= arr (array 0)))]

@racket[(array-all-and arr)] is defined as
@racketblock[(parameterize ([array-strictness #f])
               (array-ref (array-fold arr array-axis-and) #()))]
and @racket[array-all-or] is defined similarly, using @racket[array-axis-or].
}

@defform[(array-count pred? arrs ...)
         #:contracts ([arrs   (Array Ts)]
                      [pred?  (Ts ... -> Any)])]{
When given one array @racket[arr], returns the number of elements @racket[x] in @racket[arr] for
which @racket[(pred? x)] is true.
When given multiple arrays, @racket[array-count] does the same with the corresponding elements from
any number of arrays.
If the arrays' shapes are not the same, they are @tech{broadcast} first.

@examples[#:eval typed-eval
                 (array-count zero? (array #[#[0 1 0 2] #[0 3 -1 4]]))
                 (array-count equal?
                              (array #[#[0 1] #[2 3] #[0 1] #[2 3]])
                              (array #[0 1]))]

@racket[(array-count pred? arrs ...)] is like
@racketblock[(array-all-sum (array-map (λ (x ...) (if (pred? x ...) 1 0)) arrs ...)
                            0)]
but does not create intermediate (strict) arrays, and always returns an @racket[Index].
}

@deftogether[(@defform[(array-andmap pred? arrs ...)]
              @defform[(array-ormap pred? arrs ...)
                       #:contracts ([arrs   (Array Ts)]
                                    [pred?  (Ts ... -> Any)])])]{
Like @racket[andmap] and @racket[ormap], but for arrays.
Evaluation is short-cut, in row-major order.
If the arrays' shapes are not the same, they are @tech{broadcast} first.

Determining whether each row is equal to @racket[(array #[0 1])]:
@interaction[#:eval typed-eval
                    (array-andmap equal?
                                  (array #[#[0 1] #[0 1] #[0 1] #[0 1]])
                                  (array #[0 1]))]
Determining whether any row has @racket[0] as its first element or @racket[1] as its second:
@interaction[#:eval typed-eval
                    (array-ormap equal?
                                 (array #[#[0 2] #[2 3] #[1 1] #[2 3]])
                                 (array #[0 1]))]
Determining whether any row is equal to @racket[(array #[0 1])]:
@interaction[#:eval typed-eval
                    (array-ormap equal?
                                 (array->list-array (array #[#[0 2] #[2 3] #[1 1] #[2 3]]))
                                 (array->list-array (array #[0 1])))]

@racket[(array-andmap pred? arrs ...)] is defined as
@racketblock[(parameterize ([array-strictness #f])
               (array-all-and (array-map pred? arrs ...)))]
and @racket[array-ormap] is defined similarly, using @racket[array-all-or].
}

@subsection{General Reductions and Expansions}

@defproc[(array-axis-reduce [arr (Array A)] [k Integer] [h (Index (Integer -> A) -> B)]) (Array B)]{
Like @racket[array-axis-fold], but allows evaluation control (such as short-cutting @racket[and] and
@racket[or]) by moving the loop into @racket[h]. The result has the shape of @racket[arr], but with
axis @racket[k] removed.

The arguments to @racket[h] are the length of axis @racket[k] and a procedure that retrieves
elements from that axis's rows by their indexes in axis @racket[k]. It should return the elements
of the resulting array.

For example, summing the squares of the rows in axis @racket[1]:
@interaction[#:eval typed-eval
                    (define arr (index-array #(3 3)))
                    arr
                    (array-axis-reduce
                     arr 1
                     (λ: ([dk : Index] [proc : (Integer -> Real)])
                       (for/fold: ([s : Real 0]) ([jk  (in-range dk)])
                         (+ s (sqr (proc jk))))))
                    (array-axis-sum (array-map sqr arr) 1)]
Transforming an axis into a list using @racket[array-axis-fold] and @racket[array-axis-reduce]:
@interaction[#:eval typed-eval
                    (array-map (inst reverse Index)
                               (array-axis-fold arr 1
                                                (inst cons Index (Listof Index))
                                                empty))
                    (array-axis-reduce arr 1 (inst build-list Index))]
The latter is essentially the definition of @racket[array->list-array].

Every fold, including @racket[array-axis-fold], is ultimately defined using
@racket[array-axis-reduce] or its unsafe counterpart.
}

@defproc[(array-axis-expand [arr (Array A)] [k Integer] [dk Integer] [g (A Index -> B)]) (Array B)]{
Inserts a new axis number @racket[k] of length @racket[dk], using @racket[g] to generate values;
@racket[k] must be @italic{no greater than} the dimension of @racket[arr], and @racket[dk] must be
nonnegative.

Conceptually, @racket[g] is applied @racket[dk] times to each element in each row of axis @racket[k],
once for each nonnegative index @racket[jk < dk].

Turning vector elements into rows of a new last axis using @racket[array-axis-expand] and
@racket[vector-ref]:
@interaction[#:eval typed-eval
                    (define arr (array #['#(a b c) '#(d e f) '#(g h i)]))
                    (array-axis-expand arr 1 3 vector-ref)]
Creating a @racket[vandermonde-matrix]:
@interaction[#:eval typed-eval
                    (array-axis-expand (list->array '(1 2 3 4)) 1 5 expt)]

This function is a dual of @racket[array-axis-reduce] in that it can be used to invert applications
of @racket[array-axis-reduce].
To do so, @racket[g] should be a destructuring function that is dual to the constructor passed to
@racket[array-axis-reduce].
Example dual pairs are @racket[vector-ref] and @racket[build-vector], and @racket[list-ref] and
@racket[build-list].

(Do not pass @racket[list-ref] to @racket[array-axis-expand] if you care about performance, though.
See @racket[list-array->array] for a more efficient solution.)
}

@defproc[(array->list-array [arr (Array A)] [k Integer 0]) (Array (Listof A))]{
Returns an array of lists, computed as if by applying @racket[list] to the elements in each row of
axis @racket[k].
@examples[#:eval typed-eval
                 (define arr (index-array #(3 3)))
                 arr
                 (array->list-array arr 1)
                 (array-ref (array->list-array (array->list-array arr 1) 0) #())]
See @racket[mean] for more useful examples, and @racket[array-axis-reduce] for an example that shows
how @racket[array->list-array] is implemented.
}

@defproc[(list-array->array [arr (Array (Listof A))] [k Integer 0]) (Array A)]{
Returns an array in which the list elements of @racket[arr] comprise a new axis @racket[k].
Equivalent to @racket[(array-axis-expand arr k n list-ref)] where @racket[n] is the
length of the lists in @racket[arr], but with O(1) indexing.

@examples[#:eval typed-eval
                 (define arr (array->list-array (index-array #(3 3)) 1))
                 arr
                 (list-array->array arr 1)]

For fixed @racket[k], this function and @racket[array->list-array] are mutual inverses with respect
to their array arguments.
}

@;{==================================================================================================}


@section[#:tag "array:other"]{Other Array Operations}

@subsection{Fast Fourier Transform}

@margin-note{Wikipedia: @hyperlink["http://wikipedia.org/wiki/Discrete_Fourier_transform"]{Discrete
Fourier Transform}}
@defproc[(array-axis-fft [arr (Array Number)] [k Integer]) (Array Float-Complex)]{
Performs a discrete Fourier transform on axis @racket[k] of @racket[arr]. The length of @racket[k]
must be an integer power of two. (See @racket[power-of-two?].) The scaling convention is
determined by the parameter @racket[dft-convention], which defaults to the convention used in
signal processing.

The transform is done in parallel using @racket[max-math-threads] threads.
}

@defproc[(array-axis-inverse-fft [arr (Array Number)] [k Integer]) (Array Float-Complex)]{
The inverse of @racket[array-axis-fft], performed by parameterizing the forward transform on
@racket[(dft-inverse-convention)].
}

@defproc[(array-fft [arr (Array Number)]) FCArray]{
Performs a discrete Fourier transform on each axis of @racket[arr] using @racket[array-axis-fft].
}

@defproc[(array-inverse-fft [arr (Array Number)]) FCArray]{
The inverse of @racket[array-fft], performed by parameterizing the forward transform on
@racket[(dft-inverse-convention)].
}


@;{==================================================================================================}


@section[#:tag "array:subtypes"]{Subtypes}

@subsection[#:tag "flarray"]{Flonum Arrays}

@defidform[FlArray]{
The type of @deftech{flonum arrays}, a subtype of @racket[(Settable-Array Flonum)] that stores its
elements in an @racket[FlVector]. A flonum array is always @tech{strict}.
}

@defform[(flarray #[#[...] ...])]{
Like @racket[array], but creates @tech{flonum arrays}. The listed elements must be real numbers,
and may be exact.

@examples[#:eval typed-eval
                 (flarray 0.0)
                 (flarray #['x])
                 (flarray #[#[1 2] #[3 4]])]
}

@defproc[(array->flarray [arr (Array Real)]) FlArray]{
Returns a flonum array that has approximately the same elements as @racket[arr]. Exact
elements will likely lose precision during conversion.
}

@defproc[(flarray-data [arr FlArray]) FlVector]{
Returns the elements of @racket[arr] in a flonum vector, in row-major order.
@examples[#:eval typed-eval
                 (flarray-data (flarray #[#[1 2] #[3 4]]))]
}

@defproc[(flarray-map [f (Flonum ... -> Flonum)] [arrs FlArray] ...) FlArray]{
Maps the function @racket[f] over the arrays @racket[arrs]. If the arrays do not have the same
shape, they are @tech{broadcast} first. If the arrays do have the same shape, this operation can
be quite fast.

The function @racket[f] is meant to accept the same number of arguments as the number of its
following flonum array arguments. However, a current limitation in Typed Racket requires @racket[f]
to accept @italic{any} number of arguments. To map a single-arity function such as @racket[fl+],
for now, use @racket[inline-flarray-map] or @racket[array-map].
}

@defform[(inline-flarray-map f arrs ...)
         #:contracts ([f  (Flonum ... -> Flonum)]
                      [arrs  FlArray])]{
Like @racket[inline-array-map], but for flonum arrays.

@bold{This is currently unavailable in untyped Racket.}
}

@deftogether[(@defproc[(flarray+ [arr0 FlArray] [arr1 FlArray]) FlArray]
              @defproc[(flarray* [arr0 FlArray] [arr1 FlArray]) FlArray]
              @defproc*[([(flarray- [arr FlArray]) FlArray]
                         [(flarray- [arr0 FlArray] [arr1 FlArray]) FlArray])]
              @defproc*[([(flarray/ [arr FlArray]) FlArray]
                         [(flarray/ [arr0 FlArray] [arr1 FlArray]) FlArray])]
              @defproc[(flarray-min [arr0 FlArray] [arr1 FlArray]) FlArray]
              @defproc[(flarray-max [arr0 FlArray] [arr1 FlArray]) FlArray]
              @defproc[(flarray-scale [arr FlArray] [x Flonum]) FlArray]
              @defproc[(flarray-abs [arr FlArray]) FlArray]
              @defproc[(flarray-sqr [arr FlArray]) FlArray]
              @defproc[(flarray-sqrt [arr FlArray]) FlArray])]{
Arithmetic lifted to flonum arrays.
}

@subsection[#:tag "fcarray"]{Float-Complex Arrays}

@defidform[FCArray]{
The type of @deftech{float-complex arrays}, a subtype of @racket[(Settable-Array Float-Complex)]
that stores its elements in a pair of @racket[FlVector]s. A float-complex array is always
@tech{strict}.
}

@defform[(fcarray #[#[...] ...])]{
Like @racket[array], but creates @tech{float-complex arrays}. The listed elements must be numbers,
and may be exact.

@examples[#:eval typed-eval
                 (fcarray 0.0)
                 (fcarray #['x])
                 (fcarray #[#[1 2+1i] #[3 4+3i]])]
}

@defproc[(array->fcarray [arr (Array Number)]) FCArray]{
Returns a float-complex array that has approximately the same elements as @racket[arr]. Exact
elements will likely lose precision during conversion.
}

@deftogether[(@defproc[(fcarray-real-data [arr FCArray]) FlVector]
              @defproc[(fcarray-imag-data [arr FCArray]) FlVector])]{
Return the real and imaginary parts of @racket[arr]'s elements in flonum vectors, in row-major order.
@examples[#:eval typed-eval
                 (define arr (fcarray #[#[1 2+1i] #[3 4+3i]]))
                 (fcarray-real-data arr)
                 (fcarray-imag-data arr)]
}

@defproc[(fcarray-map [f (Float-Complex ... -> Float-Complex)] [arrs FCArray] ...) FCArray]{
Maps the function @racket[f] over the arrays @racket[arrs]. If the arrays do not have the same
shape, they are @tech{broadcast} first. If the arrays do have the same shape, this operation can
be quite fast.

The function @racket[f] is meant to accept the same number of arguments as the number of its
following float-complex array arguments. However, a current limitation in Typed Racket requires
@racket[f] to accept @italic{any} number of arguments. To map a single-arity function, for now,
use @racket[inline-fcarray-map] or @racket[array-map].
}

@defform[(inline-fcarray-map f arrs ...)
         #:contracts ([f  (Float-Complex ... -> Float-Complex)]
                      [arrs  FCArray])]{
Like @racket[inline-array-map], but for float-complex arrays.

@bold{This is currently unavailable in untyped Racket.}
}

@deftogether[(@defproc[(fcarray+ [arr0 FCArray] [arr1 FCArray]) FCArray]
              @defproc[(fcarray* [arr0 FCArray] [arr1 FCArray]) FCArray]
              @defproc*[([(fcarray- [arr FCArray]) FCArray]
                         [(fcarray- [arr0 FCArray] [arr1 FCArray]) FCArray])]
              @defproc*[([(fcarray/ [arr FCArray]) FCArray]
                         [(fcarray/ [arr0 FCArray] [arr1 FCArray]) FCArray])]
              @defproc[(fcarray-scale [arr FCArray] [z Float-Complex]) FCArray]
              @defproc[(fcarray-sqr [arr FCArray]) FCArray]
              @defproc[(fcarray-sqrt [arr FCArray]) FCArray]
              @defproc[(fcarray-conjugate [arr FCArray]) FCArray])]{
Arithmetic lifted to float-complex arrays.
}

@deftogether[(@defproc[(fcarray-real-part [arr FCArray]) FlArray]
              @defproc[(fcarray-imag-part [arr FCArray]) FlArray]
              @defproc[(fcarray-make-rectangular [arr0 FlArray] [arr1 FlArray]) FCArray]
              @defproc[(fcarray-magnitude [arr FCArray]) FlArray]
              @defproc[(fcarray-angle [arr FCArray]) FlArray]
              @defproc[(fcarray-make-polar [arr0 FlArray] [arr1 FlArray]) FCArray])]{
Conversions to and from complex numbers, lifted to flonum and float-complex arrays.
}


@;{==================================================================================================}


@section[#:tag "array:strict"]{Strictness}

@defparam[array-strictness strictness Boolean]{
Determines whether @racketmodname[math/array] functions return strict arrays.
The default value is @racket[#t].

See @secref{array:nonstrict} for a discussion on nonstrict arrays.
}

@defproc[(array-strict? [arr (Array A)]) Boolean]{
Returns @racket[#t] when @racket[arr] is @tech{strict}.
@examples[#:eval typed-eval
                 (define arr
                   (parameterize ([array-strictness #f])
                     (array+ (array 10) (array #[0 1 2 3]))))
                 (array-strict? arr)
                 (array-strict! arr)
                 (array-strict? arr)]
}

@defproc[(array-strict! [arr (Array A)]) Void]{
Causes @racket[arr] to compute and store all of its elements. Thereafter, @racket[arr]
computes its elements by retrieving them from the store.

If @racket[arr] is already strict, @racket[(array-strict! arr)] does nothing.
}

@defform[(array-strict arr)
         #:contracts ([arr (Array A)])]{
An expression form of @racket[array-strict!], which is often more convenient. First evaluates
@racket[(array-strict! arr)], then returns @racket[arr].

This is a macro so that Typed Racket will preserve @racket[arr]'s type exactly. If it were a
function, @racket[(array-strict arr)] would always have the type @racket[(Array A)], even if
@racket[arr] were a subtype of @racket[(Array A)], such as @racket[(Mutable-Array A)].
}

@deftogether[(@defproc[(array-default-strict! [arr (Array A)]) Void]
              @defform[(array-default-strict arr)
                       #:contracts ([arr (Array A)])])]{
Like @racket[array-strict!] and @racket[array-strict], but do nothing when @racket[array-strictness]
is @racket[#f].

Apply one of these to return values from library functions to ensure that users get strict arrays
by default. See @secref{array:nonstrict} for details.
}

@defproc[(build-simple-array [ds In-Indexes] [proc (Indexes -> A)]) (Array A)]{
Like @racket[build-array], but returns an array without storage that is nevertheless considered to be
strict, regardless of the value of @racket[array-strictness].
Such arrays will @italic{not} cache their elements when @racket[array-strict!] or
@racket[array-strict] is applied to them.

Use @racket[build-simple-array] to create arrays that represent simple functions of their indexes.
For example, basic array constructors such as @racket[make-array] are defined in terms of this or its
unsafe counterpart.

@bold{Be careful with this function.} While it creates arrays that are always memory-efficient,
it is easy to ruin your program's performance by using it to define arrays for which element lookup
is permanently expensive. In the wrong circumstances, using it instead of @racket[build-array] can
turn a linear algorithm into an exponential one!

In general, use @racket[build-simple-array] when
@itemlist[@item{Computing an element is never more expensive than computing a row-major index followed
                by applying @racket[vector-ref]. An example is @racket[index-array], which only
                computes row-major indexes.}
          @item{Computing an element is independent of any other array's elements.
                In this circumstance, it is impossible to compose some unbounded number of possibly
                expensive array @tech{procedures}.}
          @item{You can prove that each element will be computed at most once, throughout the
                entire life of your program. This is true, for example, when the result is sent
                only to a function that makes a copy of it, such as @racket[array-lazy] or
                @racket[array->mutable-array].}]
See @racket[array-lazy] for an example of the last circumstance.
}

@defproc[(array-lazy [arr (Array A)]) (Array A)]{
Returns an immutable, @tech{nonstrict} array with the same elements as @racket[arr], but element
computations are cached.

Perhaps the most natural way to use @racket[array-lazy] is for so-called ``dynamic programming,''
or memoizing a function that happens to have a rectangular domain.
For example, this computes the first 10 Fibonacci numbers in linear time:
@interaction[#:eval typed-eval
                    (eval:alts
                     (define: fibs : (Array Natural)
                       (array-lazy
                        (build-simple-array
                         #(10) (λ: ([js : Indexes])
                                 (define j (vector-ref js 0))
                                 (cond [(j . < . 2)  j]
                                       [else  (+ (array-ref fibs (vector (- j 1)))
                                                 (array-ref fibs (vector (- j 2))))])))))
                     (void))
                    (eval:alts
                     fibs
                     (ann (array #[0 1 1 2 3 5 8 13 21 34]) (Array Natural)))]
Because @racket[build-simple-array] never stores its elements, its procedure argument may refer to
the array it returns.
Wrapping its result with @racket[array-lazy] makes each @racket[array-ref] take no more than linear
time; further, each takes constant time when the elements of @racket[fibs] are computed in order.
Without @racket[array-lazy], computing the elements of @racket[fibs] would take exponential time.

Printing a lazy array computes and caches all of its elements, as does applying
@racket[array-strict!] or @racket[array-strict] to it.

Except for arrays returned by @racket[build-simple-array], it is useless to apply @racket[array-lazy]
to a @tech{strict} array.
Using the lazy copy instead of the original only degrades performance.

While it may seem that @racket[array-lazy] should just return @racket[arr] when @racket[arr] is
strict, this would violate the invariant that @racket[array-lazy] returns immutable arrays.
For example:
@interaction[#:eval typed-eval
                    (eval:alts
                     (: array-maybe-lazy (All (A) ((Array A) -> (Array A))))
                     (void))
                    (eval:alts
                     (define (array-maybe-lazy arr)
                       (if (array-strict? arr) arr (array-lazy arr)))
                     (void))
                    (eval:alts
                     (define arr (mutable-array #[0 1 2 3]))
                     (void))
                    (eval:alts
                     (define brr (array-maybe-lazy arr))
                     (void))
                    (eval:alts
                     (array-set! arr #(0) -1000)
                     (void))
                    (eval:alts
                     brr
                     (ann (array #[-1000 1 2 3]) (Array Integer)))]
}


@;{==================================================================================================}


@;{
@section[#:tag "array:unsafe"]{Unsafe Operations}

unsafe-array-ref
unsafe-array-set!
unsafe-array-proc
unsafe-settable-array-set-proc
unsafe-fcarray 
in-unsafe-array-indexes
make-unsafe-array-set-proc
make-unsafe-array-proc
unsafe-build-array
unsafe-build-simple-array
unsafe-vector->array
unsafe-flarray
unsafe-array-transform
unsafe-array-axis-reduce
}

@;{
Don't know whether or how to document these yet:

parallel-array-strict
parallel-array->mutable-array
array/syntax
}

@(close-eval typed-eval)
