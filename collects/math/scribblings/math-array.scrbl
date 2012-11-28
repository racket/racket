#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label racket/base racket/vector racket/match racket/unsafe/ops racket/string
                     math plot
                     (only-in typed/racket/base
                              ann inst : λ: define: make-predicate
                              Flonum Real Boolean Any Integer Index Natural Exact-Positive-Integer
                              Nonnegative-Real Sequenceof Fixnum Values
                              All U List Vector Listof Vectorof Struct))
          "utils.rkt")

@(define typed-eval (make-math-eval))
@interaction-eval[#:eval typed-eval
                         (require racket/match
                                  racket/vector
                                  racket/string
                                  racket/sequence)]

@title[#:tag "arrays" #:style 'toc]{Arrays}
@(author-neil)

@bold{Performance Warning:} Most of the array-producing functions exported by
@racketmodname[math/array] run 25-50 times slower in untyped Racket, due to the
overhead of checking higher-order contracts. We are working on it.

For now, if you need speed, use the @racketmodname[typed/racket] language.

@defmodule[math/array]

One of the most common ways to structure data is with an array: a grid of homogeneous,
independent elements, usually consisting of rows and columns. But an array data type
is usually absent from functional languages' libraries. This is probably because arrays
are perceived as requiring users to operate on them using destructive updates, write
loops that micromanage array elements, and in general, stray far from the declarative
ideal.

@margin-note{TODO: Cite Haskell array paper}
Normally, they do. However, experience in Python, and more recently Data-Parallel Haskell,
has shown that providing the right data types and a rich collection of whole-array operations
allows working effectively with arrays in a functional, declarative style. As a bonus,
doing so opens the possibility of parallelizing nearly every operation.

It requires changing how we think of arrays, starting with this definition:

@nested[#:style 'inset]{@bold{An @deftech{array} is a function with a finite, rectangular domain.}}

Some arrays are mutable, some are lazy, some are strict, some are sparse, and most
do not even allocate contiguous space to store their elements. All are functions that can be
applied to indexes to retrieve elements.

@local-table-of-contents[]

@section{Preliminaries}

@subsection{Definitions}

An array's domain is determined by its @deftech{shape}, a vector of @racket[Index] such as
@racket[#(4 5)], @racket[#(10 1 5 8)] or @racket[#()]. The shape's length is the number of array
dimensions, or @deftech{axes}, and its contents are the length of each axis in row-major order.
The product of the axis lengths is the array's size. In particular, an array with shape
@racket[#()] has one element.

An array's @deftech{procedure} defines the function that the array represents. The procedure
returns an element when applied to a vector of indexes in the array's domain.

A @deftech{strict} array is one whose procedure computes elements only by indexing a vector or
some other kind of storage. Arrays that perform non-indexing computation to return elements
are @deftech{non-strict}. Almost all array functions exported by @racket[math/array] return
non-strict arrays. Exceptions are noted in the documentation.

@bold{Non-strict arrays are not lazy.} By default, arrays do not cache computed elements, but
like functions, recompute them every time they are referred to. See @secref{array:strictness}
for details.

A @deftech{pointwise} operation is one that operates on each array element independently, on each
corresponding pair of elements from two arrays independently, or on a corresponding collection
of elements from many arrays independently. This is usually done using @racket[array-map].

When a pointwise operation is performed on two arrays with different shapes, the arrays are
@deftech{broadcast} so that their shapes match. See @secref{array:broadcasting} for details.

@subsection{Quick Start}

The most direct way to construct an array is to use @racket[build-array] to specify
its @tech{shape} and @tech{procedure}:
@interaction[#:eval typed-eval
                    (define arr
                      (build-array #(4 5) (λ: ([js : Indexes])
                                            (match-define (vector j0 j1) js)
                                            (+ j0 j1))))]
This creates a @tech{non-strict} array, meaning that its elements are not computed unless
referred to. One way to refer to elements is to print them:
@interaction[#:eval typed-eval
                    arr]
@bold{Important:} Printing @racket[arr] did not cache its elements. A non-strict array's elements
are recomputed every time they are referred to. But see @racket[array-lazy], which constructs
arrays that cache computed elements.

Arrays can be made @tech{strict} using @racket[array-strict] or @racket[array->mutable-array]:
@interaction[#:eval typed-eval
                    (array-strict arr)
                    (array->mutable-array arr)]
The difference is that @racket[(array-strict arr)] returns @racket[arr] whenever @racket[arr] is
already strict. (It therefore has a less precise return type.)
See @secref{array:strictness} for details.

Arrays can be indexed using @racket[array-ref], and settable arrays can be mutated using
@racket[array-set!]:
@interaction[#:eval typed-eval
                    (array-ref arr #(2 3))
                    (define brr (array->mutable-array arr))
                    (array-set! brr #(2 3) -1)
                    brr]
However, both of these activities are discouraged in favor of functional, whole-array operations.

An array can be sliced using a list of sequences or slice objects. The following examples are
all equivalent, keeping every row of @racket[arr] and every even-numbered column:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list (in-range 0 4) (:: 0 5 2)))
                    (array-slice-ref arr (list ::... (:: 0 5 2)))
                    (array-slice-ref arr (list '(0 1 2 3) (:: 0 5 2)))
                    (array-slice-ref arr (list (::) (in-range 0 5 2)))]
Here, @racket[::] constructs a slice object and has semantics similar to, but not exactly like,
@racket[in-range]. The special slice object @racket[::...] means ``every row in every unspecified,
adjacent axis'' (which in this case means every row in axis 0).

Arrays can be mapped over and otherwise operated on @tech{pointwise}:
@interaction[#:eval typed-eval
                    (array-map (λ: ([n : Natural]) (* 2 n)) arr)
                    (array+ arr arr)]
When arrays have different shapes, they are @tech{broadcast} to the same shape before applying
the pointwise operation:
@interaction[#:eval typed-eval
                    (array* arr (array 2))
                    (array* arr (array #[0 1 0 2 0]))]
Zero-dimensional arrays like @racket[(array 2)] can be broadcast to any shape.
See @secref{array:broadcasting} for details.

@subsection[#:tag "array:strictness"]{Non-Strictness}

Almost every function exported by @racketmodname[math/array] returns @tech{non-strict} arrays,
meaning that they compute each element every time the element is referred to.

This design decision is motivated by the observation that, in functional code that operates on
arrays, the elements in most intermediate arrays are referred to exactly once. Additionally,
many arrays consist only of a single, repeated constant.

The problem is well-illustrated by operating on whole vectors in a functional style:
@interaction[#:eval typed-eval
                    (vector-map string-append
                                (vector-map string-append
                                            (vector "Hello " "Hallo " "Jó napot ")
                                            (vector "Ada" "Edsger" "John"))
                                (make-vector 3 "!"))]
There are two memory inefficiencies here. The first is that the inner
@racket[(vector-map string-append ...)] allocates a vector whose elements are referred to only
once. The second is @racket[(vector "!" "!" "!")], which we construct to avoid looping over
the seemingly extraneous intermediate vector.

This is the same example using arrays:
@interaction[#:eval typed-eval
                    (array-map string-append
                               (array-map string-append
                                          (array #["Hello " "Hallo " "Jó napot "])
                                          (array #["Ada" "Edsger" "John"]))
                               (make-array #(3) "!"))]
Here, because @racket[array-map] returns non-strict arrays, the inner
@racket[(array-map string-append ...)] does not create intermediate storage. Neither does
@racket[(make-array #(3) "!")]. In fact, no elements are computed until they are printed,
and none of these arrays allocates contiguous space to store its elements.

Because the major bottleneck in functional language performance is almost always allocation and
subsequent garbage collection, avoiding allocation can significantly increase performance.
Additionally, allocating contiguous space for intermediate array elements forces synchronization
between array operations, so not doing so provides opportunities for future parallelization.

@margin-note*{Still, it is easier to reason about non-strict array performance than lazy array
              performance.}
The downside is that it is more difficult to reason about the performance characteristics of
operations on non-strict arrays. Also, when using arrays, you must decide which arrays to make
strict. (This can be done using the @racket[array-strict] function.) Fortunately, there is a
simple rule of thumb:

@nested[#:style 'inset]{@bold{Make arrays strict when you must refer to most of their elements
                              more than once or twice.}}

Additionally, having to name an array is a good indicator that it should be strict. In the
following example, which computes @racket[(+ (expt x x) (expt x x))] for @racket[x] from @racket[0]
to @racket[2499], every element in @racket[xrr] is computed twice:
@racketblock[(define xrr (array-expt (index-array #(50 50))
                                     (index-array #(50 50))))
             (define res (array+ xrr xrr))]
Having to name @racket[xrr] means we should make it strict:
@racketblock[(define xrr (array-strict
                          (array-expt (index-array #(50 50))
                                      (index-array #(50 50)))))
             (define res (array+ xrr xrr))]
Doing so halves the time it takes to compute @racket[res]'s elements because each
@racket[(expt x x)] is computed only once.

An exception to these guidelines is non-strict arrays returned from constructors like
@racket[make-array], which barely compute anything at all. Such exceptions are noted in each
function's documentation. Another exception is returning an array from a function. In this
case, application sites should make the array strict, if necessary.

If you cannot determine whether to make arrays strict, or are using arrays for so-called
``dynamic programming,'' you can make them lazy using @racket[array-lazy].

@subsection[#:tag "array:broadcasting"]{Broadcasting}

It is often useful to apply a @tech{pointwise} operation to two or more arrays in a many-to-one
manner. Library support for this, which @racketmodname[math/array] provides, is called
@tech[#:key "broadcast"]{broadcasting}.

@examples[#:eval typed-eval
                 (define diag
                   (diagonal-array 2 6 1 0))
                 (array-shape diag)
                 diag
                 (array-shape (array 10))
                 (array* diag (array 10))
                 (array+ (array* diag (array 10))
                         (array #[0 1 2 3 4 5]))]

@subsubsection[#:tag "array:broadcasting:rules"]{Broadcasting Rules}

Suppose we have two array shapes @racket[ds = (vector d0 d1 ...)] and
@racket[es = (vector e0 e1 ...)]. Broadcasting proceeds as follows:
@itemlist[#:style 'ordered
  @item{The shorter shape is padded on the left with @racket[1] until it is the same length
        as the longer shape.}
  @item{For each axis @racket[k], @racket[dk] and @racket[ek] are compared. If @racket[dk = ek],
        the result axis is @racket[dk]; if one axis is length @racket[1], the result axis
        is the length of the other; otherwise fail.}
  @item{Both arrays' axes are stretched by (conceptually) copying singleton axes' rows.}
  ]

Suppose we have an array @racket[drr] with shape @racket[ds = #(4 1 3)] and another array
@racket[err] with shape @racket[es = #(3 3)]. Following the rules:
@itemlist[#:style 'ordered
  @item{@racket[es] is padded to get @racket[#(1 3 3)].}
  @item{The result axis is derived from @racket[#(4 1 3)] and @racket[#(1 3 3)] to get
        @racket[#(4 3 3)].}
  @item{@racket[drr]'s second axis is stretched to length @racket[3], and @racket[err]'s new first
        axis (which is length @racket[1] by rule 1) is stretched to length @racket[4].}
  ]

Conceptually, step 1 is the same as adding square brackets around @racket[err] in literal
@racket[array] syntax. For example, if @racket[err = (array #[#[0 1 2] #[3 4 5] #[6 7 8]])],
after padding its shape, it is @racket[(array #[#[#[0 1 2] #[3 4 5] #[6 7 8]]])].

The same example, but more concrete:
@interaction[#:eval typed-eval
                    (define nums
                      (array #[#[#["00" "01" "02"]]
                               #[#["10" "11" "12"]]
                               #[#["20" "21" "22"]]
                               #[#["30" "31" "32"]]]))
                    (array-shape nums)
                    (define lets
                      (array #[#["aa" "ab" "ac"]
                               #["ba" "bb" "bc"]
                               #["ca" "cb" "cc"]]))
                    (array-shape lets)
                    (define nums+lets (array-map string-append nums lets))
                    (array-shape nums+lets)
                    nums+lets]
Notice how the row @racket[#["00" "01" "02"]] in @racket[nums] is repeated in the result
because @racket[nums]'s second axis was stretched during broadcasting. Also, the column
@racket[#[#["aa"] #["ba"] #["ca"]]] in @racket[lets] is repeated because @racket[lets]'s first
axis was stretched.

@subsubsection[#:tag "array:broadcasting:control"]{Broadcasting Control}

The parameter @racket[array-broadcasting] controls how pointwise operations @tech{broadcast}
arrays. Its default value is @racket[#t], which means that broadcasting proceeds as described
in @secref{array:broadcasting:rules}. Another possible value is @racket[#f], which allows pointwise
operations to succeed only if array shapes match exactly:
@interaction[#:eval typed-eval
                    (parameterize ([array-broadcasting  #f])
                      (array* (index-array #(3 3)) (array 10)))]

Another option is @hyperlink["http://www.r-project.org"]{R}-style permissive broadcasting,
which allows pointwise operations to @italic{always} succeed, by repeating any axis instead
of stretching just singleton axes:
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

@section{Types, Predicates and Accessors}

@defform[(Array A)]{
The parent array type. Its type parameter is the type of the array's elements.

The polymorphic @racket[Array] type is @italic{covariant}, meaning that @racket[(Array A)] is a
subtype of @racket[(Array B)] if @racket[A] is a subtype of @racket[B]:
@examples[#:eval typed-eval
                 (define arr (array #[1 2 3 4 5]))
                 arr
                 (ann arr (Array Real))
                 (ann arr (Array Any))]
Because subtyping is transitive, the @racket[(Array A)] in the preceeding subtyping rule can be
replaced with any of @racket[(Array A)]'s subtypes, including descendant types of @racket[Array].
For example, @racket[(Mutable-Array A)] is a subtype of @racket[(Array B)] if @racket[A] is a
subtype of @racket[B]:
@examples[#:eval typed-eval
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
@examples[#:eval typed-eval
                 (define arr (mutable-array #[1 2 3 4 5]))
                 arr
                 (ann arr (Settable-Array Integer))
                 (ann arr (Settable-Array Real))]
}

@defform[(Mutable-Array A)]{
The type of mutable arrays. Its type parameter is the type of the array's elements.

Arrays of this type are always strict, and store their elements in a @racket[(Vectorof A)]:
@examples[#:eval typed-eval
                 (define arr (mutable-array #[1 2 3 4 5]))
                 (array-strict? arr)
                 (mutable-array-data arr)]
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

@defproc[(array-strict? [arr (Array A)]) Boolean]{
Returns @racket[#t] when @racket[arr] is @tech{strict}.
@examples[#:eval typed-eval
                 (define arr (array #[0 1 2 3]))
                 (array-strict? arr)
                 (array-strict? (array-strict arr))]
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

@section{Construction}

@defform[(array #[#[...] ...])]{
Creates an @racket[Array] from nested rows of expressions.

The vector syntax @racket[#[...]] delimits rows. These may be nested to any depth, and must have a
rectangular shape. Using square parentheses is not required, but is encouraged to help distinguish
array contents from array shapes and other vectors.

@examples[#:eval typed-eval
                 (array 0)
                 (array #[0 1 2 3])
                 (array #[#[1 2 3] #[4 5 6]])
                 (array #[#[1 2 3] #[4 5]])]
As with the @racket[list] constructor, the type chosen for the array is the narrowest type
all the elements can have. Unlike @racket[list], because @racket[array] is syntax, the only way
to change the element type is to annotate the result.
@interaction[#:eval typed-eval
                    (list 1 2 3)
                    (array #[1 2 3])
                    ((inst list Real) 1 2 3)
                    ((inst array Real) #[1 2 3])
                    (ann (array #[1 2 3]) (Array Real))]
Annotating should rarely be necessary because the @racket[Array] type is covariant.

Normally, the datums within literal vectors are implicitly quoted. However, when used within the
@racket[array] form, the datums must be explicitly quoted.
@interaction[#:eval typed-eval
                    #(this is okay)
                    (array #[not okay])
                    (array #['this 'is 'okay])
                    (array #['#(an) '#(array) '#(of) '#(vectors)])]
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
}

@defproc[(make-array [ds In-Indexes] [value A]) (Array A)]{
Returns an array with @tech{shape} @racket[ds], with every element's value as @racket[value].
Analogous to @racket[make-vector], but the result is @tech{non-strict}.
@examples[#:eval typed-eval
                 (make-array #() 5)
                 (make-array #(1 2) 'sym)
                 (make-array #(4 0 2) "Invisible")]
It is useless to make a strict copy of an array returned by @racket[make-array].
}

@defproc[(build-array [ds In-Indexes] [proc (Indexes -> A)]) (Array A)]{
Returns an array with @tech{shape} @racket[ds] and @tech{procedure} @racket[proc].
Analogous to @racket[build-vector], but the result is @tech{non-strict}.
@examples[#:eval typed-eval
                 (eval:alts
                  (define: fibs : (Array Exact-Positive-Integer)
                    (build-array
                     #(10) (λ: ([js : Indexes])
                             (define j (vector-ref js 0))
                             (cond [(j . < . 2)  1]
                                   [else  (+ (array-ref fibs (vector (- j 1)))
                                             (array-ref fibs (vector (- j 2))))]))))
                  (void))
                 (eval:alts
                  fibs
                  (ann (array #[1 1 2 3 5 8 13 21 34 55]) (Array Exact-Positive-Integer)))]
Because @racket[build-array] returns a non-strict array, @racket[fibs] may refer to itself
within its definition. Of course, this naïve implementation computes its elements in time
exponential in the size of @racket[fibs]. A quick, widely applicable fix is given in
@racket[array-lazy]'s documentation.
}

@defproc[(array-strict [arr (Array A)]) (Array A)]{
Returns a @tech{strict} array with the same elements as @racket[arr]. If
@racket[(array-strict? arr)] is @racket[#t], returns @racket[arr].

Currently, if @racket[(array-strict? arr)] is @racket[#f], @racket[(array-strict arr)] returns
a new @racket[Mutable-Array]. Typed Racket code is unlikely to accidentally rely on this fact
(see @racket[mutable-array?] for the reason), but untyped Racket code could easily do so. Refrain.
The type used for new strict arrays could change to another descendant of @racket[Array] in a
future release. Use @racket[array->mutable-array] to reliably get a mutable array.
}

@defproc[(array-lazy [arr (Array A)]) (Array A)]{
Returns a @tech{non-strict} array with the same elements as @racket[arr], but element
computations are cached and reused.

The example in @racket[build-array]'s documentation computes the Fibonacci numbers in exponential
time. Speeding it up to linear time only requires wrapping its definition with @racket[array-lazy]:
@interaction[#:eval typed-eval
                    (eval:alts
                     (define: fibs : (Array Exact-Positive-Integer)
                       (array-lazy
                        (build-array
                         #(10) (λ: ([js : Indexes])
                                 (define j (vector-ref js 0))
                                 (cond [(j . < . 2)  1]
                                       [else  (+ (array-ref fibs (vector (- j 1)))
                                                 (array-ref fibs (vector (- j 2))))])))))
                     (void))
                    (eval:alts
                     fibs
                     (ann (array #[1 1 2 3 5 8 13 21 34 55]) (Array Exact-Positive-Integer)))]
Printing a lazy array computes and caches all of its elements, as does applying
@racket[array-strict] to it.
}

@defproc[(make-mutable-array [ds In-Indexes] [vs (Vectorof A)]) (Mutable-Array A)]{
Returns a mutable array with @tech{shape} @racket[ds] and elements @racket[vs]; assumes
@racket[vs] are in row-major order. If there are too many or too few elements in @racket[vs],
@racket[(make-mutable-array ds vs)] raises an error.
@examples[#:eval typed-eval
                 (make-mutable-array #(3 3) #(0 1 2 3 4 5 6 7 8))
                 (make-mutable-array #() #(singleton))
                 (make-mutable-array #(4) #(1 2 3 4 5))]
}

@defproc[(array->mutable-array [arr (Array A)]) (Mutable-Array A)]{
Returns a mutable array with the same elements as @racket[arr].

While @racket[array-strict] may return any subtype of @racket[Array], @racket[array->mutable-array]
always returns a @racket[Mutable-Array]. Additionally, @racket[(array-strict arr)] may return
@racket[arr] if @racket[arr] is already strict, but @racket[array->mutable-array] always makes a
copy.
}

@defproc[(mutable-array-copy [arr (Mutable-Array A)]) (Mutable-Array A)]{
Like @racket[(array->mutable-array arr)], but is restricted to mutable arrays. It is also faster.
}

@defproc[(indexes-array [ds In-Indexes]) (Array Indexes)]{
Returns an array with @tech{shape} @racket[ds], with each element set to its position in the
array.
@examples[#:eval typed-eval
                 (indexes-array #())
                 (indexes-array #(4))
                 (indexes-array #(2 3))
                 (indexes-array #(4 0 2))]
}

@defproc[(index-array [ds In-Indexes]) (Array Index)]{
Returns an array with @tech{shape} @racket[ds], with each element set to its row-major index in
the array.
@examples[#:eval typed-eval
                 (index-array #(2 3))
                 (array-flatten (index-array #(2 3)))]
}

@defproc[(axis-index-array [ds In-Indexes] [axis Integer]) (Array Index)]{
Returns an array with @tech{shape} @racket[ds], with each element set to its position in axis
@racket[axis]. The axis number @racket[axis] must be nonnegative and less than the number of axes
(the length of @racket[ds]).
@examples[#:eval typed-eval
                 (axis-index-array #(3 3) 0)
                 (axis-index-array #(3 3) 1)
                 (axis-index-array #() 0)]
It is useless to make a strict copy of an array returned by @racket[axis-index-array].
}

@defproc[(diagonal-array [dims Integer] [axes-length Integer] [on-value A] [off-value A])
         (Array A)]{
Returns a square array with @racket[dims] axes, each with length @racket[axes-length]. The elements
on the diagonal (i.e. at indexes of the form @racket[(vector j j ...)] for @racket[j < axes-length])
have the value @racket[on-value]; the rest have the value @racket[off-value].
@examples[#:eval typed-eval
                 (diagonal-array 2 7 1 0)]
}

@section{Conversion}

@deftogether[(@defproc[(list->array [lst (Listof A)]) (Array A)]
              @defproc[(array->list [arr (Array A)]) (Listof A)])]{
Convert lists to single-axis arrays and back. If @racket[arr] has no axes or more than one axis,
it is (conceptually) flattened before being converted to a list.
@examples[#:eval typed-eval
                 (list->array '(1 2 3))
                 (list->array '((1 2 3) (4 5)))
                 (array->list (array #[1 2 3]))
                 (array->list (array 10))
                 (array->list (array #[#[1 2 3] #[4 5 6]]))]
For conversion between nested lists and multidimensional arrays, see @racket[list*->array] and
@racket[array->list*].
}

@deftogether[(@defproc[(vector->array [vec (Vectorof A)]) (Array A)]
              @defproc[(array->vector [arr (Array A)]) (Vectorof A)])]{
Like @racket[list->array] and @racket[array->list], but for vectors.
}

@defform[(Listof* A)]{
Equivalent to @racket[(U A (Listof A) (Listof (Listof A)) ...)] if infinite unions were allowed.
This is used as an argument type to @racket[list*->array] and as the return type of
@racket[array->list*].
}

@defproc[(list*->array [lsts (Listof* A)] [pred? ((Listof* A) -> Any : A)]) (Array A)]{
Converts a nested list of elements of type @racket[A] to an array. The predicate @racket[pred?]
identifies elements of type @racket[A]. The shape of @racket[lsts] must be rectangular.
@examples[#:eval typed-eval
                 (list*->array 'singleton symbol?)
                 (list*->array '(0 1 2 3) byte?)
                 (list*->array (list (list (list 5) (list 2 3))
                                     (list (list 4.0) (list 1.4 0.2 9.3)))
                               (make-predicate (Listof Nonnegative-Real)))]
The last example demonstrates why a predicate is required. There is no well-typed Typed Racket
function that behaves like @racket[list*->array] but does not require @racket[pred?], because
without it, there is no way to distinguish between rows and elements.
}

@defproc[(array->list* [arr (Array A)]) (Listof* A)]{
The inverse of @racket[list*->array].
}

@defform[(Vectorof* A)]{
Like @racket[(Listof* A)], but for vectors. See @racket[vector*->array] and @racket[array->vector*].
}

@defproc[(vector*->array [vecs (Vectorof* A)] [pred? ((Vectorof* A) -> Any : A)]) (Array A)]{
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
Concatenates @racket[arrs] along axis @racket[axis] to form a new array. If the arrays have
different shapes, they are broadcast first. The axis number @racket[axis] must be nonnegative
and @italic{no greater than} the number of axes after broadcasting.
@examples[#:eval typed-eval
                 (array-list->array (list (array 0) (array 1) (array 2) (array 3)))
                 (array-list->array (list (array 0) (array 1) (array 2) (array 3)) 1)
                 (array-list->array (list (array #[0 1 2 3]) (array #['a 'b 'c 'd])))
                 (array-list->array (list (array #[0 1 2 3]) (array '!)))
                 (array-list->array (list (array #[0 1 2 3]) (array '!)) 1)
                 ]
This function is a left inverse of @racket[array->array-list]. (It cannot be a right inverse
because broadcasting cannot be undone.)
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

@subsection{Printing}

@;{
array-custom-printer
print-array-fields
print-array
}

@section{Comprehensions and Sequences}

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

@section{Pointwise Operations}

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

@deftogether[(@defform[(array-abs arr)]
              @defform[(array-round arr)]
              @defform[(array-floor arr)]
              @defform[(array-ceiling arr)]
              @defform[(array-truncate arr)]
              @defform[(array-sqrt arr)]
              @defform[(array-sqr arr)]
              @defform[(array-log arr)]
              @defform[(array-exp arr)]
              @defform[(array-sin arr)]
              @defform[(array-cos arr)]
              @defform[(array-tan arr)]
              @defform[(array-asin arr)]
              @defform[(array-acos arr)]
              @defform[(array-atan arr)]
              @defform[(array-inexact->exact arr)]
              @defform[(array-exact->inexact arr)]
              @defform[(array-fl arr)])]{
Equivalent to @racket[(array-map f arr)], where @racket[f] is respectively
@racket[abs],
@racket[round],
@racket[floor],
@racket[ceiling],
@racket[truncate],
@racket[sqrt],
@racket[sqr],
@racket[log],
@racket[exp],
@racket[sin],
@racket[cos],
@racket[tan],
@racket[asin],
@racket[acos],
@racket[atan],
@racket[inexact->exact],
@racket[exact->inexact],
or @racket[fl].

Instead of @racket[array-fl], you might consider using @racket[array->flarray], which returns
a mutable array of flonums, stored unboxed in an flvector.
}

@deftogether[(@defform[(array-magnitude arr)]
              @defform[(array-angle arr)]
              @defform[(array-conjugate arr)]
              @defform[(array-real-part arr)]
              @defform[(array-imag-part arr)]
              @defform[(array-fc arr)])]{
Equivalent to @racket[(array-map f arr)], where @racket[f] is respectively
@racket[magnitude],
@racket[angle],
@racket[conjugate],
@racket[real-part],
@racket[imag-part],
or @racket[number->float-complex].

Instead of @racket[array-fc], you might consider using @racket[array->fcarray], which returns
a mutable array of float-complex numbers, stored unboxed in a pair of flvectors.
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

@deftogether[(@defform[(array-expt arr0 arr1)]
              @defform[(array-make-rectangular arr0 arr1)])]{
Equivalent to @racket[(array-map expt arr0 arr1)] and
@racket[(array-map make-rectangular arr0 arr1)], respectively.
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
@racket[not], @racket[and], @racket[or] and @racket[if] lifted to operate on arrays.

The short-cutting behavior of @racket[array-and], @racket[array-or] and @racket[array-if]
can keep array arguments' elements from being referred to (and thus computed). However,
they cannot be used to distinguish base and inductive cases in a recursive function, because
the array arguments are eagerly evaluated. For example, this function never returns:
@racketblock[(: array-factorial ((Array Integer) -> (Array Integer)))
             (define (array-factorial arr)
               (array-if (array<= arr (array 0))
                         (array 1)
                         (array* arr (array-factorial (array- arr (array 1))))))]
}

@subsection{Broadcasting}

@defparam[array-broadcasting broadcasting? (U Boolean 'permissive)]{
}

@;{
array-shape-broadcast
array-broadcast
}

@section{Indexing and Slicing}

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
}

@defproc[(array-slice-ref [arr (Array A)] [specs (Listof Slice-Spec)]) (Array A)]{
Returns a transformation of @racket[arr] according to the list of slice specifications
@racket[specs]. See @secref{array:slice-specs} for documentation and examples.
}

@defproc[(slice-indexes-array [ds In-Indexes] [specs (Listof Slice-Spec)]) (Array Indexes)]{
Returns the indexes of the elements that would be retrieved if an array with shape @racket[ds]
were sliced according to @racket[specs].
Equivalent to @racketblock[(array-slice-ref (indexes-array ds) specs)]
}

@defproc[(array-slice-set! [arr (Settable-Array A)] [specs (Listof Slice-Spec)] [vals (Array A)])
         Void]{
Like @racket[array-indexes-set!], but for slice specifications. Equivalent to
@racketblock[(let ([idxs  (slice-indexes-array (array-shape arr) specs)])
               (array-indexes-set! arr idxs vals))]
}

@subsection[#:tag "array:slice-specs"]{Slice Specifications}

@defidform[Slice-Spec]{
The type of a slice specification. Currently defined as
@racketblock[(U (Sequenceof Integer) Slice Slice-Dots Integer Slice-New-Axis)]
}

Slice specifications operate on axes independently. Different types represent different
transformations:
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

@subsubsection{@racket[(Sequenceof Integer)]: pick rows}

Using a sequence of integers as a slice specification picks rows from the corresponding axis. For
example, we might use lists of integers to pick @italic{every} row from every axis:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list '(0 1) '(0 1 2) '(0 1 2 3)))]
This simply copies the array. (However, because a sliced array is @tech{non-strict}, it does not
copy the elements.)

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
However, a sequence slice specification cannot remove axes.

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

@subsubsection{@racket[Slice]: pick rows in a length-aware way}

@deftogether[(@defidform[Slice]
              @defproc*[([(:: [end (U #f Integer) #f]) Slice]
                         [(:: [start (U #f Integer)] [end (U #f Integer)] [step Integer 1])
                          Slice])])]{
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
}

@subsubsection{@racket[Slice-Dots]: preserve remaining axes}

@deftogether[(@defidform[Slice-Dots]
              @defthing[::... Slice-Dots])]{
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
}

@subsubsection{@racket[Integer]: remove an axis}

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
All of these examples can be done using @racket[array-axis-ref], but including it in slice
specifications can be handy:
@interaction[#:eval typed-eval
                    (array-slice-ref arr (list ::... 1 (:: #f #f 2)))]

@subsubsection{@racket[Slice-New-Axis]: add an axis}

@deftogether[(@defidform[Slice-New-Axis]
              @defproc[(::new [dk Integer 1]) Slice-New-Axis])]{
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
}

@subsubsection{Other Slice Functions}

@deftogether[(@defproc[(slice? [v Any]) Boolean]
              @defproc[(slice-start [s Slice]) (U #f Fixnum)]
              @defproc[(slice-end [s Slice]) (U #f Fixnum)]
              @defproc[(slice-step [s Slice]) Fixnum])]{
The @racket[Slice] predicate and accessors.
}

@defproc[(slice-dots? [v Any]) Boolean]{
Returns @racket[#t] when @racket[v] is @racket[::...].
}

@deftogether[(@defproc[(slice-new-axis? [v Any]) Boolean]
              @defproc[(slice-new-axis-length [s Slice-New-Axis]) Index])]{
The @racket[Slice-New-Axis] predicate and accessors.
}

@defproc[(slice->range-values [s Slice] [dk Index]) (Values Fixnum Fixnum Fixnum)]{
Given a slice @racket[s] and an axis length @racket[dk], returns the arguments to @racket[in-range]
that would produce an equivalent slice specification.
}

@section{Folds}

@;{
array-axis-fold
array-fold
array-all-sum
array-all-prod
array-all-min
array-all-max
array-axis-count
array-all-count
array-axis-andmap
array-all-andmap
array-axis-ormap
array-all-ormap
array-all-eq?
array-all-equal?
array-all-eqv?
array-all=
array-axis-sum
array-axis-prod
array-axis-min
array-axis-max
array-lift-comparison
array-axis-fft
array-fft
array-inverse-fft
array-axis-inverse-fft
}

@section{Transformations}

@defidform[array-axis-insert]{
}

@defidform[array-axis-ref]{
}

@defidform[array-flatten]{
}

@;{
array-transform
array-axis-permute
array-axis-swap
array-append*
array-reshape
}

@section{Subtypes}

@subsection{Flonum Arrays}

@defidform[FlArray]{
}

@defproc[(array->flarray [arr (Array Real)]) FlArray]{
Returns an flarray that has approximately the same elements as @racket[arr].
The elements may lose precision during the conversion.
}

@;{
FlArray
flarray-scale
flarray-round
flarray-floor
flarray-map
flarray-truncate
flarray-abs
flarray-sqr
flarray-sqrt
flarray-log
flarray-exp
flarray-sin
flarray-cos
flarray-tan
flarray-asin
flarray-acos
flarray-atan
flarray+
flarray*
flarray-data
flarray
flarray-ceiling
array->flarray
inline-flarray-map
flarray-
flarray/
flarray-expt
flarray-min
flarray-max
flarray=
flarray<
flarray<=
flarray>
flarray>=
}

@subsection{Float-Complex Arrays}

@defidform[FCArray]{
}

@defproc[(array->fcarray [arr (Array Number)]) FCArray]{
Returns an fcarray that has approximately the same elements as @racket[arr].
The elements may lose precision during the conversion.
}

@;{
FCArray
fcarray-fft
inline-fcarray-map
fcarray-imag-data
fcarray-real-data
fcarray
array->fcarray
fcarray-scale
fcarray-sqr
fcarray-sqrt
fcarray-conjugate
fcarray-magnitude
fcarray-angle
fcarray-exp
fcarray-sin
fcarray-cos
fcarray-tan
fcarray-acos
fcarray+
fcarray*
fcarray-
fcarray/
fcarray-expt
fcarray-real-part
fcarray-imag-part
fcarray-make-rectangular
fcarray-map
fcarray-log
fcarray-asin
fcarray-atan
}

@section{Unsafe Operations}

@;{
unsafe-array-ref
unsafe-array-set!
unsafe-array-proc
unsafe-settable-array-set-proc
unsafe-fcarray 
in-unsafe-array-indexes
make-unsafe-array-set-proc
make-unsafe-array-proc
unsafe-build-array
unsafe-mutable-array
unsafe-flarray
unsafe-array-transform
}

@;{
Don't know whether or how to document these yet:

parallel-array-strict
parallel-array->mutable-array
array/syntax
flat-vector->matrix
}

@(close-eval typed-eval)
