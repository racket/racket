#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label racket/base racket/vector racket/match racket/unsafe/ops racket/string
                     (except-in racket/list permutations) ; FIXME
                     math plot
                     (only-in typed/racket/base
                              ann inst : λ: define: make-predicate -> case-> Nothing
                              Flonum Real Boolean Any Integer Index Natural Exact-Positive-Integer
                              Nonnegative-Real Sequenceof Fixnum Values Number Float-Complex
                              All U List Vector Listof Vectorof Struct FlVector
                              Symbol Output-Port))
          "utils.rkt")

@(define untyped-eval (make-untyped-math-eval))
@interaction-eval[#:eval untyped-eval
                         (require racket/match
                                  racket/vector
                                  racket/string
                                  racket/sequence
                                  racket/list)]

@(define typed-eval (make-math-eval))
@interaction-eval[#:eval typed-eval
                         (require racket/match
                                  racket/vector
                                  racket/string
                                  racket/sequence
                                  racket/list)]

@title[#:tag "matrices" #:style 'toc]{Matrices and Linear Algebra}
@(author-jens-axel)
@(author-neil)

@bold{Performance Warning:} Matrix values are arrays, as exported by @racketmodname[math/array].
The same performance warning applies: operations are currently 25-50 times slower in untyped Racket
than in Typed Racket, due to the overhead of checking higher-order contracts. We are working on it.

For now, if you need speed, use the @racketmodname[typed/racket] language.

@defmodule[math/matrix]

Like all of @racketmodname[math], @racketmodname[math/matrix] is a work in progress.
Most of the basic algorithms are implemented, but some are still in planning.
Possibly the most useful unimplemented algorithms are
@itemlist[@item{LUP decomposition (currently, LU decomposition is implemented, in @racket[matrix-lu])}
          @item{@racket[matrix-solve] for triangular matrices}
          @item{Singular value decomposition (SVD)}
          @item{Eigendecomposition}
          @item{Decomposition-based solvers}
          @item{Pseudoinverse and least-squares solving}]
All of these are planned for the next Racket release, as well as fast flonum-specific matrix
operations and LAPACK integration.

@local-table-of-contents[]


@;{==================================================================================================}


@section[#:tag "matrix:intro"]{Introduction}

From the point of view of the functions in @racketmodname[math/matrix], a @deftech{matrix} is an
@racket[Array] with two axes and at least one entry, or an array for which @racket[matrix?] returns
@racket[#t].

Technically, a matrix's entries may be any type, and some fully polymorphic matrix functions such as
@racket[matrix-row] and @racket[matrix-map] operate on any kind of matrix.
Other functions, such as @racket[matrix+], require their matrix arguments to contain numeric values.

@subsection[#:tag "matrix:function-types"]{Function Types}

The documentation for @racketmodname[math/matrix] functions use the type @racket[Matrix], a synonym
of @racket[Array], when the function either requires that an argument is a @tech{matrix} or ensures
that a return value is a matrix.

Most functions that implement matrix algorithms are documented as accepting @racket[(Matrix Number)]
values. This includes @racket[(Matrix Real)], which is a subtype. Most of these functions have a more
precise type than is documented. For example, @racket[matrix-conjugate] has the type
@racketblock[(case-> ((Matrix Flonum)        -> (Matrix Flonum))
                     ((Matrix Real)          -> (Matrix Real))
                     ((Matrix Float-Complex) -> (Matrix Float-Complex))
                     ((Matrix Number)        -> (Matrix Number)))]
but is documented as having the type @racket[((Matrix Number) -> (Matrix Number))].

Precise function types allow Typed Racket to prove more facts about @racketmodname[math/matrix]
client programs. In particular, it is usually easy for it to prove that operations on real
matrices return real matrices:
@interaction[#:eval typed-eval
                    (matrix-conjugate (matrix [[1 2 3] [4 5 6]]))]
and that operations on inexact matrices return inexact matrices:
@interaction[#:eval typed-eval
                    (matrix-conjugate (matrix [[1.0+2.0i 2.0+3.0i 3.0+4.0i]
                                               [4.0+5.0i 5.0+6.0i 6.0+7.0i]]))]

@subsection[#:tag "matrix:failure"]{Failure Arguments}

In many matrix operations, such as inversion, failure is easy to detect during computation, but
is just as expensive to detect ahead of time as the operation itself.
In these cases, the functions implementing the operations accept an optional @deftech{failure thunk},
or a zero-argument function that returns the result of the operation in case of failure.

For example, the (simplified) type of @racket[matrix-inverse] is
@racketblock[(All (F) (case-> ((Matrix Number)        -> (Matrix Number))
                              ((Matrix Number) (-> F) -> (U F (Matrix Number)))))]
Thus, if a failure thunk is given, the call site is required to check for return values of type
@racket[F] explicitly.

Default failure thunks usually raise an error, and have the type @racket[(-> Nothing)].
For such failure thunks, @racket[(U F (Matrix Number))] is equivalent to @racket[(Matrix Number)],
because @racket[Nothing] is part of every type. (In Racket, any expression may raise an error.)
Thus, in this case, no explicit test for values of type @racket[F] is necessary (though of course they
may be caught using @racket[with-handlers] or similar).

@subsection[#:tag "matrix:broadcasting"]{Broadcasting}

Unlike array operations, pointwise matrix operations @bold{do not} @tech{broadcast} their arguments
when given matrices with different axis lengths:
@interaction[#:eval typed-eval
                    (matrix+ (identity-matrix 2) (matrix [[10]]))]
If you need broadcasting, use array operations:
@interaction[#:eval typed-eval
                    (array+ (identity-matrix 2) (matrix [[10]]))]

@subsection{Strictness}

Functions exported by @racketmodname[math/matrix] return @tech{strict} or @tech{nonstrict} arrays
based on the value of the @racket[array-strictness] parameter.
See @secref{array:nonstrict} for details.


@;{==================================================================================================}


@section[#:tag "matrix:types"]{Types, Predicates and Accessors}

@defform[(Matrix A)]{
Equivalent to @racket[(Array A)], but used for values @racket[M] for which @racket[(matrix? M)] is
@racket[#t].
}

@defproc[(matrix? [arr (Array A)]) Boolean]{
Returns @racket[#t] when @racket[arr] is a @tech{matrix}: a nonempty array with exactly two axes.
@examples[#:eval typed-eval
                 (matrix? (array 10))
                 (matrix? (array #[1 2 3]))
                 (matrix? (make-array #(5 0) 0))
                 (matrix? (array #[#[1 0] #[0 1]]))]
}

@defproc[(row-matrix? [arr (Array A)]) Boolean]{
Returns @racket[#t] when @racket[arr] is a @deftech{row matrix}:
a @tech{matrix} with exactly one row.
}

@defproc[(col-matrix? [arr (Array A)]) Boolean]{
Returns @racket[#t] when @racket[arr] is a @deftech{column matrix}:
a @tech{matrix} with exactly one column.
}

@defproc[(square-matrix? [arr (Array A)]) Boolean]{
Returns @racket[#t] when @racket[arr] is a @tech{matrix} with the same number of rows and columns.
}

@defproc[(matrix-shape [M (Matrix A)]) (Values Index Index)]{
Returns @racket[M]'s row and column count, respectively.
Raises an error if @racket[(matrix? M)] is @racket[#f].
@examples[#:eval typed-eval
                 (matrix-shape (row-matrix [1 2 3]))
                 (matrix-shape (col-matrix [1 2 3]))
                 (matrix-shape (identity-matrix 3))]
}

@defproc[(matrix-num-rows [M (Matrix A)]) Index]{
Returns the number of rows in @racket[M], or the first value of @racket[(matrix-shape M)].
}

@defproc[(matrix-num-cols [M (Matrix A)]) Index]{
Returns the number of columns in @racket[M], or the second value of @racket[(matrix-shape M)].
}

@defproc[(square-matrix-size [M (Matrix A)]) Index]{
Returns the number of rows/columns in @racket[M].
Raises an error if @racket[(square-matrix? M)] is @racket[#f].
@examples[#:eval typed-eval
                 (square-matrix-size (identity-matrix 3))
                 (square-matrix-size (row-matrix [1 2 3]))]
}


@;{==================================================================================================}


@section[#:tag "matrix:construction"]{Construction}

@defform/subs[(matrix [[expr ...+] ...+] maybe-type-ann)
              [(maybe-type-ann (code:line) (code:line : type))]]{
Like the @racket[array] form for creating arrays, but does not require @racket[#[...]] to delimit
nested rows, and the result is constrained to be a @racket[matrix?].
@examples[#:eval typed-eval
                 (matrix [[1 2 3] [4 5 6]])
                 (matrix [[1 2 3] [4 5 6]] : Number)
                 (matrix [[]])]
}

@defform/subs[(row-matrix [expr ...+] maybe-type-ann)
              [(maybe-type-ann (code:line) (code:line : type))]]{
Like @racket[matrix], but returns a @tech{row matrix}.
@examples[#:eval typed-eval
                 (row-matrix [1 2 3])
                 (row-matrix [1 2 3] : Number)
                 (row-matrix [])]
}

@defform/subs[(col-matrix [expr ...+] maybe-type-ann)
              [(maybe-type-ann (code:line) (code:line : type))]]{
Like @racket[matrix], but returns a @tech{column matrix}.
@examples[#:eval typed-eval
                 (col-matrix [1 2 3])
                 (col-matrix [1 2 3] : Number)
                 (col-matrix [])]
}

@defproc[(identity-matrix [n Integer] [one A 1] [zero A 0]) (Matrix A)]{
Returns an @racket[n]×@racket[n] identity matrix, which has the value @racket[one] on the diagonal
and @racket[zero] everywhere else. The height/width @racket[n] must be positive.
@examples[#:eval typed-eval
                 (identity-matrix 3)
                 (identity-matrix 4 1.0+0.0i 0.0+0.0i)]
}

@defproc[(make-matrix [m Integer] [n Integer] [x A]) (Matrix A)]{
Returns an @racket[m]×@racket[n] matrix filled with the value @racket[x];
both @racket[m] and @racket[n] must be positive.
Analogous to @racket[make-array] (and defined in terms of it).
}

@defproc[(build-matrix [m Integer] [n Integer] [proc (Index Index -> A)]) (Matrix A)]{
Returns an @racket[m]×@racket[n] matrix with entries returned by @racket[proc];
both @racket[m] and @racket[n] must be positive.
Analogous to @racket[build-array] (and defined in terms of it).
}

@defproc[(diagonal-matrix [xs (Listof A)] [zero A 0]) (Matrix A)]{
Returns a matrix with @racket[xs] along the diagonal and @racket[zero] everywhere else.
The length of @racket[xs] must be positive.
@examples[#:eval typed-eval
                 (diagonal-matrix '(1 2 3 4 5 6))
                 (diagonal-matrix '(1.0 2.0 3.0 4.0 5.0) 0.0)]
}

@define[block-diagonal-url]{http://en.wikipedia.org/wiki/Block_matrix#Block_diagonal_matrices}

@defproc[(block-diagonal-matrix [Xs (Listof (Matrix A))] [zero A 0]) (Matrix A)]{
@margin-note*{@hyperlink[block-diagonal-url]{Wikipedia: Block-diagonal matrices}}
Returns a matrix with matrices @racket[Xs] along the diagonal and @racket[zero] everywhere else.
The length of @racket[Xs] must be positive.
@examples[#:eval typed-eval
                 (block-diagonal-matrix (list (matrix [[6 7] [8 9]])
                                              (diagonal-matrix '(7 5 7))
                                              (col-matrix [1 2 3])
                                              (row-matrix [4 5 6])))
                 (block-diagonal-matrix (list (make-matrix 2 2 2.0+3.0i)
                                              (make-matrix 2 2 5.0+7.0i))
                                        0.0+0.0i)]
}

@define[vandermonde-url]{http://en.wikipedia.org/wiki/Vandermonde_matrix}

@defproc[(vandermonde-matrix [xs (Listof Number)] [n Integer]) (Matrix Number)]{
@margin-note*{@hyperlink[vandermonde-url]{Wikipedia: Vandermonde matrix}}
Returns an @racket[m]×@racket[n] Vandermonde matrix, where @racket[m = (length xs)].
@examples[#:eval typed-eval
                 (vandermonde-matrix '(1 2 3 4) 5)
                 (vandermonde-matrix '(5.2 3.4 2.0) 3)]
Using a Vandermonde matrix to find a Lagrange polynomial (the polynomial of least degree that passes
through a given set of points):
@interaction[#:eval untyped-eval
                    (define (lagrange-polynomial xs ys)
                      (array->list (matrix-solve (vandermonde-matrix xs (length xs))
                                                 (->col-matrix ys))))
                    (define xs '(-3 0 3))
                    (define ys '(13 3 6))
                    (match-define (list c b a) (lagrange-polynomial xs ys))
                    (plot (list (function (λ (x) (+ c (* b x) (* a x x))) -4 4)
                                (points (map list xs ys))))]
Note that the above example is in untyped Racket.

This function is defined in terms of @racket[array-axis-expand].
}

@deftogether[(@defform[(for/matrix: m n maybe-fill (for:-clause ...) maybe-type-ann
                         body ...+)]
              @defform/subs[(for*/matrix: m n maybe-fill (for:-clause ...) maybe-type-ann
                              body ...+)
                            ([maybe-fill (code:line) (code:line #:fill fill)]
                             [maybe-type-ann (code:line) (code:line : body-type)])
                            #:contracts ([m Integer]
                                         [n Integer]
                                         [fill body-type])])]{
Like @racket[for/array:] and @racket[for*/array:], but for matrices.
The only material difference is that the shape @racket[m n] is required and must be positive.
}

@deftogether[(@defform[(for/matrix m n maybe-fill (for-clause ...)
                         body ...+)]
              @defform[(for*/matrix m n maybe-fill (for-clause ...)
                         body ...+)])]{
Untyped versions of the loop macros.
}


@;{==================================================================================================}


@section[#:tag "matrix:conversion"]{Conversion}

@deftogether[(@defproc[(list->matrix [m Integer] [n Integer] [xs (Listof A)]) (Matrix A)]
              @defproc[(matrix->list [M (Matrix A)]) (Listof A)])]{
Convert a flat list to an @racket[m]×@racket[n] matrix and back;
both @racket[m] and @racket[n] must be positive, and @racket[(* m n) = (length xs)].
The entries in @racket[xs] are in row-major order.
@examples[#:eval typed-eval
                 (list->matrix 2 3 '(1 2 3 4 5 6))
                 (matrix->list (matrix [[1 2] [3 4] [5 6]]))]
}

@deftogether[(@defproc[(vector->matrix [m Integer] [n Integer] [xs (Vectorof A)]) (Matrix A)]
              @defproc[(matrix->vector [M (Matrix A)]) (Vectorof A)])]{
Like @racket[list->matrix] and @racket[matrix->list], but for vectors.
@examples[#:eval typed-eval
                 (vector->matrix 2 3 #(1 2 3 4 5 6))
                 (matrix->vector (matrix [[1 2] [3 4] [5 6]]))]
}

@deftogether[(@defproc[(->row-matrix [xs (U (Listof A) (Vectorof A) (Array A))]) (Matrix A)]
              @defproc[(->col-matrix [xs (U (Listof A) (Vectorof A) (Array A))]) (Matrix A)])]{
Convert a list, vector, or array into a row or column matrix.
If @racket[xs] is an array, it must be nonempty and @bold{not} have more than one axis with length
greater than @racket[1].
@examples[#:eval typed-eval
                 (->row-matrix '(1 2 3))
                 (->row-matrix #(1 2 3))
                 (->row-matrix (col-matrix [1 2 3]))
                 (->col-matrix (array #[#[#[1]] #[#[2]] #[#[3]]]))
                 (->col-matrix (matrix [[1 0] [0 1]]))]
}

@deftogether[(@defproc[(list*->matrix [xss (Listof (Listof A))]) (Matrix A)]
              @defproc[(matrix->list* [M (Matrix A)]) (Listof (Listof A))])]{
Convert a list of lists of entries into a matrix and back.
@examples[#:eval typed-eval
                 (list*->matrix '((1 2 3) (4 5 6)))
                 (matrix->list* (matrix [[1 2 3] [4 5 6]]))]
These functions are like @racket[list*->array] and @racket[array->list*], but use a fixed-depth
(i.e. non-recursive) list type, and do not require a predicate to distinguish entries from rows.
}

@deftogether[(@defproc[(vector*->matrix [xss (Vectorof (Vectorof A))]) (Matrix A)]
              @defproc[(matrix->vector* [M (Matrix A)]) (Vectorof (Vectorof A))])]{
Like @racket[list*->matrix] and @racket[matrix*->list], but for vectors.
@examples[#:eval typed-eval
                 ((inst vector*->matrix Integer) #(#(1 2 3) #(4 5 6)))
                 (matrix->vector* (matrix [[1 2 3] [4 5 6]]))]
As in the first example, Typed Racket often needs help inferring the type @racket[A].
}

                                                                                  
@;{==================================================================================================}


@section[#:tag "matrix:arith"]{Entrywise Operations and Arithmetic}

@deftogether[(@defproc[(matrix+ [M (Matrix Number)] [N (Matrix Number)] ...) (Matrix Number)]
              @defproc[(matrix- [M (Matrix Number)] [N (Matrix Number)] ...) (Matrix Number)]
              @defproc[(matrix* [M (Matrix Number)] [N (Matrix Number)] ...) (Matrix Number)])]{
Matrix addition, subtraction and products respectively.

For matrix addition and subtraction all matrices must have the same shape.

For matrix product the number of columns of one matrix must equal the 
number of rows in the following matrix.

@examples[#:eval untyped-eval
                 (define A (matrix ([1 2] 
                                    [3 4])))
                 (define B (matrix ([5 6] 
                                    [7 8])))
                 (define C (matrix ([ 9 10 11]
                                    [12 13 14])))
                 (matrix+ A B)
                 (matrix- A B)
                 (matrix* A C)]
}

@defproc[(matrix-expt [M (Matrix Number)] [n Integer]) (Matrix Number)]{
Computes @racket[(matrix* M ...)] with @racket[n] arguments, but more efficiently.
@racket[M] must be a @racket[square-matrix?] and @racket[n] must be nonnegative.
@examples[#:eval untyped-eval
                 ; The 100th (and 101th) Fibonacci number:
                 (matrix* (matrix-expt (matrix [[1 1] [1 0]]) 100)
                          (col-matrix [0 1]))
                 (->col-matrix (list (fibonacci 100) (fibonacci 99)))]
}

@defproc[(matrix-scale [M (Matrix Number)] [z Number]) (Matrix Number)]{
Computes the matrix @racket[zM], a matrix of the same shape as @racket[M] 
where each entry in @racket[M] is multiplied with @racket[z].
@examples[#:eval untyped-eval
                 (matrix-scale (matrix [[1 2] [3 4]]) 2)]
}

@defproc*[([(matrix-map [f (A -> R)] [M (Matrix A)]) (Matrix R)]
           [(matrix-map [f (A B Ts ... -> R)] [M0 (Matrix A)] [M1 (Matrix B)] [N (Matrix Ts)]
                        ...)
            (Matrix R)])]{
Like @racket[array-map], but requires at least one matrix argument and never @tech{broadcasts}.
@examples[#:eval untyped-eval
                 (matrix-map sqr (matrix [[1 2] [3 4]]))
                 (matrix-map  +  (matrix [[1 2] [3 4]])
                                 (matrix [[5 6] [7 8]]))]
}

@defproc[(matrix-sum [Ms (Listof (Matrix Number))]) (Matrix Number)]{
Like @racket[(apply matrix+ Ms)], but raises a runtime error when @racket[Ms] is empty.
}

@defproc[(matrix= [M0 (Matrix Number)] [M1 (Matrix Number)] [N (Matrix Number)] ...) Boolean]{
Returns @racket[#t] when its arguments are the same size and are equal entrywise.

See @racket[matrix-relative-error] and @racket[matrix-absolute-error] for equality testing that is
tolerant to floating-point error.
}


@;{==================================================================================================}


@section[#:tag "matrix:poly"]{Polymorphic Operations}

@defproc[(matrix-ref [M (Matrix A)] [i Integer] [j Integer]) A]{
Returns the entry on row @racket[i] and column @racket[j].
@examples[#:eval untyped-eval
                 (define M (matrix ([1 2 3] [4 5 6])))
                 (matrix-ref M 0 2)
                 (matrix-ref M 1 2)]
}

@deftogether[(@defproc[(matrix-row [M (Matrix A)] [i Integer]) (Matrix A)]
              @defproc[(matrix-col [M (Matrix A)] [j Integer]) (Matrix A)])]{
Returns the @racket[i]th row or @racket[j]th column as a matrix.
@examples[#:eval untyped-eval
                 (define M (matrix ([1 2 3] [4 5 6])))
                 (matrix-row M 1)
                 (matrix-col M 0)]
}

@defproc[(submatrix [M (Matrix A)]
                    [is (U Slice (Sequenceof Integer))]
                    [js (U Slice (Sequenceof Integer))])
         (Array A)]{
Returns a submatrix or subarray of @racket[M], where @racket[is] and @racket[js] specify
respectively the rows and columns to keep. Like @racket[array-slice-ref], but constrained so the
result has exactly two axes.
@examples[#:eval typed-eval
                 (submatrix (identity-matrix 5) (:: 1 #f 2) (::))
                 (submatrix (identity-matrix 5) '() '(1 2 4))]
Note that @racket[submatrix] may return an empty array, which is not a @tech{matrix}.
}

@defproc[(matrix-diagonal [M (Matrix A)]) (Array A)]{
Returns array of the entries on the diagonal of @racket[M].
@examples[#:eval untyped-eval
                 (matrix-diagonal
                  (matrix ([1 2 3] [4 5 6] [7 8 9])))]
}

@deftogether[(@defproc[(matrix-upper-triangle [M (Matrix A)] [zero A 0]) (Matrix A)]
              @defproc[(matrix-lower-triangle [M (Matrix A)] [zero A 0]) (Matrix A)])]{
The function @racket[matrix-upper-triangle] returns an upper
triangular matrix (entries below the diagonal have the value @racket[zero]) with
entries from the given matrix. Likewise the function 
@racket[matrix-lower-triangle] returns a lower triangular
matrix.
@examples[#:eval typed-eval
                 (define M (array+ (array 1) (axis-index-array #(5 7) 1)))
                 M
                 (matrix-upper-triangle M)
                 (matrix-lower-triangle M)
                 (matrix-lower-triangle (array->flarray M) 0.0)]
}

@deftogether[(@defproc[(matrix-rows [M (Matrix A)]) (Listof (Matrix A))]
              @defproc[(matrix-cols [M (Matrix A)]) (Listof (Matrix A))])]{
The functions respectively returns a list of the rows or columns
of the matrix.
@examples[#:eval untyped-eval
                 (define M (matrix ([1 2 3] [4 5 6])))
                 (matrix-rows M)
                 (matrix-cols M)]
}

@deftogether[(@defproc[(matrix-augment [Ms (Listof (Matrix A))]) (Matrix A)]
              @defproc[(matrix-stack [Ms (Listof (Matrix A))]) (Matrix A)])]{
The function @racket[matrix-augment] returns a matrix whose columns are
the columns of the matrices in @racket[Ms]. The matrices 
in list must have the same number of rows.

The function @racket[matrix-stack] returns a matrix whose rows are
the rows of the matrices in @racket[Ms]. The matrices 
in list must have the same number of columns.
@examples[#:eval untyped-eval
                 (define M0 (matrix ([1 1] [1 1])))
                 (define M1 (matrix ([2 2] [2 2])))
                 (define M2 (matrix ([3 3] [3 3])))
                 (matrix-augment (list M0 M1 M2))
                 (matrix-stack (list M0 M1 M2))]
}

@deftogether[
(@defproc*[([(matrix-map-rows 
              [f ((Matrix A) -> (Matrix B))] [M (Matrix A)]) (Matrix B)]
            [(matrix-map-rows
              [f ((Matrix A) -> (U #f (Matrix B)))] [M (Matrix A)] [fail (-> F)])
             (U F (Matrix B))])])]{
The two-argument case applies the function @racket[f] to each row of @racket[M].
If the rows are called @racket[r0], @racket[r1], ..., the result matrix has the rows
@racket[(f r0)], @racket[(f r1)], ....

@examples[#:eval untyped-eval
                 (define M (matrix ([1 2 3] [4 5 6] [7 8 9] [10 11 12])))
                 (define (double-row r) (matrix-scale r 2))
                 (matrix-map-rows double-row M)]

In the three argument case, if @racket[f] returns @racket[#f], the result of @racket[(fail)] is
returned:
@interaction[#:eval typed-eval
                    (define Z (make-matrix 4 4 0))
                    Z
                    (matrix-map-rows (λ: ([r : (Matrix Real)])
                                       (matrix-normalize r 2 (λ () #f)))
                                     Z
                                     (λ () 'FAILURE))]
}

@deftogether[
(@defproc*[([(matrix-map-cols 
              [f ((Matrix A) -> (Matrix B))] [M (Matrix A)]) (Matrix B)]
            [(matrix-map-cols
              [f ((Matrix A) -> (U #f (Matrix B)))] [M (Matrix A)] [fail (-> F)])
             (U F (Matrix B))])])]{
Like @racket[matrix-map-rows], but maps @racket[f] over columns.
}


@;{==================================================================================================}


@section[#:tag "matrix:basic"]{Basic Operations}

@defproc[(matrix-conjugate [M (Matrix Number)]) (Matrix Number)]{
Returns a matrix where each entry of the given matrix is conjugated.
@examples[#:eval untyped-eval
                 (matrix-conjugate (matrix ([1 +i] [-1 2+i])))]
}

@deftogether[(@defproc[(matrix-transpose [M (Matrix A)]) (Matrix A)]
              @defproc[(matrix-hermitian [M (Matrix Number)]) (Matrix Number)])]{
@margin-note*{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Transpose"]{Transpose},
                         @hyperlink["http://en.wikipedia.org/wiki/Hermitian_matrix"]{Hermitian}}
Returns the transpose or the hermitian of the matrix. 
The hermitian of a matrix is the conjugate of the transposed matrix.
For a real matrix these operations return the the same result.
@examples[#:eval untyped-eval
                 (matrix-transpose (matrix ([1 1] [2 2] [3 3])))
                 (matrix-hermitian (matrix ([1 +i] [2 +2i] [3 +3i])))]
}

@defproc[(matrix-trace [M (Matrix Number)]) Number]{
@margin-note*{@hyperlink["http://en.wikipedia.org/wiki/Trace_(linear_algebra)"]{Wikipedia: Trace}}
Returns the trace of the square matrix. The trace of matrix is the 
the sum of the diagonal entries. 
@examples[#:eval untyped-eval
                 (matrix-trace (matrix ([1 2] [3 4])))]
}


@;{==================================================================================================}


@section[#:tag "matrix:inner"]{Inner Product Space Operations}

The following functions treat matrices as vectors in an inner product space.
It often makes most sense to use these vector-space functions only for
@tech[#:key "row matrix"]{row matrices} and @tech[#:key "column matrix"]{column matrices}, which are
essentially vectors as we normally think of them.
There are exceptions, however, such as the fact that the Frobenius or Euclidean norm (implemented by
@racket[matrix-2norm]) can be used to measure error between matrices in a way that meets certain
reasonable criteria (specifically, it is submultiplicative).

See @secref{matrix:op-norm} for similar functions (e.g. norms and angles) defined by considering
matrices as operators between inner product spaces consisting of column matrices.

@deftogether[(@defproc[(matrix-1norm [M (Matrix Number)]) Nonnegative-Real]
              @defproc[(matrix-2norm [M (Matrix Number)]) Nonnegative-Real]
              @defproc[(matrix-inf-norm [M (Matrix Number)]) Nonnegative-Real]
              @defproc[(matrix-norm [M (Matrix Number)] [p Real 2]) Nonnegative-Real])]{
@margin-note*{@hyperlink["http://en.wikipedia.org/wiki/Norm_(mathematics)"]{Wikipedia: Norm}}
Respectively compute the L@subscript{1} norm, L@subscript{2} norm, L@subscript{∞}, and
L@subscript{p} norm.

The L@subscript{1} norm is also known under the names Manhattan or taxicab norm.
The L@subscript{1} norm of a matrix is the sum of magnitudes of the entries in the matrix.

The L@subscript{2} norm is also known under the names Euclidean or Frobenius norm.
The L@subscript{2} norm of a matrix is the square root of the sum of squares of 
magnitudes of the entries in the matrix.

The L@subscript{∞} norm is also known as the maximum or infinity norm.
The L@subscript{∞} norm computes the maximum magnitude of the entries in the matrix.

For @racket[p >= 1], @racket[matrix-norm] computes the L@subscript{@racket[p]} norm: 
the @racket[p]th root of the sum of all entry magnitudes to the @racket[p]th power.
@;{MathJax would be nice to have in Scribble...}
@examples[#:eval untyped-eval
                 (matrix-1norm    (col-matrix [1 2]))
                 (matrix-2norm    (col-matrix [1 2]))
                 (matrix-inf-norm (col-matrix [1 2]))
                 (matrix-norm     (col-matrix [1 2]) 3)
                 (matrix-norm     (col-matrix [1 2]) +inf.0)]
}

@defproc*[([(matrix-dot [M (Matrix Number)]) Nonnegative-Real]
           [(matrix-dot [M (Matrix Number)] [N (Matrix Number)]) Number])]{

The call @racket[(matrix-dot M N)] computes the Frobenius inner product of the 
two matrices with the same shape.
In other words the sum of @racket[(* a (conjugate b))] is computed where 
@racket[a] runs over the entries in @racket[M] and @racket[b] runs over
the corresponding entries in @racket[N].

The call @racket[(matrix-dot M)] computes @racket[(matrix-dot M M)] efficiently.
@examples[#:eval untyped-eval
                 (matrix-dot (col-matrix [1 2]) (col-matrix [3 4]))
                 (+ (* 1 3) (* 2 4))]
}

@defproc[(matrix-cos-angle [M (Matrix Number)] [N (Matrix Number)]) Number]{
Returns the cosine of the angle between two matrices w.r.t. the inner produce space induced by
the Frobenius inner product. That is it returns 

@racket[(/ (matrix-dot M N) (* (matrix-2norm M) (matrix-2norm N)))]

@examples[#:eval untyped-eval
                 (define M (col-matrix [1 0]))
                 (define N (col-matrix [0 1]))
                 (matrix-cos-angle M N)
                 (matrix-cos-angle M (matrix+ M N))]
}

@defproc[(matrix-angle [M (Matrix Number)] [N (Matrix Number)]) Number]{
Equivalent to @racket[(acos (matrix-cos-angle M N))].
@examples[#:eval untyped-eval
                 (require (only-in math/base radians->degrees))
                 (define M (col-matrix [1 0]))
                 (define N (col-matrix [0 1]))
                 (radians->degrees (matrix-angle M N))
                 (radians->degrees (matrix-angle M (matrix+ M N)))]
}

@defproc[(matrix-normalize [M (Matrix Number)] [p Real 2] [fail (-> F) (λ () (error ...))])
         (U F (Matrix Number))]{
Normalizes @racket[M] with respect to the L@subscript{@racket[p]} norm.
@examples[#:eval typed-eval
                 (matrix-normalize (col-matrix [1 1]))
                 (matrix-normalize (col-matrix [1 1]) 1)
                 (matrix-normalize (col-matrix [1 1]) +inf.0)]
The result of applying the @tech{failure thunk} @racket[fail] is returned if @racket[M]'s norm is
zero.
}

@deftogether[(@defproc[(matrix-normalize-rows [M (Matrix Number)]
                                              [p Real 2]
                                              [fail (-> F) (λ () (error ...))]) (Matrix Number)]
              @defproc[(matrix-normalize-cols [M (Matrix Number)]
                                              [p Real 2]
                                              [fail (-> F) (λ () (error ...))]) (Matrix Number)])]{
As @racket[matrix-normalize] but each row or column is normalized separately.
The result is a matrix with unit vectors as rows or columns.
@examples[#:eval typed-eval
                 (matrix-normalize-rows (matrix [[1 2] [2 4]]))
                 (matrix-normalize-cols (matrix [[1 2] [2 4]]))]
The result of applying the @tech{failure thunk} @racket[fail] is returned if the norm of any row
or column in @racket[M] is zero.
}

@deftogether[(@defproc[(matrix-rows-orthogonal? [M (Matrix Number)]
                                                [eps Real (* 10 epsilon.0)])
                       Boolean]
              @defproc[(matrix-cols-orthogonal? [M (Matrix Number)]
                                                [eps Real (* 10 epsilon.0)])
                       Boolean])]{
Returns @racket[#t] if the rows or columns of @racket[M] 
are very close of being orthogonal (by default a few epsilons). 
@examples[#:eval untyped-eval
                 (matrix-rows-orthogonal? (matrix [[1 1] [-1 1]]))
                 (matrix-cols-orthogonal? (matrix [[1 1] [-1 1]]))]
}


@;{==================================================================================================}


@section[#:tag "matrix:solve"]{Solving Systems of Equations}

@defproc[(matrix-solve [M (Matrix Number)] [B (Matrix Number)] [fail (-> F) (λ () (error ...))])
         (U F (Matrix Number))]{
Returns the matrix @racket[X] for which @racket[(matrix* M X)] is @racket[B].
@racket[M] and @racket[B] must have the same number of rows.

It is typical for @racket[B] (and thus @racket[X]) to be a @tech{column matrix}, but not required.
If @racket[B] is not a column matrix, @racket[matrix-solve] solves for all the columns in @racket[B]
simultaneously.

@examples[#:eval typed-eval
                 (define M (matrix [[7 5] [3 -2]]))
                 (define B0 (col-matrix [3 22]))
                 (define B1 (col-matrix [19 4]))
                 (matrix-solve M B0)
                 (matrix* M (col-matrix [4 -5]))
                 (matrix-solve M B1)
                 (matrix-cols (matrix-solve M (matrix-augment (list B0 B1))))]

@racket[matrix-solve] does not solve overconstrained or underconstrained systems, meaning that
@racket[M] must be invertible.
If @racket[M] is not invertible, the result of applying the @tech{failure thunk} @racket[fail] is
returned.

@racket[matrix-solve] is implemented using @racket[matrix-gauss-elim] to preserve exactness in its
output, with partial pivoting for greater numerical stability when @racket[M] is not exact.

See @racket[vandermonde-matrix] for an example that uses @racket[matrix-solve] to compute Legendre
polynomials.
}

@define[inverse-url]{http://en.wikipedia.org/wiki/Invertible_matrix}
@defproc[(matrix-inverse [M (Matrix Number)] [fail (-> F) (λ () (error ...))]) (U F (Matrix Number))]{
@margin-note*{@hyperlink[inverse-url]{Wikipedia: Invertible Matrix}}
Returns the inverse of @racket[M] if it exists; otherwise returns the result of applying the
@tech{failure thunk} @racket[fail].
@examples[#:eval typed-eval
                 (matrix-inverse (identity-matrix 3))
                 (matrix-inverse (matrix [[7 5] [3 -2]]))
                 (matrix-inverse (matrix [[1 2] [10 20]]))
                 (matrix-inverse (matrix [[1 2] [10 20]]) (λ () #f))]
}

@defproc[(matrix-invertible? [M (Matrix Number)]) Boolean]{
Returns @racket[#t] when @racket[M] is a @racket[square-matrix?] and @racket[(matrix-determinant M)]
is nonzero.
}

@defproc[(matrix-determinant [M (Matrix Number)]) Number]{
@margin-note*{@hyperlink["http://en.wikipedia.org/wiki/Determinant"]{Wikipedia: Determinant}}
Returns the determinant of @racket[M], which must be a @racket[square-matrix?].
@examples[#:eval typed-eval
                 (matrix-determinant (diagonal-matrix '(1 2 3 4)))
                 (* 1 2 3 4)
                 (matrix-determinant (matrix [[1 2] [10 20]]))
                 (matrix-determinant (col-matrix [1 2]))]
}

                                                                                  
@;{==================================================================================================}


@section[#:tag "matrix:row-alg"]{Row-Based Algorithms}

@define[gauss-url]{http://en.wikipedia.org/wiki/Gaussian_elimination}
@define[gauss-jordan-url]{http://en.wikipedia.org/wiki/Gauss%E2%80%93Jordan_elimination}

@defproc[(matrix-gauss-elim [M (Matrix Number)]
                            [jordan? Any #f]
                            [unitize-pivot? Any #f]
                            [pivoting (U 'first 'partial) 'partial])
         (Values (Matrix Number) (Listof Index))]{
@margin-note*{Wikipedia: @hyperlink[gauss-url]{Gaussian elimination},
                         @hyperlink[gauss-jordan-url]{Gauss-Jordan elimination}}
Implements Gaussian elimination or Gauss-Jordan elimination.

If @racket[jordan?] is true, row operations are done both above and below the pivot.
If @racket[unitize-pivot?] is true, the pivot's row is scaled so that the pivot value is @racket[1].
When both are true, the algorithm is called Gauss-Jordan elimination, and the result matrix is in
@italic{reduced} row echelon form.

If @racket[pivoting] is @racket['first], the first nonzero entry in the current column is used as the
pivot. If @racket[pivoting] is @racket['partial], the largest-magnitude nonzero entry is used, which
improves numerical stability on average when @racket[M] contains inexact entries.

The first return value is the result of Gaussian elimination.

The second return value is a list of indexes of columns that did not have a nonzero pivot.

See @racket[matrix-row-echelon] for examples.
}

@define[row-echelon-url]{http://en.wikipedia.org/wiki/Row_echelon_form}

@defproc[(matrix-row-echelon [M (Matrix Number)]
                             [jordan? Any #f]
                             [unitize-pivot? Any #f]
                             [pivoting (U 'first 'partial) 'partial])
         (Matrix Number)]{
@margin-note*{@hyperlink[row-echelon-url]{Wikipedia: Row echelon form}}
Like @racket[matrix-gauss-elim], but returns only the result of Gaussian elimination.
@examples[#:eval typed-eval
                 (define M (matrix [[2 1 -1] [-3 -1 2] [-2 1 2]]))
                 (matrix-row-echelon M)
                 (matrix-row-echelon M #t)
                 (matrix-identity? (matrix-row-echelon M #t #t))]
The last example shows that @racket[M] is invertible.

Using @racket[matrix-row-echelon] to solve a system of linear equations (without checking for
failure):
@interaction[#:eval typed-eval
                    (define B (col-matrix [8 -11 -3]))
                    (define MB (matrix-augment (list M B)))
                    (matrix-col (matrix-row-echelon MB #t #t) 3)
                    (matrix-solve M B)]

Using @racket[matrix-row-echelon] to invert a matrix (also without checking for failure):
@interaction[#:eval typed-eval
                    (define MI (matrix-augment (list M (identity-matrix 3))))
                    (submatrix (matrix-row-echelon MI  #t #t) (::) (:: 3 #f))
                    (matrix-inverse M)]
}

@define[lu-url]{http://en.wikipedia.org/wiki/LU_decomposition}
@defproc[(matrix-lu [M (Matrix Number)] [fail (-> F) (λ () (error ...))])
         (Values (U F (Matrix Number)) (Matrix Number))]{
@margin-note*{@hyperlink[lu-url]{Wikipedia: LU decomposition}}
Returns the LU decomposition of @racket[M] (which must be a @racket[square-matrix?]) if one exists.
An LU decomposition exists if @racket[M] can be put in row-echelon form without swapping rows.

Because @racket[matrix-lu] returns a @italic{unit} lower-triangular matrix (i.e. a lower-triangular
matrix with only ones on the diagonal), the decomposition is unique if it exists.

@examples[#:eval typed-eval
                 (define-values (L U)
                   (matrix-lu (matrix [[4 3] [6 3]])))
                 (values L U)
                 (matrix* L U)]

If @racket[M] does not have an LU decomposition, the first result is the result of applying the
@tech{failure thunk} @racket[fail], and the second result is the original argument @racket[M]:
@interaction[#:eval typed-eval
                    (matrix-lu (matrix [[0 1] [1 1]]))
                    (matrix-lu (matrix [[0 1] [1 1]]) (λ () #f))]
}

                                                                                  
@;{==================================================================================================}


@section[#:tag "matrix:ortho-alg"]{Orthogonal Algorithms}

@define[gram-schmidt-url]{http://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process}
@define[reortho-pdf]{http://www.cerfacs.fr/algor/reports/2002/TR_PA_02_33.pdf}

@defproc[(matrix-gram-schmidt [M (Matrix Number)] [normalize? Any #f] [start-col Integer 0])
         (Array Number)]{
@margin-note*{@hyperlink[gram-schmidt-url]{Wikipedia: Gram-Schmidt process}}
Returns an array whose columns are orthogonal and span the same subspace as @racket[M]'s columns.
The number of columns in the result is the rank of @racket[M].
If @racket[normalize?] is true, the columns are also normalized.

@examples[#:eval typed-eval
                 (define M 
                   (matrix [[12 -51   4]
                            [ 6 167 -68]
                            [-4  24 -41]]))
                 (matrix-gram-schmidt M)
                 (matrix-gram-schmidt M #t)
                 (matrix-cols-orthogonal? (matrix-gram-schmidt M))
                 (matrix-orthonormal? (matrix-gram-schmidt M #t))]

Examples with rank-deficient matrices:
@interaction[#:eval typed-eval
                    (matrix-gram-schmidt (matrix [[ 1 -2 1]
                                                  [-2  4 9]
                                                  [ 3 -6 7]]))
                    (matrix-gram-schmidt (make-matrix 3 3 0))]

When @racket[start-col] is positive, the Gram-Schmidt process is begun on column @racket[start-col]
(but still using the previous columns to orthogonalize the remaining columns).
This feature is generally not directly useful, but is used in the implementation of
@racket[matrix-basis-extension].

@margin-note*{* @bold{On the round-off error analysis of the Gram-Schmidt algorithm with
                      reorthogonalization.},
              Luc Giraud, Julien Langou and Miroslav Rozloznik. 2002. @hyperlink[reortho-pdf]{(PDF)}}
While Gram-Schmidt with inexact matrices is known to be unstable, using it twice tends to remove
instabilities:*
@interaction[#:eval typed-eval
                    (define M (matrix [[0.70000 0.70711]
                                       [0.70001 0.70711]]))
                    (matrix-orthonormal?
                     (matrix-gram-schmidt M #t))
                    (matrix-orthonormal?
                     (matrix-gram-schmidt (matrix-gram-schmidt M) #t))]
}

@defproc[(matrix-basis-extension [M (Matrix Number)]) (Array Number)]{
Returns additional orthogonal columns which, if augmented with @racket[M], would result in an
orthogonal matrix of full rank. If @racket[M]'s columns are normalized, the result's columns are
normalized.
}

@define[qr-url]{http://en.wikipedia.org/wiki/QR_decomposition}

@defproc*[([(matrix-qr [M (Matrix Number)])  (Values (Matrix Number) (Matrix Number))]
           [(matrix-qr [M (Matrix Number)] [full? Any]) (Values (Matrix Number) (Matrix Number))])]{
@margin-note*{@hyperlink["http://en.wikipedia.org/wiki/QR_decomposition"]{Wikipedia: QR decomposition}}
Computes a QR-decomposition of the matrix @racket[M]. The values returned are
the matrices @racket[Q] and @racket[R]. If @racket[full?] is @racket[#f], then
a reduced decomposition is returned, otherwise a full decomposition is returned.

@margin-note*{An @italic{orthonormal} matrix has columns which are orthoginal, unit vectors.}
The (full) decomposition of a square matrix consists of two matrices: 
a orthogonal matrix @racket[Q] and an upper triangular matrix @racket[R], 
such that @racket[QR = M]. 

For tall non-square matrices @racket[R], the triangular part of the full decomposition,
contains zeros below the diagonal. The reduced decomposition leaves the zeros out.
See the Wikipedia entry on @hyperlink[qr-url]{QR decomposition}
for more details.

@examples[#:eval typed-eval
                 (define M
                   (matrix [[12 -51   4]
                            [ 6 167 -68]
                            [-4  24 -41]]))
                 (define-values (Q R) (matrix-qr M))
                 (values Q R)
                 (matrix= M (matrix* Q R))]

The difference between full and reduced decompositions:
@interaction[#:eval typed-eval
                    (define M
                      (matrix [[12 -51]
                               [ 6 167]
                               [-4  24]]))
                    (define-values (Q1 R1) (matrix-qr M #f))
                    (define-values (Q2 R2) (matrix-qr M #t))
                    (values Q1 R1)
                    (values Q2 R2)
                    (matrix= M (matrix* Q1 R1))
                    (matrix= M (matrix* Q2 R2))]

The decomposition @racket[M = QR] is useful for solving the equation @racket[Mx=v]. 
Since the inverse of Q is simply the transpose of Q, 
  @racket[Mx=v  <=>  QRx=v  <=>  Rx = Q^T v].
And since @racket[R] is upper triangular, the system can be solved by back substitution.

The algorithm used is Gram-Schmidt with reorthogonalization.
}


@;{==================================================================================================}


@define[op-norm-url]{http://en.wikipedia.org/wiki/Matrix_norm#Induced_norm}

@section[#:tag "matrix:op-norm"]{Operator Norms and Comparing Matrices}

@secref{matrix:inner} describes functions that deal with matrices as vectors in
an inner product space. This section describes functions that deal with matrices as
@italic{linear operators}, or as functions from column matrices to column matrices.

@margin-note*{@hyperlink[op-norm-url]{Wikipedia: Induced norm}}
In this setting, a norm is the largest relative change in magnitude an operator (i.e. matrix) can
effect on a column matrix, where ``magnitude'' is defined by a vector norm.
(See the Wikipedia article linked to in the margin for a formal definition.)
Matrix norms that are defined in terms of a vector norm are called @italic{induced norms}, or
@deftech{operator norms}.

@defproc[(matrix-op-1norm [M (Matrix Number)]) Nonnegative-Real]{
The @tech{operator norm} induced by the vector norm @racket[matrix-1norm].

When M is a column matrix, @racket[(matrix-op-1norm M)] is equivalent to @racket[(matrix-1norm M)].
}

@defproc[(matrix-op-2norm [M (Matrix Number)]) Nonnegative-Real]{
The @tech{operator norm} induced by the vector norm @racket[matrix-2norm].
                                             
This function is currently undefined because a required algorithm (singular value decomposition or
eigendecomposition) is not yet implemented in @racketmodname[math/matrix].

When M is a column matrix, @racket[(matrix-op-2norm M)] is equivalent to @racket[(matrix-2norm M)].
}

@defproc[(matrix-op-inf-norm [M (Matrix Number)]) Nonnegative-Real]{
The @tech{operator norm} induced by the vector norm @racket[matrix-inf-norm].

When M is a column matrix, @racket[(matrix-op-inf-norm M)] is equivalent to
@racket[(matrix-inf-norm M)].
}

@defproc[(matrix-basis-cos-angle [M0 (Matrix Number)] [M1 (Matrix Number)]) Number]{
Returns the cosine of the angle between the two subspaces spanned by @racket[M0] and @racket[M1].

This function is currently undefined because a required algorithm (singular value decomposition or
eigendecomposition) is not yet implemented in @racketmodname[math/matrix].

When @racket[M0] and @racket[M1] are column matrices, @racket[(matrix-basis-cos-angle M0 M1)] is
equivalent to @racket[(matrix-cos-angle M0 M1)].
}
         
@defproc[(matrix-basis-angle [M0 (Matrix Number)] [M1 (Matrix Number)]) Number]{
Equivalent to @racket[(acos (matrix-basis-cos-angle M0 M1))].

The function is currently undefined because @racket[matrix-basis-cos-angle] is currently undefined.
}

@defparam[matrix-error-norm norm ((Matrix Number) -> Nonnegative-Real)]{
The norm used by @racket[matrix-relative-error] and @racket[matrix-absolute-error].
The default value is @racket[matrix-op-inf-norm].

Besides being a true norm, @racket[norm] should also be @deftech{submultiplicative}:
@racketblock[(norm (matrix* M0 M1)) <= (* (norm M0) (norm M1))]
This additional triangle-like inequality makes it possible to prove error bounds for formulas that
involve matrix multiplication.

All operator norms (@racket[matrix-op-1norm], @racket[matrix-op-2norm], @racket[matrix-op-inf-norm])
are submultiplicative by definition, as is the Frobenius norm (@racket[matrix-2norm]).
}

@defproc[(matrix-absolute-error [M (Matrix Number)]
                                [R (Matrix Number)]
                                [norm ((Matrix Number) -> Nonnegative-Real) (matrix-error-norm)])
         Nonnegative-Real]{
Basically equivalent to @racket[(norm (matrix- M R))], but handles non-rational flonums like
@racket[+inf.0] and @racket[+nan.0] specially.

See @racket[absolute-error] for the scalar version of this function.
}

@defproc[(matrix-relative-error [M (Matrix Number)]
                                [R (Matrix Number)]
                                [norm ((Matrix Number) -> Nonnegative-Real) (matrix-error-norm)])
         Nonnegative-Real]{
Measures the error in @racket[M] relative to the true matrix @racket[R], under the norm @racket[norm].
Basically equivalent to @racket[(/ (norm (matrix- M R)) (norm R))], but handles non-rational flonums
like @racket[+inf.0] and @racket[+nan.0] specially, as well as the case @racket[(norm R) = 0].

See @racket[relative-error] for the scalar version of this function.
}

@defproc[(matrix-zero? [M (Matrix Number)] [eps Real (* 10 epsilon.0)]) Boolean]{
Returns @racket[#t] when @racket[M] is very close to a zero matrix (by default, within a few
epsilons). Equivalent to
@racketblock[(<= (matrix-absolute-error M (make-matrix m n 0)) eps)]
where @racket[m n] is the shape of @racket[M].
}
                         
@defproc[(matrix-identity? [M (Matrix Number)] [eps Real (* 10 epsilon.0)]) Boolean]{
Returns @racket[#t] when @racket[M] is very close to the identity matrix (by default, within a few
epsilons).
Equivalent to
@racketblock[(and (square-matrix? M)
                  (<= (matrix-relative-error M (identity-matrix (square-matrix-size M)))
                      eps))]
}

@defproc[(matrix-orthonormal? [M (Matrix Number)] [eps Real (* 10 epsilon.0)]) Boolean]{
Returns @racket[#t] when @racket[M] is very close to being orthonormal; that is, when
@racket[(matrix* M (matrix-hermitian M))] is very close to an identity matrix.
Equivalent to
@racketblock[(matrix-identity? (matrix* M (matrix-hermitian M)) eps)]
}

@(close-eval typed-eval)
@(close-eval untyped-eval)
