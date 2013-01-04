#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label racket/base racket/vector racket/match racket/unsafe/ops racket/string
                     racket/list
                     math plot
                     (only-in typed/racket/base
                              ann inst : λ: define: make-predicate ->
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

Documentation for this module is currently under construction.

Intro topics: definitions, case-> types, non-strictness

@local-table-of-contents[]


@;{==================================================================================================}


@section[#:tag "matrix:types"]{Types, Predicates and Accessors}

@defform[(Matrix A)]{
Equivalent to @racket[(Array A)], but used for values @racket[M] for which @racket[(matrix? M)] is
@racket[#t].
}

@defproc[(matrix? [arr (Array A)]) Boolean]{
Returns @racket[#t] when @racket[arr] is a @deftech{matrix}: a nonempty array with exactly two axes.
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

@defproc[(identity-matrix [n Integer]) (Matrix (U 0 1))]{
Returns an @racket[n]×@racket[n] identity matrix; @racket[n] must be positive.
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

@defproc[(diagonal-matrix [xs (Listof A)]) (Matrix (U A 0))]{
Returns a matrix with @racket[xs] along the diagonal and @racket[0] everywhere else.
The length of @racket[xs] must be positive.
}

@margin-note{@hyperlink["http://en.wikipedia.org/wiki/Block_matrix#Block_diagonal_matrices"]{Wikipedia: Block-diagonal matrices}}
@defproc[(block-diagonal-matrix [Xs (Listof (Matrix A))]) (Matrix (U A 0))]{
Returns a matrix with matrices @racket[Xs] along the diagonal and @racket[0] everywhere else.
The length of @racket[Xs] must be positive.
@examples[#:eval typed-eval
                 (block-diagonal-matrix (list (matrix [[6 7] [8 9]])
                                              (diagonal-matrix '(7 5 7))
                                              (col-matrix [1 2 3])
                                              (row-matrix [4 5 6])))]
}

@margin-note{@hyperlink["http://en.wikipedia.org/wiki/Vandermonde_matrix"]{Wikipedia: Vandermonde matrix}}
@defproc[(vandermonde-matrix [xs (Listof Number)] [n Integer]) (Matrix Number)]{
Returns an @racket[m]×@racket[n] Vandermonde matrix, where @racket[m = (length xs)].
@examples[#:eval typed-eval
                 (vandermonde-matrix '(1 2 3 4) 5)]
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
                          (col-matrix [0 1]))]
}

@defproc[(matrix-scale [M (Matrix Number)] [z Number]) (Matrix Number)]{
Computes the matrix @racket[zM], a matrix of the same shape as @racket[M] 
where each entry in @racket[M] is multiplied with @racket[z].
@examples[#:eval untyped-eval
                 (matrix-scale (matrix [[1 2] [3 4]]) 2)]
}

@defproc*[([(matrix-map [f (A -> R)] [arr0 (Matrix A)]) (Matrix R)]
           [(matrix-map [f (A B Ts ... -> R)] [arr0 (Matrix A)] [arr1 (Matrix B)] [arrs (Matrix Ts)]
                        ...)
            (Matrix R)])]{
Like @racket[array-map], but requires at least one array argument and never @tech{broadcasts}.
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
                 (define A (matrix ([1 2 3] [4 5 6])))
                 (matrix-ref A 0 2)
                 (matrix-ref A 1 2)]
}

@defthing[submatrix Procedure]{
@;{ TODO
(: submatrix (All (A) (Matrix A) Slice-Spec Slice-Spec -> (Matrix A)))
(define (submatrix a row-range col-range)
  (array-slice-ref (ensure-matrix 'submatrix a) (list row-range col-range)))
} 
 TODO
}

@deftogether[(@defproc[(matrix-row [M (Matrix A)] [i Integer]) (Matrix A)]
              @defproc[(matrix-col [M (Matrix A)] [j Integer]) (Matrix A)])]{
Returns the given row or column.
@examples[#:eval untyped-eval
                 (define A (matrix ([1 2 3] [4 5 6])))
                 (matrix-row A 1)
                 (matrix-col A 0)]
}

@defproc[(matrix-diagonal [M (Matrix A)]) (Array A)]{
Returns array of the elements on the diagonal of the square matrix.
@examples[#:eval untyped-eval
                 (define A (matrix ([1 2 3] [4 5 6] [7 8 9])))
                 (matrix-diagonal A)]        
}

@deftogether[(@defproc[(matrix-upper-triangle [M (Matrix A)]) (Matrix A)]
              @defproc[(matrix-lower-triangle [M (Matrix A)]) (Matrix A)])]{                                                           
The function @racket[matrix-upper-triangle] returns an upper
triangular matrix (entries below the diagonal are zero) with
elements from the given matrix. Likewise the function 
@racket[matrix-lower-triangle] returns an lower triangular
matrix.
@examples[#:eval untyped-eval
                 (define A (matrix ([1 2 3] [4 5 6] [7 8 9])))
                 (matrix-upper-triangle A)
                 (matrix-lower-triangle A)]
}

@deftogether[(@defproc[(matrix-rows [M (Matrix A)]) (Listof (Matrix A))]
              @defproc[(matrix-cols [M (Matrix A)]) (Listof (Matrix A))])]{
The functions respectively returns a list of the rows or columns
of the matrix.
@examples[#:eval untyped-eval
                 (define A (matrix ([1 2 3] [4 5 6])))
                 (matrix-rows A)
                 (matrix-cols A)]
}

@deftogether[(@defproc[(matrix-augment [Ms (Listof (Matrix A))]) (Matrix A)]
              @defproc[(matrix-stack [Ms (Listof (Matrix A))]) (Matrix A)])]{
The function @racket[matrix-augment] returns a matrix whose columns are
the columns of the matrices in @racket[Ms]. This implies that the matrices 
in list must have the same number of rows.

The function @racket[matrix-stack] returns a matrix whose rows are
the rows of the matrices in @racket[Ms]. This implies that the matrices 
in list must have the same number of columns.
@examples[#:eval untyped-eval
                 (define A (matrix ([1 1] [1 1])))
                 (define B (matrix ([2 2] [2 2])))
                 (define C (matrix ([3 3] [3 3])))
                 (matrix-augment (list A B C))
                 (matrix-stack (list A B C))]
}

@deftogether[
(@defproc[(matrix-map-rows 
           [f ((Matrix A) -> (U #f (Matrix B)))] [M (Matrix A)] [fail (-> F) (λ () #f)]) (Matrix B)]  
 @defproc[(matrix-map-cols 
           [f ((Matrix A) -> (U #f (Matrix B)))] [M (Matrix A)] [fail (-> F) (λ () #f)]) (Matrix B)])]{
In the simple case the function @racket[matrix-map-rows] applies the function @racket[f]
to each row of @racket[M]. If the rows are called @racket[r0], @racket[r1], ... then
the result matrix has the rows @racket[(f r0)], @racket[(f r1)], ... .
In the three argument case, the result of @racket[(fail)] is used, 
if @racket[f] returns @racket[#f].

The function @racket[matrix-map-cols] works likewise but on rows.

@examples[#:eval untyped-eval
                 (define A (matrix ([1 2 3] [4 5 6] [7 8 9] [10 11 12])))
                 (define (double-row r) (matrix-scale r 2))
                 (matrix-map-rows double-row A)]
}


@;{==================================================================================================}


@section[#:tag "matrix:basic"]{Basic Operations}

@defproc[(matrix-conjugate [M (Matrix A)]) (Matrix A)]{
Returns a matrix where each element of the given matrix is conjugated.
@examples[#:eval untyped-eval
                 (matrix-conjugate (matrix ([1 +i] [-1 2+i])))]
}

@margin-note{@hyperlink["http://en.wikipedia.org/wiki/Transpose"]{Wikipedia: Transpose}}
@deftogether[(@defproc[(matrix-transpose [M (Matrix A)]) (Matrix A)]
              @defproc[(matrix-hermitian [M (Matrix A)]) (Matrix A)])]{
@margin-note{@hyperlink["http://en.wikipedia.org/wiki/Hermitian_matrix"]{Wikipedia: Hermitian}}
Returns the transpose or the hermitian of the matrix. 
The hermitian of a matrix is the conjugate of the transposed matrix.
For a real matrix these operations return the the same result.
@examples[#:eval untyped-eval
                 (matrix-transpose (matrix ([1 1] [2 2] [3 3])))
                 (matrix-hermitian (matrix ([1 +i] [2 +2i] [3 +3i])))]
}

@margin-note{@hyperlink["http://en.wikipedia.org/wiki/Trace_(linear_algebra)"]{Wikipedia: Trace}}
@defproc[(matrix-trace [M (Matrix Number)]) (Matrix Number)]{
Returns the trace of the square matrix. The trace of matrix is the 
the sum of the diagonal elements. 
@examples[#:eval untyped-eval
                 (matrix-trace (matrix ([1 2] [3 4])))]
}



@;{==================================================================================================}


@section[#:tag "matrix:inner"]{Inner Product Space Operations}

The set of matrices of a given size forms a vector space.
Since the vector space of @racket[mx1] matrices is isomorphic to
the vector space of vectors of size @racket[m], any inner
product (or norm) induce an inner product (or norm) on vectors.

Put differently, the following innner products and norm, even
though defined on general matrices, work on vectors in the
form of column and row matrices.

See @secref{matrix:op-norm} for similar functions (e.g. norms and angles) defined by considering
matrices as operators between inner product spaces consisting of column matrices.

@margin-note{@hyperlink["http://en.wikipedia.org/wiki/Norm_(mathematics)"]{Wikipedia: Norm}}
@deftogether[(@defproc[(matrix-1norm [M (Matrix Number)]) Number]
              @defproc[(matrix-2norm [M (Matrix Number)]) Number]
              @defproc[(matrix-inf-norm [M (Matrix Number)]) Number]
              @defproc[(matrix-norm [M (Matrix Number)]) Number]
              @defproc[(matrix-norm [M (Matrix Number)] [p Real]) Number])]{
The first three functions compute the L1-norm, the L2-norm, and, the L∞-norm respectively.

The L1-norm is also known under the names Manhattan- or taxicab-norm.
The L1-norm of a matrix is the sum of magnitudes of the entries in the matrix.

The L2-norm is also known under the names Euclidean- or Frobenius-norm. 
The L2-norm of a matrix is the square root of the sum of squares of 
magnitudes of the entries in the matrix.

The L∞-norm is also known as the maximum- or infinity-norm.
The L∞-norm computes the maximum magnitude of the entries in the matrix.

The function @racket[matrix-norm] computes the Lp-norm. 
For a number @racket[p>=1] the @racket[p]th root of the sum
of all entries to the @racket[p]th power. 
@;{MathJax would be nice to have in Scribble...}
If no @racket[p] is given, the 2-norm (Eucledian) is used.
@examples[#:eval untyped-eval
                 (matrix-1norm    (col-matrix [1 2]))
                 (matrix-2norm    (col-matrix [1 2]))
                 (matrix-inf-norm (col-matrix [1 2]))
                 (matrix-norm     (col-matrix [1 2]) 3)]
}

@deftogether[(@defproc[(matrix-dot [M (Matrix Number)]) Nonnegative-Real]
              @defproc[(matrix-dot [M1 (Matrix Number)] [M2 (Matrix Number)]) Number])]{

The call @racket[(matrix-dot M1 M2)] computes the Frobenius inner product of the 
two matrices with the same shape.
In other words the sum of @racket[(* a (conjugate b))] is computed where 
@racket[a] runs over the entries in @racket[M1] and @racket[b] runs over
the corresponding entries in @racket[M2].

The call @racket[(matrix-dot M)] computes @racket[(matrix-dot M M)] efficiently.
@examples[#:eval untyped-eval
                 (matrix-dot (col-matrix [1 2]) (col-matrix [3 4]))
                 (+ (* 1 3) (* 2 4))]
}

@defproc[(matrix-cos-angle [M (Matrix Number)]) Number]{
Returns the cosine of the angle between two matrices w.r.t. the inner produce space induced by
the Frobenius inner product. That is it returns 

@racket[(/ (matrix-dot M N) (* (matrix-2norm M) (matrix-2norm N)))]

@examples[#:eval untyped-eval
                 (define e1 (col-matrix [1 0]))
                 (define e2 (col-matrix [0 1]))
                 (matrix-cos-angle e1 e2)
                 (matrix-cos-angle e1 (matrix+ e1 e2))]
}

@defproc[(matrix-angle [M1 (Matrix Number)] [M2 (Matrix Number)]) Number]{
Equivalent to @racket[(acos (matrix-cos-angle M0 M1))].
@examples[#:eval untyped-eval
                 (require racket/math) ; for radians->degrees
                 (define e1 (col-matrix [1 0]))
                 (define e2 (col-matrix [0 1]))
                 (radians->degrees (matrix-angle e1 e2))
                 (radians->degrees (matrix-angle e1 (matrix+ e1 e2)))]
}

@defproc[(matrix-normalize [M (Matrix Number)] [p Real 2] [fail (-> A) raise-argument-error]) (U (Matrix Number) A)]{
To normalize a matrix is to scale it, such that the result has norm 1.
                                                                                                              
The call @racket[(matrix-normalize M p fail)] normalizes @racket[M] with respect to 
the @racket[Lp]-norm. If the matrix @racket[M] has norm 0, the result of calling 
the thunk @racket[fail] is returned.

If no fail-thunk is given, an argument-error exception is raised.
If no @racket[p] the L2-norm (Euclidean norm) is used.
@examples[#:eval untyped-eval
                 (require racket/math) ; for radians->degrees
                 (matrix-normalize (col-matrix [1 1]))
                 (matrix-normalize (col-matrix [1 1]) 1)
                 (matrix-normalize (col-matrix [1 1]) +inf.0)]
}

@deftogether[(@defproc[(matrix-normalize-rows [M (matrix Number)] [p Real 2] [fail (-> A) raise-argument-error]) (Matrix Number)]
              @defproc[(matrix-normalize-cols [M (matrix Number)] [p Real 2] [fail (-> A) raise-argument-error]) (Matrix Number)])]{
As @racket[matrix-normalize] but each row or column is normalized separately.
The result is this a matrix with unit vectors as rows or columns.
@examples[#:eval untyped-eval
                 (require racket/math) ; for radians->degrees
                 (matrix-normalize-rows (matrix [[1 2] [2 4]]))
                 (matrix-normalize-cols (matrix [[1 2] [2 4]]))]
}
                                                                                                             
@deftogether[(@defproc[(matrix-rows-orthogonal? [M (Matrix Number)] [eps Real (* 10 epsilon.0)]) Boolean]
              @defproc[(matrix-cols-orthogonal? [M (Matrix Number)] [eps Real (* 10 epsilon.0)]) Boolean])]{
Returns @racket[#t] if the rows or columns of @racket[M] 
are very close of being orthogonal (by default a few epsilons). 
@examples[#:eval untyped-eval
                 (require racket/math) ; for radians->degrees
                 (matrix-rows-orthogonal? (matrix [[1 1] [-1 1]]))
                 (matrix-cols-orthogonal? (matrix [[1 1] [-1 1]]))]
}



@;{==================================================================================================}


@section[#:tag "matrix:solve"]{Solving Systems of Equations}

@defthing[matrix-solve Procedure]{}

@defthing[matrix-inverse Procedure]{}

@defthing[matrix-invertible? Procedure]{}

@defthing[matrix-determinant Procedure]{}

                                                                                  
@;{==================================================================================================}


@section[#:tag "matrix:row-alg"]{Row-Based Algorithms}

@defthing[matrix-gauss-elim Procedure]{}

@defthing[matrix-row-echelon Procedure]{}

@defthing[matrix-lu Procedure]{}

                                                                                  
@;{==================================================================================================}


@section[#:tag "matrix:ortho-alg"]{Orthogonal Algorithms}

@defthing[matrix-gram-schmidt Procedure]{}

@defthing[matrix-basis-extension Procedure]{}

@margin-note{@hyperlink["http://en.wikipedia.org/wiki/QR_decomposition"]{Wikipedia: QR decomposition}}
@deftogether[(@defproc[(matrix-qr [M (Matrix Real)])   (Values (Matrix Real) (Matrix Real))]
              @defproc[(matrix-qr [M (Matrix Real)] [full Any]) (Values (Matrix Real) (Matrix Real))] 
              @defproc[(matrix-qr [M (Matrix Number)])  (Values (Matrix Number) (Matrix Number))]
              @defproc[(matrix-qr [M (Matrix Number)] [full Any]) (Values (Matrix Number) (Matrix Number))])]{
Computes a QR-decomposition of the matrix @racket[M]. The values returned are
the matrices @racket[Q] and @racket[R]. If @racket[full] is false, then
a reduced decomposition is returned, otherwise a full decomposition is returned.

@margin-note{An @italic{orthonormal} matrix has columns which are orthooginal, unit vectors.}
The (full) decomposition of a square matrix consists of two matrices: 
a orthogonal matrix @racket[Q] and an upper triangular matrix @racket[R], 
such that @racket[QR = M]. 

For tall non-square matrices @racket[R], the triangular part of the full decomposition,
contains zeros below the diagonal. The reduced decomposition leaves the zeros out.
See the Wikipedia entry on @hyperlink["http://en.wikipedia.org/wiki/QR_decomposition"]{QR decomposition}
for more details.

The decomposition @racket[M = QR] is useful for solving the equation @racket[Mx=v]. 
Since the inverse of Q is simply the transpose of Q, 
  @racket[Mx=v  <=>  QRx=v  <=>  Rx = Q^T v].
And since @racket[R] is upper triangular, the system can be solved by back substitution.

The algorithm used is Gram-Schmidt with reorthogonalization.
See the paper @hyperlink["http://www.cerfacs.fr/algor/reports/2002/TR_PA_02_33.pdf"]{On the round-off error analysis of the Gram-Schmidt algorithm with reorthogonalization.}
by Luc Giraud, Julien Langou, and, Miroslav Rozloznik.
}                                                                                  
@;{==================================================================================================}


@section[#:tag "matrix:op-norm"]{Operator Norms and Comparing Matrices}

@defproc[(matrix-op-1norm [M (Matrix Number)]) Nonnegative-Real]{
TODO: describe

When M is a column matrix, @racket[(matrix-op-1norm M)] is equivalent to @racket[(matrix-1norm M)].
}

@defproc[(matrix-op-2norm [M (Matrix Number)]) Nonnegative-Real]{
TODO: describe (spectral norm)

When M is a column matrix, @racket[(matrix-op-2norm M)] is equivalent to @racket[(matrix-2norm M)].
}

@defproc[(matrix-op-inf-norm [M (Matrix Number)]) Nonnegative-Real]{
TODO: describe

When M is a column matrix, @racket[(matrix-op-inf-norm M)] is equivalent to
@racket[(matrix-inf-norm M)].
}

@defproc[(matrix-basis-cos-angle [M0 (Matrix Number)] [M1 (Matrix Number)]) Number]{
Returns the cosine of the angle between the two subspaces spanned by @racket[M0] and @racket[M1].

When @racket[M0] and @racket[M1] are column matrices, @racket[(matrix-basis-cos-angle M0 M1)] is
equivalent to @racket[(matrix-cos-angle M0 M1)].
}
         
@defproc[(matrix-basis-angle [M0 (Matrix Number)] [M1 (Matrix Number)]) Number]{
Equivalent to @racket[(acos (matrix-basis-cos-angle M0 M1))].
}

@defparam[matrix-error-norm norm ((Matrix Number) -> Nonnegative-Real)]{
The norm used by @racket[matrix-relative-error] and @racket[matrix-absolute-error].

Besides being a true norm, @racket[norm] should also be @deftech{submultiplicative}:
@racketblock[(norm (matrix* M0 M1)) <= (* (norm A) (norm B))]
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
In fact, @racket[(matrix-orthonormal? M eps)] is equivalent to
@racketblock[(matrix-identity? (matrix* M (matrix-hermitian M)) eps)]
}

@(close-eval typed-eval)
@(close-eval untyped-eval)
