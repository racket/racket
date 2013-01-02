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

TODO: fill in empty docs

@defproc*[([(matrix-map [f (A -> R)] [arr0 (Matrix A)]) (Matrix R)]
           [(matrix-map [f (A B Ts ... -> R)] [arr0 (Matrix A)] [arr1 (Matrix B)] [arrs (Matrix Ts)]
                        ...)
            (Matrix R)])]{
Like @racket[array-map], but requires at least one array argument and never @tech{broadcasts}.

TODO: more
}

@deftogether[(@defproc[(matrix* [M (Matrix Number)] [N (Matrix Number)] ...) (Matrix Number)]
              @defproc[(matrix+ [M (Matrix Number)] [N (Matrix Number)] ...) (Matrix Number)]
              @defproc[(matrix- [M (Matrix Number)] [N (Matrix Number)] ...) (Matrix Number)])]{
}

@defproc[(matrix-expt [M (Matrix Number)] [n Integer]) (Matrix Number)]{
Computes @racket[(matrix* M ...)] with @racket[n] arguments, but more efficiently.
@racket[M] must be a @racket[square-matrix?] and @racket[n] must be nonnegative.
}

@defproc[(matrix-scale [M (Matrix Number)] [x Number]) (Matrix Number)]{
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

@defthing[matrix-ref Procedure]{
@;{(: matrix-ref (All (A) (Matrix A) Integer Integer -> A))}
}

@defthing[submatrix Procedure]{
@;{
(: submatrix (All (A) (Matrix A) Slice-Spec Slice-Spec -> (Matrix A)))
(define (submatrix a row-range col-range)
  (array-slice-ref (ensure-matrix 'submatrix a) (list row-range col-range)))
}
}

@deftogether[(@defthing[matrix-row Procedure]
              @defthing[matrix-col Procedure])]{
@;{
(: matrix-row (All (A) (Matrix A) Integer -> (Matrix A)))
(: matrix-col (All (A) (Matrix A) Integer -> (Matrix A)))
}
}

@defthing[matrix-diagonal Procedure]{
@;{(: matrix-diagonal (All (A) ((Matrix A) -> (Array A))))}
}

@deftogether[(@defthing[matrix-upper-triangle Procedure]
              @defthing[matrix-lower-triangle Procedure])]{
@;{
(: matrix-upper-triangle (All (A) ((Matrix A) -> (Matrix (U A 0)))))
(: matrix-lower-triangle (All (A) ((Matrix A) -> (Matrix (U A 0)))))
}
}

@deftogether[(@defthing[matrix-rows Procedure]
              @defthing[matrix-cols Procedure])]{
@;{
(: matrix-rows (All (A) (Matrix A) -> (Listof (Matrix A))))
(: matrix-cols (All (A) (Matrix A) -> (Listof (Matrix A))))
}
}

@deftogether[(@defthing[matrix-augment Procedure]
              @defthing[matrix-stack Procedure])]{
@;{
(: matrix-augment (All (A) (Listof (Matrix A)) -> (Matrix A)))
(: matrix-stack (All (A) (Listof (Matrix A)) -> (Matrix A)))
}
}

@deftogether[(@defthing[matrix-map-rows Procedure]
              @defthing[matrix-map-cols Procedure])]{
@;{
(: matrix-map-rows
   (All (A B F) (case-> (((Matrix A) -> (Matrix B)) (Matrix A)        -> (Matrix B))
                        (((Matrix A) -> (U #f (Matrix B))) (Matrix A) (-> F)
                                                           -> (U F (Matrix B))))))

(: matrix-map-cols
   (All (A B F) (case-> (((Matrix A) -> (Matrix B)) (Matrix A)        -> (Matrix B))
                        (((Matrix A) -> (U #f (Matrix B))) (Matrix A) (-> F)
                                                           -> (U F (Matrix B))))))
}
}


@;{==================================================================================================}


@section[#:tag "matrix:basic"]{Basic Operations}

@defthing[matrix-conjugate Procedure]{
@;{(: matrix-conjugate ((Matrix Number) -> (Matrix Number)))}
}

@deftogether[(@defthing[matrix-transpose Procedure]
              @defthing[matrix-hermitian Procedure])]{
@;{
(: matrix-transpose (All (A) (Matrix A) -> (Matrix A)))
(: matrix-hermitian ((Matrix Number) -> (Matrix Number)))
}
}

@defthing[matrix-trace Procedure]{
@;{(: matrix-trace ((Matrix Number) -> Number))}
}


@;{==================================================================================================}


@section[#:tag "matrix:inner"]{Inner Product Space Operations}

TODO: explain: operations on the inner product space of matrices

TODO: explain: probably most useful to use these functions on row and column matrices

See @secref{matrix:op-norm} for similar functions (e.g. norms and angles) defined by considering
matrices as operators between inner product spaces consisting of column matrices.

@deftogether[(@defthing[matrix-1norm Procedure]
              @defthing[matrix-2norm Procedure]
              @defthing[matrix-inf-norm Procedure]
              @defthing[matrix-norm Procedure])]{
@;{
(: matrix-1norm ((Matrix Number) -> Nonnegative-Real))
;; Manhattan, taxicab, or sum norm

(: matrix-2norm ((Matrix Number) -> Nonnegative-Real))
;; Frobenius, or Euclidean norm

(: matrix-inf-norm ((Matrix Number) -> Nonnegative-Real))
;; Maximum, or infinity norm

(: matrix-norm (case-> ((Matrix Number) -> Nonnegative-Real)
                       ((Matrix Number) Real -> Nonnegative-Real)))
;; Any p norm
}
}

@defthing[matrix-dot Procedure]{
@;{
(: matrix-dot (case-> ((Matrix Number) -> Nonnegative-Real)
                      ((Matrix Number) (Matrix Number) -> Number)))
;; Computes the Frobenius inner product of a matrix with itself or of two matrices
}
}

@defthing[matrix-cos-angle Procedure]{
@;{
(: matrix-cos-angle ((Matrix Number) (Matrix Number) -> Number))
;; Returns the cosine of the angle between two matrices w.r.t. the inner produce space induced by
;; the Frobenius inner product
}
}

@defthing[matrix-angle Procedure]{
Equivalent to @racket[(acos (matrix-cos-angle M0 M1))].
}

@defthing[matrix-normalize Procedure]{
@;{
(: matrix-normalize
   (All (A) (case-> ((Matrix Number)             -> (Matrix Number))
                    ((Matrix Number) Real        -> (Matrix Number))
                    ((Matrix Number) Real (-> A) -> (U A (Matrix Number))))))
}
}

@deftogether[(@defthing[matrix-normalize-rows Procedure]
              @defthing[matrix-normalize-cols Procedure])]{
@;{
(: matrix-normalize-rows
   (All (A) (case-> ((Matrix Number)             -> (Matrix Number))
                    ((Matrix Number) Real        -> (Matrix Number))
                    ((Matrix Number) Real (-> A) -> (U A (Matrix Number))))))

(: matrix-normalize-cols
   (All (A) (case-> ((Matrix Number)             -> (Matrix Number))
                    ((Matrix Number) Real        -> (Matrix Number))
                    ((Matrix Number) Real (-> A) -> (U A (Matrix Number))))))
}
}

@deftogether[(@defthing[matrix-rows-orthogonal? Procedure]
              @defthing[matrix-cols-orthogonal? Procedure])]{
@;{
(: matrix-rows-orthogonal? ((Matrix Number) [Real (* 10 epsilon.0)] -> Boolean))
(: matrix-cols-orthogonal? ((Matrix Number) [Real (* 10 epsilon.0)] -> Boolean))
}
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

@defthing[matrix-qr Procedure]{}

                                                                                  
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
