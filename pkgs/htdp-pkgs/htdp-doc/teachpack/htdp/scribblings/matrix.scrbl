#lang scribble/doc

@(require scribble/manual "shared.rkt"
          (for-label scheme teachpack/htdp/matrix htdp/matrix lang/posn))

@teachpack["matrix"]{Matrix Functions}

@;declare-exporting[teachpack/htdp/matrix]
@defmodule[#:require-form beginner-require htdp/matrix]

The experimental teachpack supports matrices and matrix functions. A
matrix is just a rectangle of 'objects'. It is displayed as an image, just
like the images from @secref["image"]. Matrices are images and, indeed,
scenes in the sense of the @secref["world"]. 

@emph{No educational materials involving matrices exist.}

The functions access a matrix in the usual (school-mathematics) manner:
row first, column second. 

The functions aren't tuned for efficiency so don't expect to build
programs that process lots of data. 

@deftech{Rectangle} 
A Rectangle (of X) is a non-empty list of lists containing X where all
elements of the list are lists of equal (non-zero) length.

@defproc[(matrix? [o any/c]) boolean?]{
determines whether the given object is a matrix?}

@defproc[(matrix-rows [m matrix?]) natural-number/c]{
determines how many rows this matrix @racket[m] has} 

@defproc[(matrix-cols [m matrix?]) natural-number/c]{
determines ow many columns this matrix @racket[m] has} 

@defproc[(rectangle->matrix [r (unsyntax @tech{Rectangle})]) matrix?]{
creates a matrix from the given @tech{Rectangle}}

@defproc[(matrix->rectangle [m matrix?]) (unsyntax @tech{Rectangle})]{
creates a rectangle from this matrix @racket[m]}

@defproc[(make-matrix [n natural-number/c][m natural-number/c][l (Listof X)]) matrix?]{
creates an @racket[n] by @racket[m] matrix from @racket[l] 

NOTE: @racket[make-matrix] would consume an optional number of entries, if
it were like @racket[make-vector]}

@defproc[(build-matrix 
  [n natural-number/c][m natural-number/c]
  [f (-> (and/c natural-number/c (</c m)) 
	 (and/c natural-number/c (</c n))
	 any/c)])
 matrix?]{
creates an @racket[n] by @racket[m] matrix by applying @racket[f] to @racket[(0,0)],
@racket[(0,1)], ..., (@racket[(sub1 m),(sub1 n)])}

@defproc[(matrix-ref [m matrix?][i (and/c natural-number/c (</c (matrix-rows m)))][j (and/c natural-number/c (</c (matrix-rows m)))]) any/c]{
retrieve the item at (@racket[i],@racket[j]) in matrix @racket[m]}

@defproc[(matrix-set [m matrix?][i (and/c natural-number/c (</c (matrix-rows m)))][j (and/c natural-number/c (</c (matrix-rows m)))] 
		     [x any/c]) 
         matrix?]{
creates a new matrix with @racket[x] at (@racket[i],@racket[j]) and all
other places the same as in @racket[m]}

@defproc[(matrix-where? [m matrix?] [pred? (-> any/c boolean?)]) (listof posn?)]{
@racket[(matrix-where? M P)] produces a list of @racket[(make-posn i j)]
such that @racket[(P (matrix-ref M i j))] holds}

@defproc[(matrix-render [m matrix?]) (unsyntax @tech{Rectangle})]{
renders this matrix @racket[m] as a rectangle of strings}

@defproc[(matrix-minor [m matrix?][i (and/c natural-number/c (</c (matrix-rows m)))][j (and/c natural-number/c (</c (matrix-rows m)))]) 
          matrix?]{ 
creates a matrix minor from @racket[m] at (@racket[i],@racket[j])}

@;defproc[(matrix-set! [m matrix?][i (and/c natural-number/c (</c (matrix-rows m)))][j (and/c natural-number/c (</c (matrix-rows m)))] [x any/c]) matrix?]{like @racket[matrix-set] but uses a destructive update}

@; -----------------------------------------------------------------------------

@section{Matrix Snip}

@(require (for-label (only-in mrlib/cache-image-snip cache-image-snip%)))

@;defmodule[htdp/matrix]

The @racket[htdp/matrix] teachpack exports the @racket[snip-class] object to
support saving and reading matrix snips. 

@defthing[snip-class (instance/of matrix-snip-class%)]{An object to support 2D matrix rendering.}
