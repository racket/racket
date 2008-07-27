#lang scribble/doc

@(require scribble/manual "shared.ss"
          (for-label scheme
                     teachpack/htdp/image
                     teachpack/htdp/world
                     lang/private/imageeq))

@teachpack["matrix"]{Matrix Operations}

The teachpack supports matrices and matrix operations. A matrix is just a
rectangle of 'objects'. It is displayed as an image, just like the images
from @secref["image"]. 

@declare-exporting[teachpack/htdp/matrix]

@deftech{Rectangle} 
A Rectangle (of X) is a non-empty list of lists containing X where all
elements of the list are lists of equal (non-zero) length.

@defproc[(matrix? [o any/c]) boolean?]{
determines whether the given object is a matrix?}

@defproc[(matrix-rows [m matrix?]) natural-number/c]{
determines how many rows this matrix @scheme[m] has} 

@defproc[(matrix-cols [m matrix?]) natural-number/c]{
determines ow many columns this matrix @scheme[m] has} 

@defproc[(rectangle->matrix [r (unsyntax @tech{Rectangle})]) matrix?]{
creates a matrix from the given @tech{Rectangle}}

@defproc[(matrix->rectangle [m matrix?]) (unsyntax @tech{Rectangle})]{
creates a rectangle from this matrix @scheme[m]}

@defproc[(make-matrix [n natural-number/c][m natural-number/c][l (Listof X)]) matrix?]{
creates an @scheme[n] by @scheme[m] matrix from @scheme[l] 

NOTE: make-matrix would consume an optional number of entries, if it were
like make-vector}

@defproc[(build-matrix [n natural-number/c][m natural-number/c]
                       [f (-> natural-number/c natural-number/c any)])
         matrix?]{
creates an @scheme[n] by @scheme[m] matrix by applying @scheme[f] to (0,0),
(0,1), ..., (n-1,m-1)}

@defproc[(matrix-ref [m matrix?][i natural-number/c][j natural-number/c]) any]{
retrieve the item at (@scheme[i],@scheme[j]) in matrix @scheme[m]}

@defproc[(matrix-set [m matrix?][i natural-number/c][j natural-number/c] 
		     [x any/c]) 
         matrix?]{
creates a new matrix with @scheme[x] at (@scheme[i],@scheme[j]) and all
other places the same as in @scheme[m]}

@defproc[(matrix-where? [m matrix?] [pred? (-> any/c boolean?)]) (listof posn?)]{
@scheme[(matrix-where? M P)] produces a list of @scheme[(make-posn i j)]
such that @scheme[(P (matrix-ref M i j))] holds}

@defproc[(matrix-render [m matrix?]) (unsyntax @tech{Rectangle})]{
renders this matrix @scheme[m] as a rectangle of strings}

@defproc[(matrix-minor [m matrix?][i natural-number/c][j natural-number/c]) 
          matrix?]{ 
creates a matrix minor from @scheme[m] at (@scheme[i],@scheme[j])}

@defproc[(matrix-set! [m matrix?][i natural-number/c][j natural-number/c]
		      [x any/c])
         matrix?]{
like @scheme[matrix-set] but uses a destructive update}

