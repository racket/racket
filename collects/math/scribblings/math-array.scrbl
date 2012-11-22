#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label racket/base
                     math plot
                     (only-in typed/racket/base Flonum Real Boolean Any Listof Integer))
          "utils.rkt")

@(define untyped-eval (make-untyped-math-eval))

@title[#:tag "arrays"]{Arrays}
@(author-neil)

@defmodule[math/array]

@section{Introduction}

One of the most common ways to structure data is with an array: a grid of homogeneous,
independent elements, usually consisting of rows and columns. But an array data type
is often absent from functional languages' libraries. This is probably because arrays
are perceived as requiring users to operate on them using destructive updates, write
loops that micromanage array elements, and in general, stray far from the declarative
ideal.

@margin-note{TODO: Cite Haskell array paper}
Normally, they do. However, experience in Python, and more recently Haskell, has shown
that providing the right data types and a rich collection of whole-array operations
allows working effectively with arrays in a functional, declarative style. As a bonus,
doing so opens the possibility of parallelizing nearly every operation.

It requires a change in definition. The new definition is this:

@bold{An @deftech{array} is just a function with a finite, rectangular domain.}

Some arrays are mutable, some are lazy, some are strict, some are sparse, and some
do not even allocate space to store their elements. All are functions that can be
applied to indexes to retrieve elements.

@subsection{Definitions}

The domain of an array is determined by its @deftech{shape}, a vector of numbers that
describes the extent of each dimension.

@examples[#:eval untyped-eval
                 (array-shape (array [0 1 2 3]))
                 (array-shape (array [[0 1] [2 3] [4 5]]))
                 (array-shape (array 0))]

The function represented by the array is called its @deftech{procedure}, which accepts
vectors of indexes and returns elements.

@examples[#:eval untyped-eval
                 (define arr (array [[0 1] [2 3]]))
                 (define proc (array-proc arr))
                 (proc #(1 1))
                 (array-ref arr #(1 1))]

@section{Quick Start}

@section{Array Types}

@section{Array Constructors}

array syntax

(: make-array (All (A) (User-Indexes A -> (Array A))))

(: axis-index-array (User-Indexes Integer -> (Array Index)))

(: index-array (User-Indexes -> (Array Index)))

(: indexes-array (User-Indexes -> (Array Indexes)))

(: diagonal-array (All (A) (Integer Integer A A -> (Array A))))


@section{Pointwise Array Operations}

@section{Array Folds}

@section{Array Transformations}

@section{Mutable Arrays}

mutable-array syntax

@section{Flonum Arrays}

@section{Float-Complex Arrays}

@(close-eval untyped-eval)
