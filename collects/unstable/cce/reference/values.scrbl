#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme unstable/cce/values))

@title[#:style 'quiet #:tag "cce-values"]{Multiple Values}

@defmodule[unstable/cce/values]

This module provides tools for manipulating functions and expressions that
produce multiple values.

@defform[(values->list expr)]{

Produces a list of the values returned by @scheme[expr].

@defexamples[
#:eval (evaluator 'unstable/cce/values)
(values->list (values 1 2 3))
]

}

@defproc[(map2 [f (-> A ... (values B C))] [lst (listof A)] ...)
         (values (listof B) (listof C))]{

Produces a pair of lists of the respective values of @scheme[f] applied to the
elements in @scheme[lst ...] sequentially.

@defexamples[
#:eval (evaluator 'unstable/cce/values)
(map2 (lambda (x) (values (+ x 1) (- x 1))) (list 1 2 3))
]

}

@defproc[(map/values [n natural-number/c]
                     [f (-> A ... (values B_1 ... B_n))]
                     [lst (listof A)]
                     ...)
         (values (listof B_1) ... (listof B_n))]{

Produces lists of the respective values of @scheme[f] applied to the elements in
@scheme[lst ...] sequentially.

@defexamples[
#:eval (evaluator 'unstable/cce/values)
(map/values
 3
 (lambda (x)
   (values (+ x 1) x (- x 1)))
 (list 1 2 3))
]

}

@deftogether[(
@defproc[(foldr/values [f (-> A ... B ... (values B ...))]
                       [vs (list/c B ...)]
                       [lst (listof A)]
                       ...)
         (values B ...)]
@defproc[(foldl/values [f (-> A ... B ... (values B ...))]
                       [vs (list/c B ...)]
                       [lst (listof A)]
                       ...)
         (values B ...)]
)]{

These functions combine the values in the lists @scheme[lst ...] using the
multiple-valued function @scheme[f]; @scheme[foldr/values] traverses the lists
right to left and @scheme[foldl/values] traverses left to right.

@defexamples[
#:eval (evaluator 'unstable/cce/values)
(define (add/cons a b c d)
  (values (+ a c) (cons b d)))
(foldr/values add/cons (list 0 null)
              (list 1 2 3 4) (list 5 6 7 8))
(foldl/values add/cons (list 0 null)
              (list 1 2 3 4) (list 5 6 7 8))
]

}
